test_that("report_route validates inputs correctly", {
  skip_on_cran()

  # Check file path validation
  expect_snapshot(
    report_route("/report", "nonexistent_file.Rmd"),
    error = TRUE
  )

  # Check max_age validation
  expect_snapshot(
    report_route("/report", "fixtures/reports/test.Rmd", max_age = "invalid"),
    error = TRUE
  )

  # Check continue validation
  expect_snapshot(
    report_route("/report", "fixtures/reports/test.Rmd", continue = "invalid"),
    error = TRUE
  )

  # Check finalize validation
  expect_snapshot(
    report_route(
      "/report",
      "fixtures/reports/test.Rmd",
      finalize = "not_function"
    ),
    error = TRUE
  )

  # Check finalize must take ... argument
  expect_snapshot(
    report_route(
      "/report",
      "fixtures/reports/test.Rmd",
      finalize = function(req, res) {}
    ),
    error = TRUE
  )

  # Check async validation
  expect_snapshot(
    report_route("/report", "fixtures/reports/test.Rmd", async = "invalid"),
    error = TRUE
  )
})

test_that("report_info detects file format correctly", {
  skip_on_cran()

  skip_if_not_installed("rmarkdown")

  # Test with .Rmd file
  rmd_file <- "fixtures/reports/test.Rmd"
  info <- report_info(rmd_file)
  expect_type(info, "list")
  expect_named(info, c("formats", "mime_types", "ext", "query_params", "title"))
  expect_true(any(grepl("text/html", info$mime_types)))
  expect_true(any(grepl("application/pdf", info$mime_types)))
  expect_setequal(names(info$query_params), c("param1", "param2"))

  # Test with .qmd file if quarto is available
  skip_if_not_installed("quarto")
  qmd_file <- "fixtures/reports/test.qmd"
  info <- report_info(qmd_file)
  expect_type(info, "list")
  expect_named(info, c("formats", "mime_types", "ext", "query_params", "title"))
  expect_true(any(grepl("text/html", info$mime_types)))
  expect_true(any(grepl("application/pdf", info$mime_types)))
  expect_setequal(names(info$query_params), c("param1", "param2"))
})

test_that("register_report_format works", {
  skip_on_cran()

  # Test registering a new format
  format_name <- "test_format"
  register_report_format(format_name, "text/test", "test")

  # Check it was registered
  formats <- show_report_formats()
  expect_true(format_name %in% formats$format)
  expect_equal(formats$mime_type[formats$format == format_name], "text/test")
  expect_equal(formats$extension[formats$format == format_name], "test")

  # Test force parameter
  expect_snapshot(
    register_report_format(format_name, "text/new", "new", force = FALSE),
    error = TRUE
  )

  # Test force overwrite
  register_report_format(format_name, "text/new", "new", force = TRUE)
  formats <- show_report_formats()
  expect_equal(formats$mime_type[formats$format == format_name], "text/new")
})

test_that("register_report_format auto-detects extension", {
  skip_on_cran()

  # Test with known mime type
  format_name <- paste0("test_format_html_", as.integer(Sys.time()))
  register_report_format(format_name, "text/html")

  formats <- show_report_formats()
  expect_equal(formats$extension[formats$format == format_name], "html")

  # Test with unknown mime type
  expect_snapshot(
    register_report_format("unknown_format", "application/x-unknown"),
    error = TRUE
  )
})

test_that("show_report_formats returns a data frame", {
  skip_on_cran()

  formats <- show_report_formats()
  expect_s3_class(formats, "data.frame")
  expect_named(formats, c("format", "mime_type", "extension"))

  # Check some standard formats are included
  expect_true("html" %in% formats$format)
  expect_true("pdf" %in% formats$format)
  expect_true("docx" %in% formats$format)
})

test_that("quarto_info extracts report information correctly", {
  skip_on_cran()

  skip_if_not_installed("quarto")

  # Test with actual qmd file
  qmd_file <- "fixtures/reports/test.qmd"
  info <- quarto_info(qmd_file)
  expect_type(info, "list")
  expect_named(info, c("params", "formats", "title"))
  expect_type(info$params, "list")
  expect_type(info$formats, "character")

  # Check extracted parameters
  expect_setequal(names(info$params), c("param1", "param2"))

  # Check extracted formats
  expect_true("html" %in% info$formats)
  expect_true("pdf" %in% info$formats)

  # Same for python reports
  qmd_file <- "fixtures/reports/python.qmd"
  info <- quarto_info(qmd_file)
  expect_type(info, "list")
  expect_named(info, c("params", "formats", "title"))
  expect_type(info$params, "list")
  expect_type(info$formats, "character")

  # Check extracted parameters
  expect_setequal(names(info$params), c("param1", "param2"))
})

test_that("rmarkdown_info extracts report information correctly", {
  skip_on_cran()

  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")

  # Test with actual Rmd file
  rmd_file <- "fixtures/reports/test.Rmd"
  info <- rmarkdown_info(rmd_file)
  expect_type(info, "list")
  expect_named(info, c("params", "formats", "title"))

  # Check extracted parameters
  expect_setequal(names(info$params), c("param1", "param2"))

  # Check extracted formats
  expect_true("html_document" %in% info$formats)
  expect_true("pdf_document" %in% info$formats)
})

test_that("report_route creates correct route", {
  skip_on_cran()

  skip_if_not_installed("rmarkdown")

  # Use the test Rmd file
  rmd_file <- "fixtures/reports/test.Rmd"

  # Create route with async=FALSE to avoid dependencies on mirai
  route <- report_route("/report", rmd_file, async = FALSE)
  expect_s3_class(route, "Route")

  # Test that the main handler exists
  expect_false(is.null(route$get_handler("get", "/report")))

  # Test that the format-specific handlers exist
  expect_false(is.null(route$get_handler("get", "/report.html")))
  expect_false(is.null(route$get_handler("get", "/report.pdf")))
})

test_that("report_route content negotiation redirects correctly", {
  skip_on_cran()

  skip_if_not_installed("rmarkdown")

  # Use the test Rmd file
  rmd_file <- "fixtures/reports/test.Rmd"
  route <- report_route("/report", rmd_file)

  # Test HTML content negotiation
  rook_html <- fiery::fake_request('www.example.com/report', 'get')
  rook_html$HTTP_ACCEPT <- "text/html"
  req_html <- reqres::Request$new(rook_html)
  res_html <- req_html$respond()

  # Dispatch the request
  result_html <- route$dispatch(req_html)

  # Should redirect to the HTML version
  expect_false(result_html)
  expect_equal(res_html$status, 307L)
  expect_equal(res_html$get_header("Location"), "report/html_document")

  # Test PDF content negotiation
  rook_pdf <- fiery::fake_request('www.example.com/report', 'get')
  rook_pdf$HTTP_ACCEPT <- "application/pdf"
  req_pdf <- reqres::Request$new(rook_pdf)
  res_pdf <- req_pdf$respond()

  # Dispatch the request
  result_pdf <- route$dispatch(req_pdf)

  # Should redirect to the PDF version
  expect_false(result_pdf)
  expect_equal(res_pdf$status, 307L)
  expect_equal(res_pdf$get_header("Location"), "report/pdf_document")
})

test_that("report_route handles query parameters correctly", {
  skip_on_cran()

  skip_if_not_installed("rmarkdown")

  # Use the test Rmd file
  rmd_file <- "fixtures/reports/test.Rmd"
  route <- report_route("/report", rmd_file)

  # Test with query parameters
  rook <- fiery::fake_request(
    'www.example.com/report?param1=custom&param2=100',
    'get'
  )
  rook$HTTP_ACCEPT <- "text/html"
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should redirect with query parameters intact
  expect_false(result)
  expect_equal(res$status, 307L)
  expect_equal(
    res$get_header("Location"),
    "report/html_document?param1=custom&param2=100"
  )
})

test_that("report_route works with trailing slash in path", {
  skip_on_cran()

  skip_if_not_installed("rmarkdown")

  # Use the test Rmd file
  rmd_file <- "fixtures/reports/test.Rmd"
  route <- report_route("/report/", rmd_file)

  # Test with trailing slash
  rook <- fiery::fake_request('www.example.com/report/', 'get')
  rook$HTTP_ACCEPT <- "text/html"
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should redirect correctly
  expect_false(result)
  expect_equal(res$status, 307L)
  expect_equal(res$get_header("Location"), "html_document")
})
