test_that('asset_route function returns AssetRoute object', {
  route <- asset_route("/assets", "fixtures/test_files")
  expect_s3_class(route, "AssetRoute")
  expect_equal(route$at, "/assets")
  expect_equal(route$path, "fixtures/test_files")
})

test_that('AssetRoute validates path exists', {
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "non/existent/path"),
    error = TRUE
  )
})

test_that('AssetRoute validates input parameters', {
  # Test at parameter validation
  expect_snapshot(
    AssetRoute$new(at = 123, path = "fixtures/test_files"),
    error = TRUE
  )

  # Test use_index parameter validation
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "fixtures/test_files", use_index = "yes"),
    error = TRUE
  )

  # Test fallthrough parameter validation
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "fixtures/test_files", fallthrough = "yes"),
    error = TRUE
  )

  # Test html_charset parameter validation
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "fixtures/test_files", html_charset = 123),
    error = TRUE
  )

  # Test headers parameter validation
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "fixtures/test_files", headers = list("Content-Type")),
    error = TRUE
  )

  # Test validation parameter validation
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "fixtures/test_files", validation = 123),
    error = TRUE
  )

  # Test except parameter validation
  expect_snapshot(
    AssetRoute$new(at = "/assets", path = "fixtures/test_files", except = 123),
    error = TRUE
  )
})

test_that('AssetRoute accessors work', {
  route <- asset_route("/assets", "fixtures/test_files")

  # Test getters
  expect_equal(route$at, "/assets")
  expect_equal(route$path, "fixtures/test_files")
  expect_true(route$use_index)
  expect_false(route$fallthrough)
  expect_equal(route$html_charset, "utf-8")
  expect_equal(route$headers, list())
  expect_null(route$validation)
  expect_equal(route$except, character(0))
  expect_equal(route$name, "asset_routr")

  # Test setters
  route$at <- "/static"
  expect_equal(route$at, "/static")

  route$path <- "fixtures"
  expect_equal(route$path, "fixtures")

  route$use_index <- FALSE
  expect_false(route$use_index)

  route$fallthrough <- TRUE
  expect_true(route$fallthrough)

  route$html_charset <- "UTF-16"
  expect_equal(route$html_charset, "UTF-16")

  route$headers <- list("X-Test" = "Value")
  expect_equal(route$headers, list("X-Test" = "Value"))

  route$validation <- "test-validation"
  expect_equal(route$validation, "test-validation")

  route$except <- c("/admin")
  expect_equal(route$except, c("/admin"))
})

test_that('AssetRoute path setter validates path exists', {
  route <- asset_route("/assets", "fixtures/test_files")

  expect_snapshot(
    route$path <- "non/existent/path",
    error = TRUE
  )
})

test_that('AssetRoute headers setter validates input', {
  route <- asset_route("/assets", "fixtures/test_files")

  # Test headers must be named
  expect_snapshot(
    route$headers <- list("Content-Type"),
    error = TRUE
  )

  # Test headers values must be strings
  expect_snapshot(
    route$headers <- list("Content-Type" = 123),
    error = TRUE
  )
})

test_that('is.AssetRoute correctly identifies AssetRoute objects', {
  route <- asset_route("/assets", "fixtures/test_files")
  not_route <- list()

  expect_true(is.AssetRoute(route))
  expect_false(is.AssetRoute(not_route))
})

test_that('AssetRoute on_attach method works', {
  route <- asset_route("/assets", "fixtures/test_files")

  # Mock a fiery app object
  app <- fiery::Fire$new()

  # Call on_attach
  app$attach(route)

  # Verify that the app's attach method was called
  expect_named(app$plugins, "asset_routr")
})
