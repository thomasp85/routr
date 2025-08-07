test_that('resource_route validates inputs', {
  # Check arguments
  expect_snapshot(
    resource_route(continue = "not_logical"),
    error = TRUE
  )

  expect_snapshot(
    resource_route(finalize = "not_function"),
    error = TRUE
  )

  expect_snapshot(
    resource_route(finalize = function(request, response) {}),
    error = TRUE
  )

  expect_snapshot(
    resource_route(default_file = 1),
    error = TRUE
  )

  expect_snapshot(
    resource_route(default_ext = 1),
    error = TRUE
  )

  # Check that named mappings are required
  expect_snapshot(
    resource_route("path/to/dir"),
    error = TRUE
  )
})

test_that('resource_route creates a route with expected properties', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/",
    default_file = 'index.html',
    default_ext = 'html'
  )

  # Check the type
  expect_s3_class(route, "Route")

  # Check that it has a handler for the mapped path
  expect_true(!is.null(route$get_handler("all", "/files/*")))
})

test_that('resource_route supports complete_paths function', {
  # Create route with various path formats
  route <- resource_route(
    '/complete/' = "fixtures/test_files/",  # with leading and trailing slash
    'incomplete' = "fixtures/test_files"    # without leading and trailing slash
  )

  # Check handlers exist for both paths
  expect_true(!is.null(route$get_handler("all", "/complete/*")))
  expect_true(!is.null(route$get_handler("all", "/incomplete/*")))
})

test_that('resource_route handles requests for existing files', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Test request to an existing file
  rook <- fiery::fake_request('www.example.com/files/test.txt')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 200L)
  expect_equal(basename(res$body), "test.txt")
  expect_equal(res$type, "text/plain")
})

test_that('resource_route adds default extension when needed', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/",
    default_ext = 'html'
  )

  # Test request to a file without extension
  rook <- fiery::fake_request('www.example.com/files/sample')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 200L)
  expect_equal(basename(res$body), "sample.html")
  expect_equal(res$type, "text/html")
})

test_that('resource_route adds default file when needed', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/",
    default_file = 'index.html'
  )

  # Test request to a directory
  rook <- fiery::fake_request('www.example.com/files/')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 200L)
  expect_equal(basename(res$body), "index.html")
  expect_equal(res$type, "text/html")
})

test_that('resource_route handles subdirectory default files', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Test request to a subdirectory
  rook <- fiery::fake_request('www.example.com/files/subdir/')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 200L)
  expect_true(grepl("subdir/index.html$", res$body))
  expect_equal(res$type, "text/html")
})

test_that('resource_route returns TRUE for non-existent files', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Test request to a non-existent file
  rook <- fiery::fake_request('www.example.com/files/nonexistent.txt')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result - should return TRUE to continue to next route
  expect_true(result)
})

test_that('resource_route handles HTTP methods correctly', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Test with GET method
  rook_get <- fiery::fake_request('www.example.com/files/test.txt', 'get')
  req_get <- reqres::Request$new(rook_get)
  res_get <- req_get$respond()

  result_get <- route$dispatch(req_get)
  expect_false(result_get)
  expect_equal(res_get$status, 200L)

  # Test with HEAD method
  rook_head <- fiery::fake_request('www.example.com/files/test.txt', 'head')
  req_head <- reqres::Request$new(rook_head)
  res_head <- req_head$respond()

  result_head <- route$dispatch(req_head)
  expect_false(result_head)
  expect_equal(res_head$status, 200L)
  expect_equal(res_head$body, "")  # HEAD should not have body

  # Test with POST method - should not be handled
  rook_post <- fiery::fake_request('www.example.com/files/test.txt', 'post')
  req_post <- reqres::Request$new(rook_post)
  res_post <- req_post$respond()

  result_post <- route$dispatch(req_post)
  expect_true(result_post)  # Should return TRUE to continue to next route
})

test_that('resource_route handles finalize function correctly', {
  # Create finalize function to track calls
  finalize_called <- FALSE
  test_finalize <- function(request, response, ...) {
    finalize_called <<- TRUE
    response$set_header("X-Test", "finalized")
  }

  # Create route with finalize function
  route <- resource_route(
    '/files/' = "fixtures/test_files/",
    finalize = test_finalize
  )

  # Test request
  rook <- fiery::fake_request('www.example.com/files/test.txt')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_true(finalize_called)
  expect_equal(res$get_header("X-Test"), "finalized")
})

test_that('resource_route handles encodings correctly', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Test request with Accept-Encoding header
  rook <- fiery::fake_request('www.example.com/files/sample.html')
  rook$HTTP_ACCEPT_ENCODING <- "gzip, deflate, br"
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 200L)
  expect_equal(res$get_header("Content-Encoding"), "gzip")
})

test_that('resource_route handles If-Modified-Since correctly', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Get file info to set future modification time
  file_path <- "fixtures/test_files/test.txt"
  file_info <- fs::file_info(file_path)
  future_time <- file_info$modification_time + 3600 #one hour into the future

  # Test request with If-Modified-Since header in the future
  rook <- fiery::fake_request('www.example.com/files/test.txt')
  rook$HTTP_IF_MODIFIED_SINCE <- reqres::to_http_date(future_time)
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 304L)
})

test_that('resource_route handles ETag correctly', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Get file info to calculate ETag
  file_path <- "fixtures/test_files/test.txt"
  file_info <- fs::file_info(file_path)
  etag <- rlang::hash(file_info$modification_time)

  # Test request with If-None-Match header matching ETag
  rook <- fiery::fake_request('www.example.com/files/test.txt')
  rook$HTTP_IF_NONE_MATCH <- etag
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 304L)
})

test_that('resource_route sets correct headers', {
  # Create route
  route <- resource_route(
    '/files/' = "fixtures/test_files/"
  )

  # Test request
  rook <- fiery::fake_request('www.example.com/files/test.txt')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check headers
  expect_false(result)
  expect_equal(res$type, "text/plain")
  expect_equal(res$get_header("Content-Encoding"), "identity")
  expect_false(is.null(res$get_header("ETag")))
  expect_equal(res$get_header("Cache-Control"), "max-age=3600")
  expect_false(is.null(res$get_header("Last-Modified")))
  expect_equal(res$get_header("Content-Location"), "/files/test.txt")
})

test_that('resource_route respects continue parameter', {
  # Create route with continue=TRUE
  route_continue <- resource_route(
    '/files/' = "fixtures/test_files/",
    continue = TRUE
  )

  # Test request
  rook <- fiery::fake_request('www.example.com/files/test.txt')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route_continue$dispatch(req)

  # Check the result
  expect_true(result)  # Should return TRUE to continue
  expect_equal(res$status, 200L)
})
