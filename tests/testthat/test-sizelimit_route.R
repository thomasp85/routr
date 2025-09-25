test_that("sizelimit_route validates numeric limits correctly", {
  # Test with valid numeric limit
  expect_no_error(sizelimit_route(1024))

  # Test with negative limit
  expect_snapshot(
    sizelimit_route(-1),
    error = TRUE
  )

  # Test with non-numeric limit
  expect_snapshot(
    sizelimit_route("not a number"),
    error = TRUE
  )
})

test_that("sizelimit_route validates function limits correctly", {
  # Test with valid function limit
  expect_no_error(sizelimit_route(function(request) 1024))

  # Test with function that doesn't have 'request' parameter
  expect_snapshot(
    sizelimit_route(function() 1024),
    error = TRUE
  )
})

test_that("sizelimit_route creates a route with the correct handler", {
  # Create a size limit route
  route <- sizelimit_route(1024, "post", "/upload")

  # Check that it's a Route
  expect_s3_class(route, "Route")

  # Check that it has the correct handler
  expect_true(!is.null(route$get_handler("post", "/upload")))
})

test_that("sizelimit_route allows requests under the limit", {
  # Create a size limit route with 5MB limit
  route <- sizelimit_route(5 * 1024^2)

  # Create a request with a small Content-Length
  rook <- fiery::fake_request('www.example.com', 'post')
  rook$HTTP_CONTENT_LENGTH <- "1024" # 1KB
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should allow the request to continue (return TRUE)
  expect_true(result)
  # Status should remain unchanged
  expect_equal(res$status, 404L) # Default status code for fiery::fake_request
})

test_that("sizelimit_route rejects requests over the limit with 413", {
  # Create a size limit route with 1KB limit
  route <- sizelimit_route(1024)

  # Create a request with a large Content-Length
  rook <- fiery::fake_request('www.example.com', 'post')
  rook$HTTP_CONTENT_LENGTH <- "2048" # 2KB, over the limit
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should reject the request (return FALSE)
  expect_false(result)
  # Status should be 413 Request Entity Too Large
  expect_equal(res$status, 413L)
})

test_that("sizelimit_route rejects requests without Content-Length with 411", {
  # Create a size limit route with 1KB limit
  route <- sizelimit_route(1024)

  # Create a request without Content-Length
  rook <- fiery::fake_request('www.example.com', 'post')
  # Ensure Content-Length is not set
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should reject the request (return FALSE)
  expect_false(result)
  # Status should be 411 Length Required
  expect_equal(res$status, 411L)
})

test_that("sizelimit_route works with function limit", {
  # Create a size limit route with a function limit
  dynamic_limit <- function(request) {
    # Return different limits based on the path
    if (grepl("/upload", request$path)) {
      return(1024) # 1KB for uploads
    } else {
      return(10 * 1024^2) # 10MB for other requests
    }
  }

  route <- sizelimit_route(dynamic_limit)

  # Test with upload path and small content
  rook_small <- fiery::fake_request('www.example.com/upload', 'post')
  rook_small$HTTP_CONTENT_LENGTH <- "512" # Under limit
  req_small <- reqres::Request$new(rook_small)
  res_small <- req_small$respond()
  expect_true(route$dispatch(req_small)) # Should allow

  # Test with upload path and large content
  rook_large <- fiery::fake_request('www.example.com/upload', 'post')
  rook_large$HTTP_CONTENT_LENGTH <- "2048" # Over limit
  req_large <- reqres::Request$new(rook_large)
  res_large <- req_large$respond()
  expect_false(route$dispatch(req_large)) # Should reject
  expect_equal(res_large$status, 413L)

  # Test with different path and large content
  rook_other <- fiery::fake_request('www.example.com/other', 'post')
  rook_other$HTTP_CONTENT_LENGTH <- "5242880" # 5MB, under 10MB limit
  req_other <- reqres::Request$new(rook_other)
  res_other <- req_other$respond()
  expect_true(route$dispatch(req_other)) # Should allow
})

test_that("sizelimit_route validates function limit return value", {
  # Create a route with a function that returns an invalid limit
  bad_limit <- function(request) {
    return("not a number")
  }

  route <- sizelimit_route(bad_limit)

  # Create a request
  rook <- fiery::fake_request('www.example.com', 'post')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch should error because the function returns a non-numeric value
  expect_snapshot(route$dispatch(req), error = TRUE)
})

test_that("sizelimit_route works with different methods", {
  # Create a route that only limits POST requests
  route <- sizelimit_route(1024, method = "post")

  # Test with POST method and large content
  rook_post <- fiery::fake_request('www.example.com', 'post')
  rook_post$HTTP_CONTENT_LENGTH <- "2048" # Over limit
  req_post <- reqres::Request$new(rook_post)
  res_post <- req_post$respond()
  expect_false(route$dispatch(req_post)) # Should reject
  expect_equal(res_post$status, 413L)

  # Test with GET method and large content (should not be limited)
  rook_get <- fiery::fake_request('www.example.com', 'get')
  rook_get$HTTP_CONTENT_LENGTH <- "2048" # Over limit but GET method
  req_get <- reqres::Request$new(rook_get)
  res_get <- req_get$respond()
  # The route won't match because the method doesn't match
  expect_true(route$dispatch(req_get)) # Should allow
})

test_that("sizelimit_route works with specific paths", {
  # Create a route that only limits requests to /upload path
  route <- sizelimit_route(1024, path = "/upload")

  # Test with /upload path and large content
  rook_upload <- fiery::fake_request('www.example.com/upload', 'post')
  rook_upload$HTTP_CONTENT_LENGTH <- "2048" # Over limit
  req_upload <- reqres::Request$new(rook_upload)
  res_upload <- req_upload$respond()
  expect_false(route$dispatch(req_upload)) # Should reject
  expect_equal(res_upload$status, 413L)

  # Test with different path and large content
  rook_other <- fiery::fake_request('www.example.com/other', 'post')
  rook_other$HTTP_CONTENT_LENGTH <- "2048" # Over limit but different path
  req_other <- reqres::Request$new(rook_other)
  res_other <- req_other$respond()
  # The route won't match because the path doesn't match
  expect_true(route$dispatch(req_other)) # Should allow
})

test_that("sizelimit_route works in a route stack", {
  # Create a size limit route
  limit_route <- sizelimit_route(1024)

  # Create a route that will only be reached if size limit passes
  app_route <- Route$new(
    post = list(
      "/" = function(request, response, ...) {
        response$status <- 200L
        response$body <- "Request processed"
        FALSE
      }
    )
  )

  # Create a route stack
  stack <- RouteStack$new(size_limit = limit_route, app = app_route)

  # Test with small request
  rook_small <- fiery::fake_request('www.example.com', 'post')
  rook_small$HTTP_CONTENT_LENGTH <- "512" # Under limit
  req_small <- reqres::Request$new(rook_small)
  res_small <- req_small$respond()

  # Dispatch the request through the stack
  result_small <- stack$dispatch(req_small)

  # Should reach the app route and return its result (FALSE)
  expect_false(result_small)
  expect_equal(res_small$body, "Request processed")

  # Test with large request
  rook_large <- fiery::fake_request('www.example.com', 'post')
  rook_large$HTTP_CONTENT_LENGTH <- "2048" # Over limit
  req_large <- reqres::Request$new(rook_large)
  res_large <- req_large$respond()

  # Dispatch the request through the stack
  result_large <- stack$dispatch(req_large)

  # Should be rejected by size limit route and not reach app route
  expect_false(result_large)
  expect_equal(res_large$status, 413L)
})
