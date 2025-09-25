test_that("shared_secret_route validates inputs correctly", {
  # Check that secret must be a string
  expect_snapshot(
    shared_secret_route(123, "X-Secret"),
    error = TRUE
  )

  # Check that header must be a string
  expect_snapshot(
    shared_secret_route("my-secret", 123),
    error = TRUE
  )
})

test_that("shared_secret_route creates a route with the correct handler", {
  # Create a shared secret route
  route <- shared_secret_route("my-secret", "X-Secret")

  # Check that it's a Route
  expect_s3_class(route, "Route")

  # Check that it has an 'all' handler for all paths
  expect_true(!is.null(route$get_handler("all", "*")))
})

test_that("shared_secret_route allows requests with correct secret", {
  # Create a shared secret route
  route <- shared_secret_route("my-secret", "X-Secret")

  # Create a request with the correct secret
  rook <- fiery::fake_request(
    'www.example.com',
    'get',
    headers = list("x-secret" = "my-secret")
  )
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should allow the request to continue (return TRUE)
  expect_true(result)
  # Status should not be changed (default is 404)
  expect_equal(res$status, 404L)
})

test_that("shared_secret_route rejects requests with incorrect secret", {
  # Create a shared secret route
  route <- shared_secret_route("my-secret", "X-Secret")

  # Create a request with an incorrect secret
  rook <- fiery::fake_request('www.example.com', 'get')
  rook$HTTP_X_SECRET <- "wrong-secret"
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should reject the request (return FALSE)
  expect_false(result)
  # Status should be 400 Bad Request
  expect_equal(res$status, 400L)
})

test_that("shared_secret_route rejects requests without a secret", {
  # Create a shared secret route
  route <- shared_secret_route("my-secret", "X-Secret")

  # Create a request without a secret
  rook <- fiery::fake_request('www.example.com', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should reject the request (return FALSE)
  expect_false(result)
  # Status should be 400 Bad Request
  expect_equal(res$status, 400L)
})

test_that("shared_secret_route handles different HTTP methods", {
  # Create a shared secret route
  route <- shared_secret_route("my-secret", "X-Secret")

  # Test with GET
  rook_get <- fiery::fake_request('www.example.com', 'get')
  rook_get$HTTP_X_SECRET <- "my-secret"
  req_get <- reqres::Request$new(rook_get)
  res_get <- req_get$respond()
  expect_true(route$dispatch(req_get))

  # Test with POST
  rook_post <- fiery::fake_request('www.example.com', 'post')
  rook_post$HTTP_X_SECRET <- "my-secret"
  req_post <- reqres::Request$new(rook_post)
  res_post <- req_post$respond()
  expect_true(route$dispatch(req_post))

  # Test with PUT
  rook_put <- fiery::fake_request('www.example.com', 'put')
  rook_put$HTTP_X_SECRET <- "my-secret"
  req_put <- reqres::Request$new(rook_put)
  res_put <- req_put$respond()
  expect_true(route$dispatch(req_put))
})

test_that("shared_secret_route handles header with hyphens correctly", {
  # Create a shared secret route with a hyphenated header name
  route <- shared_secret_route("my-secret", "X-My-Secret-Header")

  # Create a request with the correct header (note HTTP_ prefix and underscores)
  rook <- fiery::fake_request('www.example.com', 'get')
  rook$HTTP_X_MY_SECRET_HEADER <- "my-secret"
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should allow the request to continue
  expect_true(result)
})

test_that("shared_secret_route respects secret case-sensitivity", {
  # Create a shared secret route
  route <- shared_secret_route("My-Secret", "X-Secret")

  # Create a request with the secret in different case
  rook <- fiery::fake_request('www.example.com', 'get')
  rook$HTTP_X_SECRET <- "my-secret" # Different case
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Should reject the request (secrets should be case-sensitive)
  expect_false(result)
  expect_equal(res$status, 400L)
})

test_that("shared_secret_route works in a route stack", {
  # Create a shared secret route
  auth_route <- shared_secret_route("my-secret", "X-Secret")

  # Create a route that will only be reached if authentication passes
  protected_route <- Route$new(
    get = list(
      "/" = function(request, response, ...) {
        response$status <- 200L
        response$body <- "Protected content"
        FALSE
      }
    )
  )

  # Create a route stack
  stack <- RouteStack$new(auth = auth_route, protected = protected_route)

  # Test with correct secret
  rook_auth <- fiery::fake_request('www.example.com', 'get')
  rook_auth$HTTP_X_SECRET <- "my-secret"
  req_auth <- reqres::Request$new(rook_auth)
  res_auth <- req_auth$respond()

  # Dispatch the request through the stack
  result_auth <- stack$dispatch(req_auth)

  # Should reach the protected route and return its result (FALSE)
  expect_false(result_auth)
  expect_equal(res_auth$body, "Protected content")

  # Test with incorrect secret
  rook_unauth <- fiery::fake_request('www.example.com', 'get')
  req_unauth <- reqres::Request$new(rook_unauth)
  res_unauth <- req_unauth$respond()

  # Dispatch the request through the stack
  result_unauth <- stack$dispatch(req_unauth)

  # Should be rejected by auth route and not reach protected route
  expect_false(result_unauth)
  expect_equal(res_unauth$status, 400L)
})
