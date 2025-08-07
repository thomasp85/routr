test_that('Redirector initializes correctly', {
  redirector <- Redirector$new()
  expect_s3_class(redirector, "Redirector")
  expect_s3_class(redirector, "Route")
})

test_that('Redirector can set up temporary redirects', {
  redirector <- Redirector$new()

  # Add a simple redirect
  redirector$redirect_temporary("get", "/old", "/new")

  # Test the redirect
  rook <- fiery::fake_request('www.example.com/old', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- redirector$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 307L)
  expect_equal(res$get_header("Location"), "/new")
})

test_that('Redirector can set up permanent redirects', {
  redirector <- Redirector$new()

  # Add a simple redirect
  redirector$redirect_permanently("get", "/old", "/new")

  # Test the redirect
  rook <- fiery::fake_request('www.example.com/old', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- redirector$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 308L)
  expect_equal(res$get_header("Location"), "/new")
})

test_that('Redirector handles path parameters', {
  redirector <- Redirector$new()

  # Add a redirect with path parameters
  redirector$redirect_temporary("get", "/users/:id", "/profiles/:id")

  # Test the redirect
  rook <- fiery::fake_request('www.example.com/users/123', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- redirector$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 307L)
  expect_equal(res$get_header("Location"), "/profiles/123")
})

test_that('Redirector handles wildcards', {
  redirector <- Redirector$new()

  # Add a redirect with wildcards
  redirector$redirect_temporary("get", "/old/*", "/new/*")

  # Test the redirect
  rook <- fiery::fake_request('www.example.com/old/path/to/resource', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- redirector$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 307L)
  expect_equal(res$get_header("Location"), "/new/path/to/resource")
})

test_that('Redirector supports all HTTP methods', {
  redirector <- Redirector$new()

  # Add a redirect for all methods
  redirector$redirect_temporary("all", "/api/v1/*", "/api/v2/*")

  # Test GET request
  rook_get <- fiery::fake_request('www.example.com/api/v1/users', 'get')
  req_get <- reqres::Request$new(rook_get)
  res_get <- req_get$respond()
  result_get <- redirector$dispatch(req_get)
  expect_false(result_get)
  expect_equal(res_get$status, 307L)
  expect_equal(res_get$get_header("Location"), "/api/v2/users")

  # Test POST request
  rook_post <- fiery::fake_request('www.example.com/api/v1/users', 'post')
  req_post <- reqres::Request$new(rook_post)
  res_post <- req_post$respond()
  result_post <- redirector$dispatch(req_post)
  expect_false(result_post)
  expect_equal(res_post$status, 307L)
  expect_equal(res_post$get_header("Location"), "/api/v2/users")
})

test_that('Redirector ignores query parameters', {
  redirector <- Redirector$new()

  # Add a redirect
  redirector$redirect_temporary("get", "/search?query=test", "/search-new")

  # Test with a request that includes query parameters
  rook <- fiery::fake_request('www.example.com/search?query=test&page=2', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- redirector$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 307L)
  expect_equal(res$get_header("Location"), "/search-new")
})

test_that('make_redirect_handler validates target parameters', {
  # Test that the to path cannot contain parameters not in the from path
  expect_snapshot(
    make_redirect_handler(
      list(keys = c("id"), n_wildcard = 0),
      c("user_id"),
      "/profiles/:user_id",
      307L
    ),
    error = TRUE
  )
})

test_that('Redirector validation checks input types', {
  redirector <- Redirector$new()

  # Test method validation
  expect_snapshot(
    redirector$redirect_temporary("INVALID", "/old", "/new"),
    error = TRUE
  )

  # Test from path validation
  expect_snapshot(
    redirector$redirect_temporary("get", 123, "/new"),
    error = TRUE
  )

  # Test to path validation
  expect_snapshot(
    redirector$redirect_temporary("get", "/old", 123),
    error = TRUE
  )
})
