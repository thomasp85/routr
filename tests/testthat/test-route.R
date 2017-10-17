context("route")

test_that('Route initialization works', {
  route <- Route$new()
  rook <- fiery::fake_request('www.example.com')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  res$status <- 105L
  expect_output(route$print(), 'A route with 0 handlers')
  expect_true(route$dispatch(req))
  expect_equal(res$status, 105L)
  route <- Route$new(get = list(
    '/test' = function(request, response, keys, ...) {
      response$status <- 205L
      FALSE
    }
  ))
  rook <- fiery::fake_request('www.example.com/test')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  expect_output(route$print(), 'A route with 1 handlers')
  expect_false(route$dispatch(req))
  expect_equal(res$status, 205L)
  rook <- fiery::fake_request('www.example.com/test', 'post')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  expect_true(route$dispatch(req))
})

test_that('handlers can get added and removed', {
  route <- Route$new()
  rook <- fiery::fake_request('www.example.com/test')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  route$add_handler('get', '/test', function(request, response, keys, ...) {
    res$status <- 205L
    FALSE
  })
  expect_output(route$print(), 'A route with 1 handlers')
  expect_false(route$dispatch(req))
  expect_equal(res$status, 205L)

  res$status <- 404L
  route$remove_handler('get', '/test')
  expect_output(route$print(), 'A route with 0 handlers')
  expect_true(route$dispatch(req))
  expect_equal(res$status, 404L)

  expect_warning(route$remove_handler('get', '/test'))
  expect_error(route$add_handler('get', '/test', function(request, ...) {FALSE}))
})

test_that('dispatch dispatches', {
  route <- Route$new()
  rook <- fiery::fake_request('www.example.com/test')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  route$add_handler('all', '/test', function(request, response, keys, ...) {
    res$status <- 205L
    FALSE
  })
  expect_error(route$dispatch(rook))
  expect_false(route$dispatch(req))
  expect_equal(res$status, 205L)

  route <- Route$new()
  rook <- fiery::fake_request('www.example.com/test/this/route')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  route$add_handler('get', '/test/:what/:test', function(request, response, keys, ...) {
    res$set_data('keys', keys)
    FALSE
  })
  expect_false(route$dispatch(req))
  expect_equal(res$get_data('keys'), list(what = 'this', test = 'route'))

  route <- Route$new()
  rook <- fiery::fake_request('www.example.com/test')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  route$add_handler('get', '/test', function(request, response, keys, ...) {
    stop('not working', call. = FALSE)
    TRUE
  })
  expect_error(route$dispatch(req), 'not working')

  route <- Route$new()
  rook <- fiery::fake_request('www.example.com/test')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  route$add_handler('get', '/test', function(request, response, keys, ...) {
    NULL
  })
  expect_error(route$dispatch(req))
})
