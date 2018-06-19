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

test_that('route remapping works', {
  
  # first, test for adding a prefix to a route path 
  r <- Route$new()
  original <- function(request, response, keys, ...) {
    response$status <- 200L
    response$body <- "All is good"
    return(FALSE)
  }
  r$add_handler("get", "/", original)
  r$remap_handlers(function(method, path, handler) {
    r$add_handler(method, paste0('/prefix', path), handler)
  })
  expect_identical(
    r$get_handler("get", "/prefix/"), 
    original
  )
  
  # next, test we can modify the route handler
  new_handler <- function(request, response, keys, ...) {
    response$status <- 404L
    response$body <- "Not so good"
    return(FALSE)
  }
  r$remap_handlers(function(method, path, handler) {
    r$add_handler(method, paste0('/prefix2', path), new_handler)
  })
  expect_identical(
    r$get_handler("get", "/prefix2/prefix/"), 
    new_handler
  )
})
