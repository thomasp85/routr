test_that('routestacks can be initiated with routes', {
  router <- RouteStack$new()
  expect_snapshot(router$print())
  route <- Route$new()
  router <- RouteStack$new(test = route)
  expect_snapshot(router$print())
})

test_that('routes can be added, queried, and removed', {
  router <- RouteStack$new()
  route <- Route$new()
  router$add_route(route, 'test')
  expect_snapshot(router$print())
  expect_true(router$has_route('test'))
  expect_identical(router$get_route('test'), route)
  expect_silent(router$remove_route('test'))
  expect_snapshot(router$print())
  expect_snapshot(router$remove_route('test'))

  router$add_route(route, 'test')
  expect_snapshot(router$add_route(route, 'test'), error = TRUE)
  router$add_route(route, 'test2')
  router$add_route(route, 'test3', 0)
  router$add_route(route, 'test4', 2)
  expect_snapshot(router$print())
})

test_that('attach_to and name works as expected', {
  router <- RouteStack$new()
  expect_equal(router$attach_to, 'request')
  expect_equal(router$name, 'request_routr')

  expect_snapshot(router$attach_to <- 'test', error = TRUE)
  expect_snapshot(router$name <- 'test', error = TRUE)

  router$attach_to <- 'message'
  expect_equal(router$attach_to, 'message')
  expect_equal(router$name, 'message_routr')
})

test_that('dispatching works', {
  rook <- fiery::fake_request('www.example.com')
  req <- reqres::Request$new(rook)
  res <- req$respond()
  router <- RouteStack$new()
  route1 <- Route$new(get = list(
    '*' = function(request, response, keys, ...) {
      response$body <- 1
      TRUE
    }
  ))
  router$add_route(route1, 'first')
  expect_true(router$dispatch(req))
  expect_equal(res$body, 1)

  route2 <- Route$new(get = list(
    '*' = function(request, response, keys, ...) {
      response$body <- 2
      FALSE
    }
  ))
  router$add_route(route2, 'second')
  expect_false(router$dispatch(req))
  expect_equal(res$body, 2)
  res$body <- ''

  route3 <- Route$new(get = list(
    '*' = function(request, response, keys, ...) {
      response$body <- 3
      TRUE
    }
  ))
  router$add_route(route2, 'third')
  expect_false(router$dispatch(req))
  expect_equal(res$body, 2)
})

test_that('error handling works', {
  route <- Route$new()
  rook <- fiery::fake_request('www.example.com/test')
  req <- reqres::Request$new(rook)
  route$add_handler('get', '/test', function(request, response, keys, ...) {
    stop('not working', call. = FALSE)
    TRUE
  })
  router <- RouteStack$new(def = route)
  expect_snapshot(router$dispatch(req))

  router$on_error(function(error, request, response) {
    warning(conditionMessage(error))
  })
  expect_snapshot(router$dispatch(req))

  expect_snapshot(router$on_error(function() NULL), error = TRUE)
})
