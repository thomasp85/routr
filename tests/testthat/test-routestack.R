context("routestack")

test_that('routestacks can be initiated with routes', {
  router <- RouteStack$new()
  expect_output(router$print(), 'A RouteStack containing 0 routes')
  route <- Route$new()
  router <- RouteStack$new(test = route)
  expect_output(router$print(), 'A RouteStack containing 1 routes')
})

test_that('routes can be added, queried, and removed', {
  router <- RouteStack$new()
  route <- Route$new()
  router$add_route(route, 'test')
  expect_output(router$print(), 'A RouteStack containing 1 routes')
  expect_true(router$has_route('test'))
  expect_identical(router$get_route('test'), route)
  expect_silent(router$remove_route('test'))
  expect_output(router$print(), 'A RouteStack containing 0 routes')
  expect_warning(router$remove_route('test'))

  router$add_route(route, 'test')
  expect_error(router$add_route(route, 'test'))
  router$add_route(route, 'test2')
  router$add_route(route, 'test3', 0)
  router$add_route(route, 'test4', 2)
  expect_output(router$print(), '1: test3\n2: test\n3: test4\n4: test2')
})

test_that('attach_to and name works as expected', {
  router <- RouteStack$new()
  expect_equal(router$attach_to, 'request')
  expect_equal(router$name, 'request_routr')

  expect_error(router$attach_to <- 'test')
  expect_error(router$name <- 'test')

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
  expect_message(router$dispatch(req), 'not working')

  router$on_error(function(error, request, response) {
    warning(conditionMessage(error))
  })
  expect_warning(router$dispatch(req), 'not working')

  expect_error(router$on_error(function() NULL))
})
