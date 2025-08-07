test_that('route() function works', {
  # Test empty route
  empty_route <- route()
  expect_s3_class(empty_route, "Route")
  expect_equal(empty_route$root, "/")

  # Test route with root
  rooted_route <- route(root = "/api")
  expect_equal(rooted_route$root, "/api")

  # Test route with handlers
  handler_fn <- function(request, response, keys, ...) TRUE
  route_with_handlers <- route(get = list('/test' = handler_fn))
  expect_equal(route_with_handlers$get_handler("get", "/test"), handler_fn)
})

test_that('route_add() works', {
  test_route <- route()
  handler_fn <- function(request, response, keys, ...) TRUE

  # Test adding a handler
  modified_route <- route_add(test_route, "get", "/test", handler_fn)
  expect_identical(modified_route, test_route) # Should return the same object
  expect_equal(test_route$get_handler("get", "/test"), handler_fn)

  # Test adding multiple handlers
  handler_fn2 <- function(request, response, keys, ...) FALSE
  modified_route <- route_add(test_route, "post", "/test", handler_fn2)
  expect_equal(test_route$get_handler("post", "/test"), handler_fn2)

  # Test adding handler with parameters
  param_handler <- function(request, response, keys, ...) TRUE
  modified_route <- route_add(test_route, "get", "/:param", param_handler)
  expect_equal(test_route$get_handler("get", "/:param"), param_handler)
})

test_that('route_remove() works', {
  # Setup
  test_route <- route()
  handler_fn <- function(request, response, keys, ...) TRUE
  test_route <- route_add(test_route, "get", "/test", handler_fn)

  # Test removal
  modified_route <- route_remove(test_route, "get", "/test")
  expect_identical(modified_route, test_route) # Should return the same object
  expect_null(test_route$get_handler("get", "/test"))

  # Test removing a non-existent handler
  expect_warning(route_remove(test_route, "get", "/nonexistent"), NA)
})

test_that('route_get() works', {
  # Setup
  test_route <- route()
  handler_fn <- function(request, response, keys, ...) TRUE
  test_route <- route_add(test_route, "get", "/test", handler_fn)

  # Test getting a handler
  result <- route_get(test_route, "get", "/test")
  expect_identical(result, test_route) # Should return the route itself

  # Test getting a non-existent handler
  result <- route_get(test_route, "post", "/test")
  expect_identical(result, test_route) # Should return the route itself even if handler doesn't exist
})

test_that('route_merge() works', {
  # Setup
  route1 <- route()
  handler_fn1 <- function(request, response, keys, ...) TRUE
  route1 <- route_add(route1, "get", "/test1", handler_fn1)

  route2 <- route()
  handler_fn2 <- function(request, response, keys, ...) FALSE
  route2 <- route_add(route2, "post", "/test2", handler_fn2)

  # Test merging
  merged_route <- route_merge(route1, route2)
  expect_identical(merged_route, route1) # Should return the first route
  expect_equal(merged_route$get_handler("get", "/test1"), handler_fn1)
  expect_equal(merged_route$get_handler("post", "/test2"), handler_fn2)

  # Test merging with root
  route3 <- route(root = "/api")
  handler_fn3 <- function(request, response, keys, ...) TRUE
  route3 <- route_add(route3, "get", "/test3", handler_fn3)

  merged_with_root <- route_merge(route1, route3)
  expect_equal(merged_with_root$get_handler("get", "/api/test3"), handler_fn3)

  # Test merging without using root
  route4 <- route(root = "/api")
  handler_fn4 <- function(request, response, keys, ...) TRUE
  route4 <- route_add(route4, "get", "/test4", handler_fn3)
  merged_no_root <- route_merge(route1, route4, use_root = FALSE)
  expect_equal(merged_no_root$get_handler("get", "/test4"), handler_fn3)
})

test_that('route_stack() works with different input types', {
  # Test empty stack
  empty_stack <- route_stack()
  expect_s3_class(empty_stack, "RouteStack")

  # Test stacking with Route
  test_route <- route()
  handler_fn <- function(request, response, keys, ...) TRUE
  test_route <- route_add(test_route, "get", "/test", handler_fn)

  stacked_route <- route_stack(test_route, another = route())
  expect_s3_class(stacked_route, "RouteStack")
  expect_true(stacked_route$has_route("x"))
  expect_true(stacked_route$has_route("another"))

  # Test stacking with AssetRoute
  asset_r <- asset_route("/assets", system.file(package = "routr"))
  stacked_asset <- route_stack(asset_r, api = route())
  expect_s3_class(stacked_asset, "RouteStack")
  expect_true(stacked_asset$has_route("x"))
  expect_true(stacked_asset$has_route("api"))

  # Test adding to existing RouteStack
  stack <- route_stack()
  stack_with_routes <- route_stack(stack, first = route(), second = route())
  expect_true(stack_with_routes$has_route("first"))
  expect_true(stack_with_routes$has_route("second"))

  # Test invalid input
  expect_snapshot(route_stack(1), error = TRUE)
})

test_that('route_stack() with .after parameter works', {
  stack <- route_stack()
  r1 <- route()
  r2 <- route()
  r3 <- route()

  # Add routes in specific order
  stack <- route_stack(stack, first = r1)
  stack <- route_stack(stack, third = r3)
  stack <- route_stack(stack, second = r2, .after = 1)

  # Check that the order is correct
  route_names <- stack$routes
  expect_equal(route_names[1], "first")
  expect_equal(route_names[2], "second")
  expect_equal(route_names[3], "third")
})
