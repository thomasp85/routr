test_that('openapi_route validates spec file exists', {
  expect_snapshot(
    openapi_route("nonexistent_file.json"),
    error = TRUE
  )
})

test_that('openapi_route handles json spec files', {
  # Using the fixture JSON file
  json_spec <- "fixtures/sample_openapi.json"

  # Create route with default settings
  route <- openapi_route(json_spec)
  expect_s3_class(route, "Route")

  # Check that route has the expected handlers
  expect_true(!is.null(route$get_handler("get", "/openapi.json")))
  expect_true(!is.null(route$get_handler("get", "__docs__")))
  expect_true(!is.null(route$get_handler("get", "__docs__/")))
  expect_true(!is.null(route$get_handler("get", "__docs__/index.html")))

  # Test with custom root
  custom_route <- openapi_route(json_spec, root = "/api/docs")
  expect_true(!is.null(custom_route$get_handler("get", "/api/docs")))
  expect_true(!is.null(custom_route$get_handler("get", "/api/docs/")))
  expect_true(!is.null(custom_route$get_handler("get", "/api/docs/index.html")))
})

test_that('openapi_route handles yaml spec files', {
  # Using the fixture YAML file
  yaml_spec <- "fixtures/sample_openapi.yaml"

  # Create route with default settings
  route <- openapi_route(yaml_spec)
  expect_s3_class(route, "Route")

  # Check that route has the expected handlers
  expect_true(!is.null(route$get_handler("get", "/openapi.yaml")))
})

test_that('openapi_route works with different UIs', {
  skip_if_not_installed("rapidoc")
  skip_if_not_installed("swagger")
  skip_if_not_installed("redoc")

  json_spec <- "fixtures/sample_openapi.json"

  # Test with RapiDoc UI
  rapidoc_route <- openapi_route(json_spec, ui = "rapidoc")
  expect_s3_class(rapidoc_route, "Route")
  expect_true(!is.null(rapidoc_route$get_handler("get", "__docs__/")))

  # Test with Swagger UI
  swagger_route <- openapi_route(json_spec, ui = "swagger")
  expect_s3_class(swagger_route, "Route")
  expect_true(!is.null(swagger_route$get_handler("get", "__docs__/")))

  # Test with Redoc UI
  redoc_route <- openapi_route(json_spec, ui = "redoc")
  expect_s3_class(redoc_route, "Route")
  expect_true(!is.null(redoc_route$get_handler("get", "__docs__/")))
})

test_that('openapi_route properly handles different root depths', {
  json_spec <- "fixtures/sample_openapi.json"

  # Test with nested root
  nested_route <- openapi_route(json_spec, root = "/api/v1/docs")
  expect_true(!is.null(nested_route$get_handler("get", "/api/v1/docs")))
  expect_true(!is.null(nested_route$get_handler("get", "/api/v1/docs/")))
  expect_true(!is.null(nested_route$get_handler("get", "/api/v1/docs/index.html")))
})

test_that('openapi_route sets correct response for the spec file', {
  json_spec <- "fixtures/sample_openapi.json"

  # Create route
  route <- openapi_route(json_spec)

  # Create a mock request to the spec file
  rook <- fiery::fake_request('www.example.com/openapi.json', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 200L)
  expect_match(res$file, "fixtures/sample_openapi.json", fixed = TRUE)
  expect_equal(res$type, "application/json")
})

test_that('openapi_route handles redirects correctly', {
  json_spec <- "fixtures/sample_openapi.json"

  # Create route
  route <- openapi_route(json_spec, root = "/docs")

  # Create a mock request to the root path
  rook <- fiery::fake_request('www.example.com/docs', 'get')
  req <- reqres::Request$new(rook)
  res <- req$respond()

  # Dispatch the request
  result <- route$dispatch(req)

  # Check the result
  expect_false(result)
  expect_equal(res$status, 308L)
  expect_equal(res$get_header('Location'), '/docs/')
})
