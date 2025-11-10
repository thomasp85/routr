# Package index

## Tidy API

While the R6 classes that are the foundation of routr allow you to
manipulate and work with them directly through their class methods, it
is often more convenient to use a functional and pipe-friendly
interface.

- [`route()`](https://routr.data-imaginist.com/reference/Route.md) :
  Construct a new route
- [`route_stack()`](https://routr.data-imaginist.com/reference/route_stack.md)
  : Combine routes in a stack
- [`route_add()`](https://routr.data-imaginist.com/reference/route_add.md)
  [`route_remove()`](https://routr.data-imaginist.com/reference/route_add.md)
  [`route_get()`](https://routr.data-imaginist.com/reference/route_add.md)
  : Route handlers
- [`route_merge()`](https://routr.data-imaginist.com/reference/route_merge.md)
  : Merge one route into another

## Base Classes

A router based on routr is build up of Route objects combined together
into a RouteStack object. Requests are passed through each route in a
RouteStack and subjected to whatever handlers get matched to the URL at
each step.

- [`Route`](https://routr.data-imaginist.com/reference/Route-class.md) :
  Single route dispatch
- [`RouteStack`](https://routr.data-imaginist.com/reference/RouteStack.md)
  : Combine multiple routes for sequential routing
- [`AssetRoute`](https://routr.data-imaginist.com/reference/AssetRoute.md)
  : Static file serving

## Route constructors

A few predetermined route functionalities have constructors that help
you create them easily.

- [`asset_route()`](https://routr.data-imaginist.com/reference/asset_route.md)
  : High performance route for serving static files
- [`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md)
  : Create a route for fetching files
- [`sizelimit_route()`](https://routr.data-imaginist.com/reference/sizelimit_route.md)
  : Limit the size of requests
- [`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md)
  : Create a route for serving OpenAPI documentation of your server
- [`report_route()`](https://routr.data-imaginist.com/reference/report_route.md)
  : Create a route that renders and serves an Rmarkdown or Quarto report
- [`shared_secret_route()`](https://routr.data-imaginist.com/reference/shared_secret_route.md)
  : Reject requests not in possession of the correct shared secret
