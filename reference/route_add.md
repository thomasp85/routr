# Route handlers

These functions helps you to add, remove, and retrieve route handlers.
They all (except for `route_get()`) returns the route for easy chaining.

## Usage

``` r
route_add(x, method, path, handler)

route_remove(x, method, path)

route_get(x, method, path)
```

## Arguments

- x:

  A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
  object

- method:

  A request method for the handler. The special method `"all"` will
  allow the handler to match all http request methods that comes in.

- path:

  A URL path for the handler. See Paths for more on path semantics

- handler:

  A handler function. See Handlers for more about the semantics of
  handlers

## Value

`x`, modified. `route_get()` returns the requested handler

## Paths

The path is a URL path consisting of strings, parameters (strings
prefixed with `:`), and wildcards (`*`), separated by `/`. A wildcard
will match anything and is thus not restricted to a single path element
(i.e. it will span multiple `/` if possible). When serving a request
only a single handler is selected based on the path match that is most
specific. Specificity is based on number of path parts (ie. number of
elements separated by `/`), the more the better; number of wildcards,
the fewer the better; and number of keys, the fewer the better. When
printing the route you can see the priority of all the paths in the
route as they are sorted by that

## Handlers

The handler is a function. At the very least it should have a `...`
argument and it must return eiter `TRUE` or `FALSE`. Returning `TRUE`
means that the request is allowed to continue processing and can be
passed on to the next route in the stack. Returning `FALSE` stops the
processing of the request by the stack.

While any arguments besides `...` are optional, there are a few that
will get passed in named:

- `request` will hold the request as a
  [reqres::Request](https://reqres.data-imaginist.com/reference/Request.html)
  object

- `response` will hold the request as a
  [reqres::Response](https://reqres.data-imaginist.com/reference/Response.html)
  object

- `keys` will be a named list containing the values of the matched path
  keys (see example)

Further, if routr is used as a fiery plugin, the handler will receive:

- `server` is the
  [`fiery::Fire`](https://fiery.data-imaginist.com/reference/Fire.html)
  object defining the app

- `id` is the id of the client sending the request, as provided by fiery

- `arg_list` is a list of values as calculated by the servers
  `before-request` event handlers

Any and all of the above can be ignored by your handler, but accepting
the `server` is often paramount to more powerful features such as
delayed execution or logging.

## Examples

``` r
# Add a handler
route <- route() |>
  route_add("get", "/:what", function(request, response, keys, ...) {
    message("Requesting", keys$what)
    TRUE
  })
route
#> A route with 1 handler
#> get:
#> • /:what

# Retrieve the handler
route |> route_get("get", "/:what")
#> A route with 1 handler
#> get:
#> • /:what

# Remove the handler
route |> route_remove("get", "/:what")
#> A route with 0 handlers
```
