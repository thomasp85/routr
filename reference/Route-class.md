# Single route dispatch

The `Route` class is used to encapsulate a single URL dispatch, that is,
chose a single handler from a range based on a URL path. A handler will
be called with a request, response, and keys argument as well as any
additional arguments passed on to `dispatch()`.

## Method matching

A handler is referencing a specific HTTP method (`get`, `post`, etc.)
but can also reference `all` to indicate that it should match all types
of requests. Handlers referencing `all` have lower precedence than those
referencing specific methods, so will only be called if a match is not
found within the handlers of the specific method.

## Path matching

The path will be stripped the query string prior to handler lookup.
routr is using the waysign package to match URL paths to the path
pattern provided along with the handler. A path pattern consists of zero
or more elements separated by `/`, each element can be one of three
basic types:

- **Fixed:** Is a string literal that will be matched exactly. The
  pattern `/user/thomas` consists of two fixed elements and will only
  ever be matched exactly to `/user/thomas`

- **Parameterized:** Is a variable that can take the value of any string
  (not including a `/`). A parameter consist of a `:` followed by a name
  (made up of alphanumeric characters). The patter `/user/:id` consist
  of a literal and a parameter and will match to e.g. `/user/thomas` and
  `user/hana`, but not `user/thomas/settings`. A parameter doesn't have
  to take up all of an element, it can be a mix of literal and one or
  more parameters, e.g. `/posts/date-:year-:month-:day` will match to
  `posts/date-2025-11-05`. If you want to add an alphanumeric literal
  end to a parameterized element you can separate it by `\\` like
  `/posts/:title\\post` which will match `/posts/hello_worldpost`. A
  parameter can be made optional by terminating it with `?`.
  `/user/:id?` will match both `/user/thomas` and `/user/`. While
  optional parameters are most useful in the end of a path they can also
  be in the middle of a pattern, e.g. `/user/:id?/settings` which will
  match `/user/thomas/settings` and `/user//settings` (note the double
  slashes)

- **Wildcards:** Is like parameters except they can take up multiple
  elements (i.e. the match to strings that contain `/`). The come in two
  flavors: one-or-more and zero-or-more. The first uses a `+` and the
  latter a `*`. These can and should be named be prepending a parameter
  name to the operator (e.g. `:name+`). The pattern `/user/:id+` will
  match `/user/thomas` and `user/thomas/settings`, the pattern
  `user/:id*` will match those two as well and additionally match
  `/user/`.

The syntax allows for multiple patterns matching to the same string,
e.g. `/posts/:date`, `/posts/:day-:month-:year`, and
`/posts/:remainder+` all matches to `/posts/03-09-2024`. waysign
resolves this by always matching to the most specific pattern. Literals
are more specific than parameters which are more specific than
wildcards. Further, a element consisting of multiple parameters are
considered more specific than one consisting of fewer.

## Handler calling

Handlers are only called for their side-effects and are expected to
return either `TRUE` or `FALSE` indicating whether additional routes in
a
[`RouteStack`](https://routr.data-imaginist.com/reference/RouteStack.md)
should be called, e.g. if a handler is returning `FALSE` all further
processing of the request will be terminated and the response will be
passed along in its current state. Thus, the intend of the handlers is
to modify the request and response objects, in place.

When the handler is called it will be passed in a
[request](https://reqres.data-imaginist.com/reference/Request.html)
object to the `request` argument, a
[response](https://reqres.data-imaginist.com/reference/Response.html)
object to the `response` argument and a list to the `keys` argument. The
names of the elements in the `keys` list will match those given in the
pattern (excluding the `:`) and the value will be the part it matched
to. If wildcards are unnamed they will be named after their index and
type, e.g. `/path/+/and/some/more/*` will automatically name the two
wildcards `+1` and `*2`. To avoid ambiguity and errors it is recommended
to explicitly name wildcards if you intend to use their value for
anything. In addition to `request`, `response`, and `keys` any argument
passed to the `...` in the `dispatch()` method is also passed into the
handler.

## Initialization

A new 'Route'-object is initialized using the `new()` method on the
generator or alternatively by using
[`route()`](https://routr.data-imaginist.com/reference/Route.md):

**Usage**

|                           |
|---------------------------|
| `route <- Route$new(...)` |

|                       |
|-----------------------|
| `route <- route(...)` |

## See also

[RouteStack](https://routr.data-imaginist.com/reference/RouteStack.md)
for binding multiple routes sequentially

## Active bindings

- `root`:

  The root of the route. Will be removed from the path of any request
  before matching a handler

- `name`:

  An autogenerated name for the route

- `empty`:

  Is the route empty

## Methods

### Public methods

- [`Route$new()`](#method-Route-new)

- [`Route$print()`](#method-Route-print)

- [`Route$add_handler()`](#method-Route-add_handler)

- [`Route$remove_handler()`](#method-Route-remove_handler)

- [`Route$get_handler()`](#method-Route-get_handler)

- [`Route$remap_handlers()`](#method-Route-remap_handlers)

- [`Route$merge_route()`](#method-Route-merge_route)

- [`Route$dispatch()`](#method-Route-dispatch)

- [`Route$on_attach()`](#method-Route-on_attach)

- [`Route$clone()`](#method-Route-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Route

#### Usage

    Route$new(..., ignore_trailing_slash = FALSE)

#### Arguments

- `...`:

  Handlers to add up front. Must be in the form of named lists where the
  names corresponds to paths and the elements are the handlers. The name
  of the argument itself defines the method to listen on (see examples)

- `ignore_trailing_slash`:

  Logical. Should the trailing slash of a path be ignored when adding
  handlers and handling requests. Setting this will not change the
  request or the path associated with but just ensure that both
  `path/to/resource` and `path/to/resource/` ends up in the same
  handler. Because the request is left untouched, setting this to `TRUE`
  will not affect further processing by other routes

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Pretty printing of the object

#### Usage

    Route$print(...)

#### Arguments

- `...`:

  Ignored

------------------------------------------------------------------------

### Method `add_handler()`

Add a handler to the specified method and path. The special method
`'all'` will allow the handler to match all http request methods. The
path is a URL path consisting of strings, parameters (strings prefixed
with `:`), and wildcards (`*`), separated by `/`. A wildcard will match
anything and is thus not restricted to a single path element (i.e. it
will span multiple `/` if possible). The handler must be a function
containing the arguments `request`, `response`, `keys`, and `...`, and
must return either `TRUE` or `FALSE`. The `request` argument will be a
[reqres::Request](https://reqres.data-imaginist.com/reference/Request.html)
object and the `response` argument will be a
[reqres::Response](https://reqres.data-imaginist.com/reference/Response.html)
object matching the current exchange. The `keys` argument will be a
named list with the value of all matched parameters from the path. Any
additional argument passed on to the `dispatch` method will be avaiable
as well. This method will override an existing handler with the same
method and path.

#### Usage

    Route$add_handler(method, path, handler, reject_missing_methods = FALSE)

#### Arguments

- `method`:

  The http method to match the handler to

- `path`:

  The URL path to match to

- `handler`:

  A handler function

- `reject_missing_methods`:

  Should requests to this path that doesn't have a handler for the
  specific method automatically be rejected with a 405 Method Not
  Allowed response with the correct Allow header informing the client of
  the implemented methods. Assigning a handler to `"all"` for the same
  path at a later point will overwrite this functionality. Be aware that
  setting this to `TRUE` will prevent the request from falling through
  to other routes that might have a matching method and path.

------------------------------------------------------------------------

### Method `remove_handler()`

Removes the handler assigned to the specified method and path. If no
handler have been assigned it will silently ignore it.

#### Usage

    Route$remove_handler(method, path)

#### Arguments

- `method`:

  The http method of the handler to remove

- `path`:

  The URL path of the handler to remove

------------------------------------------------------------------------

### Method `get_handler()`

Returns a handler already assigned to the specified method and path. If
no handler have been assigned it will return NULL.

#### Usage

    Route$get_handler(method, path)

#### Arguments

- `method`:

  The http method of the handler to find

- `path`:

  The URL path of the handler to find

------------------------------------------------------------------------

### Method `remap_handlers()`

Allows you to loop through all added handlers and reassings them at
will. A function with the parameters `method`, `path`, and `handler`
must be provided which is responsible for reassigning the handler given
in the arguments. If the function does not reassign the handler, then
the handler is removed.

#### Usage

    Route$remap_handlers(.f)

#### Arguments

- `.f`:

  A function performing the remapping of each handler

------------------------------------------------------------------------

### Method `merge_route()`

Merge another route into this one, adopting all its handlers. The other
route will be empty after the merge.

#### Usage

    Route$merge_route(route, use_root = TRUE)

#### Arguments

- `route`:

  A Route object

- `use_root`:

  Should the root of `route` be prepended to all paths from the route
  before adding them

------------------------------------------------------------------------

### Method `dispatch()`

Based on a
[reqres::Request](https://reqres.data-imaginist.com/reference/Request.html)
object the route will find the correct handler and call it with the
correct arguments. Anything passed in with `...` will be passed along to
the handler.

#### Usage

    Route$dispatch(request, ..., .require_bool_output = TRUE)

#### Arguments

- `request`:

  The request to route

- `...`:

  Additional arguments to the handlers

- `.require_bool_output`:

  Should the dispatch enforce a boolean output. Mainly for internal use.

------------------------------------------------------------------------

### Method `on_attach()`

Method for use by `fiery` when attached as a plugin. Should not be
called directly. This method creates a RouteStack with the route as the
single route and then mounts that to the app. For more flexibility
create the RouteStack manually

#### Usage

    Route$on_attach(app, on_error = deprecated(), ...)

#### Arguments

- `app`:

  The Fire object to attach the router to

- `on_error`:

  **\[deprecated\]** A function for error handling

- `...`:

  Ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Route$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Initialise an empty route
route <- Route$new()

# Initialise a route with handlers assigned
route <- Route$new(
  all = list(
    '/*' = function(request, response, keys, ...) {
      message('Request received')
      TRUE
    }
  )
)

# Remove it again
route$remove_handler('all', '/*')
```
