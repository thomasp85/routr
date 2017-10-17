#' Combine multiple routes for sequential routing
#'
#' The `RouteStack` class encapsulate multiple [Route]s and lets a request be
#' passed through each sequentially. If a route is returning `FALSE` upon
#' dispatch further dispatching is cancelled.
#'
#' @usage NULL
#' @format NULL
#'
#' @section Initialization:
#' A new 'RouteStack'-object is initialized using the \code{new()} method on the
#' generator:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{route <- RouteStack$new(..., path_extractor = function(msg, bin) '/')}
#' }
#'
#' \strong{Arguments}
#' \tabular{lll}{
#'  \code{...} \tab  \tab Routes to add up front. Must be in the form of named
#'  arguments containing `Route` objects. \cr
#'  \code{path_extractor} \tab  \tab A function that returns a path to dispatch
#'  on from a WebSocket message. Will only be used if
#'  \code{attach_to == 'message'}. Defaults to a function returning `'/'`
#' }
#'
#' @section Field:
#' The following fields are accessible in a `RouteStack` object:
#'
#' \describe{
#'  \item{`attach_to`}{Either `"request"` (default), `"header"`, or `"message"`
#'  that defines which event the router should be attached to when used as a
#'  `fiery` plugin.}
#'  \item{`name`}{The plugin name (used by `fiery`). Will return `'<attach_to>_routr'` (e.g. `'request_routr'` if `attach_to == 'request'`)}
#' }
#'
#' @section Methods:
#' The following methods are accessible in a `RouteStack` object:
#'
#' \describe{
#'  \item{`add_route(route, name, after = NULL)`}{Adds a new route to the stack.
#'  `route` must be a `Route` object, `name` must be a string. If `after` is
#'  given the route will be inserted after the given index, if not (or `NULL`)
#'  it will be inserted in the end of the stack.}
#'  \item{`has_route(name)`}{Test if the routestack contains a route with the
#'  given name.}
#'  \item{`remove(name)`}{Removes the route with the given name from the stack.}
#'  \item{`dispatch(request, ...)`}{Passes a [reqres::Request] through the stack
#'  of routes in sequence until one of the routes return `FALSE` or every route
#'  have been passed through. `...` will be passed on to the dispatch of each
#'  `Route` on the stack.}
#'  \item{`on_error(fun)`}{Set the error handling function. This must be a
#'  function that accepts an `error`, `request`, and `reponse` argument. The
#'  error handler will be called if any of the route handlers throws an error
#'  and can be used to modify the `500` response before it is send back. By
#'  default, the error will be signaled using `message`}
#'  \item{`on_attach(app, on_error = NULL, ...)`}{Method for use by `fiery` when
#'  attached as a plugin. Should not be called directly.}
#' }
#'
#' @section Fiery plugin:
#' A `RouteStack` object is a valid `fiery` plugin and can thus be passed in to
#' the `attach()` method of a `Fire` object. When used as a fiery plugin it is
#' important to be concious for what event it is attached to. By default it will
#' be attached to the `request` event and thus be used to handle HTTP request
#' messaging. An alternative is to attach it to the `header` event that is fired
#' when all headers have been recieved but before the body is. This allows you
#' to short-circuit request handling and e.g. reject requests above a certain
#' size. When the router is attached to the `header` event any handler returning
#' `FALSE` will signal that further handling of the request should be stopped
#' and the response in its current form should be returned without fetching the
#' request body.
#'
#' One last possibility is to attach it to the `message` event and thus use it
#' to handle WebSocket messages. This use case is a bit different from that of
#' `request` and `header`. As `routr` uses `Request` objects as a vessel between
#' routes and WebSocket messages are not HTTP requests, some modification is
#' needed. The way `routr` achieves this is be modifying the HTTP request that
#' established the WebSocket connection and send this through the routes. Using
#' the `path_extractor` function provided in the `RouteStack` constructor it
#' will extract a path to dispatch on and assign it to the request. Furthermore
#' it assigns the message to the body of the request and sets the `Content-Type`
#' header based on whether the message is binary `application/octet-stream` or
#' not `text/plain`. As WebSocket communication is asynchronous the response is
#' ignored when attached to the `message` event. If communication should be send
#' back, use `server$send()` inside the handler(s).
#'
#' How a `RouteStack` is attached is defined by the `attach_to` field which must
#' be either `'request'`, `'header'`, or `'message'`.
#'
#' When attaching the `RouteStack` it is possible to modify how errors are
#' handled, using the `on_error` argument, which will change the error handler
#' set on the `RouteStack`. By default the error handler will be changed to
#' using the `fiery` logging system if the `Fire` object supports it.
#'
#' @seealso [Route] for defining single routes
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.string assert_that has_attr is.count
#' @importFrom reqres as.Request is.Request
#'
#' @export
#'
#' @examples
#' # Create a new stack
#' routes <- RouteStack$new()
#'
#' # Populate it wih routes
#' first <- Route$new()
#' first$add_handler('all', '*', function(request, response, keys, ...) {
#'   message('This will always get called first')
#'   TRUE
#' })
#' second <- Route$new()
#' second$add_handler('get', '/demo/', function(request, response, keys, ...) {
#'   message('This will get called next if the request asks for /demo/')
#'   TRUE
#' })
#' routes$add_route(first, 'first')
#' routes$add_route(second, 'second')
#'
#' # Send a request through
#' rook <- fiery::fake_request('http://example.com/demo/', method = 'get')
#' req <- reqres::Request$new(rook)
#' routes$dispatch(req)
#'
RouteStack <- R6Class('RouteStack',
  public = list(
    # Methods
    initialize = function(..., path_extractor = function(msg, bin) '/') {
      routes <- list(...)
      assert_that(is.function(path_extractor))
      private$path_from_message <- path_extractor
      if (length(routes) > 0) {
        assert_that(has_attr(routes, 'names'))
        lapply(names(routes), function(name) {
          self$add_route(routes[[name]], name)
        })
      }
      private$error_fun <- function(error, request, response) {
        message('routr error: ', conditionMessage(error))
      }
    },
    print = function(...) {
      n_routes <- length(private$stack)
      cat('A RouteStack containing ', n_routes, ' routes\n', sep = '')
      for (i in seq_len(n_routes)) {
        cat(format(i, width = nchar(n_routes)), ': ', private$routeNames[i], '\n', sep = '')
      }
      invisible(self)
    },
    add_route = function(route, name, after = NULL) {
      assert_that(inherits(route, 'Route'))
      assert_that(is.string(name))
      if (is.null(after)) after <- length(private$stack)
      assert_that(after == 0 || is.count(after))
      if (self$has_route(name)) {
        stop('Route named "', name, '" already exists', call. = FALSE)
      }
      private$stack <- append(private$stack, list(route), after)
      private$routeNames <- append(private$routeNames, name, after)
      invisible(self)
    },
    get_route = function(name) {
      if (self$has_route(name)) {
        ind <- match(name, private$routeNames)
        private$stack[[ind]]
      } else {
        stop('No route named ', name, call. = FALSE)
      }
    },
    has_route = function(name) {
      assert_that(is.string(name))
      name %in% private$routeNames
    },
    remove_route = function(name) {
      if (!self$has_route(name)) {
        warning('No route named "', name, '" exists')
      } else {
        ind <- match(name, private$routeNames)
        private$stack <- private$stack[-ind]
        private$routeNames <- private$routeNames[-ind]
      }
      invisible(self)
    },
    dispatch = function(request, ...) {
      if (!is.Request(request)) {
        request <- as.Request(request)
      }
      for (route in private$stack) {
        continue <- tri(route$dispatch(request, ...))
        if (is.error_cond(continue)) {
          response <- request$respond()
          response$status <- 500L
          error <- continue
          private$error_fun(error, request, response)
          continue <- FALSE
        }
        if (!continue) break
      }
      continue
    },
    on_attach = function(app, on_error = NULL, ...) {
      assert_that(inherits(app, 'Fire'))
      if (!is.null(app$log) && is.null(on_error)) {
        self$on_error(function(error, request, response) {
          app$log('error', conditionMessage(error))
        })
      } else if (!is.null(on_error)) {
        self$on_error(on_error)
      }
      if (self$attach_to == 'message') {
        assert_that(!is.null(private$path_from_message))
        app$on('message', function(server, id, binary, message, request, arg_list) {
          rook <- request$origin
          rook$PATH_INFO <- private$path_from_message(message, binary)
          rook$HTTP_Content_Type <- if (binary) 'application/octet-stream' else 'text/plain'
          request <- as.Request(rook)
          request$set_body(message)
          self$dispatch(request, server = server, id = id, arg_list = arg_list)
        })
      } else {
        app$on(self$attach_to, function(server, id, request, arg_list) {
          self$dispatch(request, server = server, id = id, arg_list = arg_list)
        })
      }
    },
    on_error = function(fun) {
      assert_that(is.function(fun))
      assert_that(has_args(fun, c('error', 'request', 'response')))
      private$error_fun <- fun
    }
  ),
  active = list(
    attach_to = function(value) {
      if (missing(value)) return(private$attachAt)
      assert_that(value %in% c('request', 'header', 'message'))
      private$attachAt <- value
    },
    name = function() paste0(self$attach_to, '_routr')
  ),
  private = list(
    # Data
    stack = list(),
    routeNames = character(),
    attachAt = 'request',
    path_from_message = NULL,
    error_fun = NULL
  )
)
