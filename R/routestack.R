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
#'  \code{router <- RouteStack$new(..., path_extractor = function(msg, bin) '/')}
#' }
#'
#' @section Fiery plugin:
#' A `RouteStack` object is a valid `fiery` plugin and can thus be passed in to
#' the `attach()` method of a `Fire` object. When used as a fiery plugin it is
#' important to be concious for what event it is attached to. By default it will
#' be attached to the `request` event and thus be used to handle HTTP request
#' messaging. An alternative is to attach it to the `header` event that is fired
#' when all headers have been received but before the body is. This allows you
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
    #' @description Create a new RouteStack
    #' @param ... Routes to add up front. Must be in the form of named arguments
    #' containing `Route` objects.
    #' @param path_extractor A function that returns a path to dispatch on from
    #' a WebSocket message. Will only be used if \code{attach_to == 'message'}.
    #' Defaults to a function returning `'/'`
    #'
    initialize = function(..., path_extractor = function(msg, bin) '/') {
      routes <- list2(...)
      check_function_args(path_extractor, c("msg", "bin"))
      private$path_from_message <- path_extractor
      if (length(routes) > 0) {
        check_named(routes, arg = "...")
        lapply(names(routes), function(name) {
          self$add_route(routes[[name]], name)
        })
      }
      private$redirector <- Redirector$new()
    },
    #' @description Pretty printing of the object
    #' @param ... Ignored
    #'
    print = function(...) {
      n_routes <- length(private$stack) + length(private$assets)
      cli::cli_text('A RouteStack containing {n_routes} route{?s}')
      cli::cli_ol()
      for (i in seq_along(private$assets)) {
        cli::cli_li('{private$assetNames[i]} (asset route)')
      }
      for (i in seq_along(private$stack)) {
        cli::cli_li('{private$routeNames[i]}')
      }
      invisible(self)
    },
    #' @description Adds a new route to the stack. `route` must be a `Route`
    #' object, `name` must be a string. If `after` is given the route will be
    #' inserted after the given index, if not (or `NULL`) it will be inserted in
    #' the end of the stack.
    #' @param route A `Route` object
    #' @param name The name of the route
    #' @param after The location in the stack to put the route
    #'
    add_route = function(route, name, after = NULL) {
      if (self$has_route(name)) {
        cli::cli_abort('Route with name {.val {name}} already exists')
      }

      if (is.AssetRoute(route)) {
        if (is.null(after)) after <- length(private$assets)
        check_number_whole(after, min = 0)
        private$assets <- append(private$assets, list(route), after)
        private$assetNames <- append(private$assetNames, name, after)
      } else if (inherits(route, "Route")) {
        if (is.null(after)) after <- length(private$stack)
        check_number_whole(after, min = 0)
        private$stack <- append(private$stack, list(route), after)
        private$routeNames <- append(private$routeNames, name, after)
      } else {
        stop_input_type(route, cli::cli_fmt(cli::cli_text("a {.cls Route} or {.cls AssetRoute} object")))
      }

      invisible(self)
    },
    #' @description Adds a permanent (308) or temporary (307) redirect from a
    #' path to another. The paths can contain path arguments and wildcards, but
    #' all those present in `to` must also be present in `from` (the reverse is
    #' not required)
    #' @param method The http method to match the handler to
    #' @param from The path the redirect should respond to
    #' @param to The path the redirect should signal to the client as the new
    #' path
    #' @param permanent Logical. If `TRUE` then a 308 Permanent Redirect is send
    #' back, instructing the client to update the URL in the browser to show the
    #' new path as well as avoid sending requests to the old URL again. If
    #' `FALSE` then a 307 Temporary Redirect is send back, instructing the
    #' client to proceed as if the response comes from the old path
    #'
    add_redirect = function(method, from, to, permanent = TRUE) {
      check_bool(permanent)
      if (permanent) {
        private$redirector$redirect_permanently(method, from, to)
      } else {
        private$redirector$redirect_temporary(method, from, to)
      }
      invisible(self)
    },
    #' @description Get the route with a given name
    #' @param name The name of the route to retrieve
    #'
    get_route = function(name) {
      if (!self$has_route(name)) {
        cli::cli_abort('No route named {.val {name}}')
      }
      ind <- match(name, private$routeNames)
      if (is.na(ind)) {
        ind <- match(name, private$assetNames)
        private$assets[[ind]]
      } else {
        private$stack[[ind]]
      }
    },
    #' @description Test if the routestack contains a route with the given name.
    #' @param name The name of the route to look for
    #'
    has_route = function(name) {
      check_string(name)
      name %in% private$routeNames || name %in% private$assetNames
    },
    #' @description Removes the route with the given name from the stack.
    #' @param name The name of the route to remove
    #'
    remove_route = function(name) {
      if (!self$has_route(name)) {
        cli::cli_warn('No route named {.val {name}} exists')
        return(invisible(self))
      }
      ind <- match(name, private$routeNames)
      if (is.na(ind)) {
        ind <- match(name, private$assetNames)
        private$assets <- private$assets[-ind]
        private$assetNames <- private$assetNames[-ind]
      } else {
        private$stack <- private$stack[-ind]
        private$routeNames <- private$routeNames[-ind]
      }

      invisible(self)
    },
    #' @description asses a [reqres::Request] through the stack of routes in
    #' sequence until one of the routes return `FALSE` or every route have been
    #' passed through. `...` will be passed on to the dispatch of each `Route`
    #' on the stack.
    #' @param request The request to route
    #' @param ... Additional arguments to pass on to the handlers
    dispatch = function(request, ...) {
      if (!is.Request(request)) {
        request <- as.Request(request)
      }

      continue <- private$redirector$dispatch(request, ...)
      if (!isTRUE(continue)) return(FALSE)

      promise <- NULL

      for (route in private$stack) {
        if (is.null(promise)) {
          continue <- route$dispatch(request, ...)
          if (promises::is.promising(continue)) {
            promise <- promises::as.promise(continue)
          } else if (!isTRUE(continue)) {
            break
          }
        } else {
          promise <- promises::then(promise, function(continue) {
            if (isTRUE(continue)) {
              continue <- route$dispatch(request, ...)
              if (promises::is.promising(continue)) {
                continue <- promises::as.promise(continue)
              }
            }
            continue
          })
        }
      }

      if (is.null(promise))  {
        continue
      } else {
        promise
      }
    },
    #' @description Method for use by `fiery` when attached as a plugin. Should
    #' not be called directly.
    #' @param app The Fire object to attach the router to
    #' @param on_error `r lifecycle::badge('deprecated')` A function for error handling
    #' @param ... Ignored
    #'
    on_attach = function(app, on_error = deprecated(), ...) {
      if (!inherits(app, "Fire")) {
        stop_input_type(route, cli::cli_fmt(cli::cli_text("a {.cls Fire} object")))
      }
      if (lifecycle::is_present(on_error)) {
        lifecycle::deprecate_soft("0.5.0", "RouteStack$on_attach(on_error)")
      }
      if (!is.null(private$fiery_app)) {
        if (private$fiery_app != app) {
          cli::cli_abort("This RouteStack is already being used as plugin in another fiery app")
        }
        return()
      }
      if (length(private$assets) != 0) {
        for (a in private$assets) {
          app$serve_static(
            at = a$at,
            path = a$path,
            use_index = a$use_index,
            fallthrough = a$fallthrough,
            html_charset = a$html_charset,
            headers = a$headers,
            validation = a$validation
          )
          for (ex in a$except) {
            app$exclude_static(paste0(a$at, ex))
          }
        }
      }
      if (self$attach_to == 'message') {
        check_function(private$path_from_message)
        app$on('message', function(server, id, binary, message, request, arg_list) {
          rook <- request$origin
          rook$PATH_INFO <- private$path_from_message(message, binary)
          rook$HTTP_Content_Type <- if (binary) 'application/octet-stream' else 'text/plain'
          request <- as.Request(rook)
          request$set_body(message)
          self$dispatch(request, server = server, id = id, arg_list = arg_list)
        })
      } else {
        app$on(self$attach_to, function(server, id, request, arg_list = list()) {
          self$dispatch(request, server = server, id = id, arg_list = arg_list)
        })
      }
    },
    #' @description Merge two route stacks together adding all routes from the
    #' other route to this. The other route stack will be empty after this.
    #' @param stack Another RouteStack object to merge into this one
    #'
    merge_stack = function(stack) {
      if (!inherits(stack, "RouteStack")) {
        stop_input_type(stack, cli::cli_fmt(cli::cli_text("a {.cls RouteStack} object")))
      }
      if (length(stack$routes) != 0) {
        current_routes <- self$routes
        for (route in stack$routes) {
          route_name <- make.unique(c(current_routes, route), "_")[length(current_routes) + 1]
          self$add_route(stack$get_route(route), route_name)
          stack$remove_route(route)
        }
      }
    }
  ),
  active = list(
    #' @field attach_to The event this routr should respond to
    attach_to = function(value) {
      if (missing(value)) return(private$attachAt)
      value <- arg_match0(value, c('request', 'header', 'message'))
      private$attachAt <- value
    },
    #' @field name An autogenerated name for the route stack
    name = function() {
      paste0(self$attach_to, '_routr')
    },
    #' @field routes Gices the name of all routes in the stack
    #'
    routes = function() {
      private$routeNames
    },
    #' @field empty Is the route stack empty
    empty = function() {
      length(private$routeNames) == 0
    }
  ),
  private = list(
    # Data
    stack = list(),
    routeNames = character(),
    assets = list(),
    assetNames = character(),
    attachAt = 'request',
    path_from_message = NULL,
    redirector = NULL
  )
)
