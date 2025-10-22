#' Single route dispatch
#'
#' @description
#' Class for handling a single route dispatch
#'
#' @details
#' The `Route` class is used to encapsulate a single URL dispatch, that is,
#' chose a single handler from a range based on a URL path. A handler will be
#' called with a request, response, and keys argument as well as any additional
#' arguments passed on to `dispatch()`.
#'
#' The path will strip the query string prior to assignment of the handler, can
#' contain wildcards, and can be parameterised using the `:` prefix. If there
#' are multiple matches of the request path the most specific will be chosen.
#' Specificity is based on number of elements (most), number of parameters
#' (least), and number of wildcards (least), in that order. Parameter
#' values will be available in the keys argument passed to the handler, e.g. a
#' path of `/user/:user_id` will provide `list(user_id = 123)` for a dispatch on
#' `/user/123` in the `keys` argument.
#'
#' Handlers are only called for their side-effects and are expected to return
#' either `TRUE` or `FALSE` indicating whether additional routes in a
#' [`RouteStack`] should be called, e.g. if a handler is returning `FALSE` all
#' further processing of the request will be terminated and the response will be
#' passed along in its current state. Thus, the intend of the handlers is to
#' modify the request and response objects, in place. All calls to handlers will
#' be wrapped in [try()] and if an exception is raised the response code will be
#' set to `500` with the body of the response being the error message. Further
#' processing of the request will be terminated. If a different error handling
#' scheme is wanted it must be implemented within the handler (the standard
#' approach is chosen to avoid handler errors resulting in a server crash).
#'
#' A handler is referencing a specific HTTP method (`get`, `post`, etc.) but can
#' also reference `all` to indicate that it should match all types of requests.
#' Handlers referencing `all` have lower precedence than those referencing
#' specific methods, so will only be called if a match is not found within the
#' handlers of the specific method.
#'
#' @usage NULL
#' @format NULL
#'
#' @section Initialization:
#' A new 'Route'-object is initialized using the \code{new()} method on the
#' generator or alternatively by using [route()]:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{route <- Route$new(...)}
#' }
#' \tabular{l}{
#'  \code{route <- route(...)}
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom reqres is.Request
#' @importFrom stringi stri_match_first
#'
#' @export
#' @rdname Route-class
#'
#' @seealso [RouteStack] for binding multiple routes sequentially
#'
#' @examples
#' # Initialise an empty route
#' route <- Route$new()
#'
#' # Initialise a route with handlers assigned
#' route <- Route$new(
#'   all = list(
#'     '/*' = function(request, response, keys, ...) {
#'       message('Request received')
#'       TRUE
#'     }
#'   )
#' )
#'
#' # Remove it again
#' route$remove_handler('all', '/*')
#'
Route <- R6Class(
  'Route',
  public = list(
    # Methods
    #' @description Create a new Route
    #' @param ... Handlers to add up front. Must be in the form of named lists
    #' where the names corresponds to paths and the elements are the handlers.
    #' The name of the argument itself defines the method to listen on (see
    #' examples)
    #' @param ignore_trailing_slash Logical. Should the trailing slash of a path
    #' be ignored when adding handlers and handling requests. Setting this will
    #' not change the request or the path associated with but just ensure that
    #' both `path/to/resource` and `path/to/resource/` ends up in the same
    #' handler. Because the request is left untouched, setting this to `TRUE`
    #' will not affect further processing by other routes
    #'
    initialize = function(..., ignore_trailing_slash = FALSE) {
      check_bool(ignore_trailing_slash)
      private$ignore_trailing_slash = ignore_trailing_slash
      handlers <- list2(...)
      if (length(handlers) == 0) {
        return()
      }
      check_named(handlers, arg = "...")
      call = current_call()
      lapply(names(handlers), function(method) {
        check_named(handlers[[method]], arg = method, call = call)
        lapply(names(handlers[[method]]), function(path) {
          check_function(
            handlers[[method]][[path]],
            arg = paste0(method, "[[", path, "]]"),
            call = call
          )
          self$add_handler(method, path, handlers[[method]][[path]])
        })
      })
    },
    #' @description Pretty printing of the object
    #' @param ... Ignored
    #'
    print = function(...) {
      n_handlers <- sum(lengths(private$handlerMap))
      cli::cli_text('A route with {n_handlers} handler{?s}')
      if (n_handlers != 0) {
        method_order <- c(http_methods, 'all')
        reg_methods <- names(private$handlerMap)
        map_order <- match(reg_methods, method_order)
        map_order[is.na(map_order)] <- sum(!is.na(map_order)) +
          seq_len(sum(is.na(map_order)))
        method_length <- max(nchar(reg_methods))
        for (i in order(map_order)) {
          paths <- names(private$handlerMap[[reg_methods[i]]])
          cli::cli_text('{.emph {reg_methods[i]}:}')
          id <- cli::cli_ul()
          for (j in seq_along(paths)) {
            cli::cli_li(paths[j])
          }
          cli::cli_end(id)
        }
      }
      return(invisible(self))
    },
    #' @description Add a handler to the specified method and path. The special
    #' method `'all'` will allow the handler to match all http request methods.
    #' The path is a URL path consisting of strings, parameters (strings
    #' prefixed with `:`), and wildcards (`*`), separated by `/`. A wildcard
    #' will match anything and is thus not restricted to a single path element
    #' (i.e. it will span multiple `/` if possible). The handler must be a
    #' function containing the arguments `request`, `response`, `keys`, and
    #' `...`, and must return either `TRUE` or `FALSE`. The `request` argument
    #' will be a [reqres::Request] object and the `response` argument will be a
    #' [reqres::Response] object matching the current exchange. The `keys`
    #' argument will be a named list with the value of all matched parameters
    #' from the path. Any additional argument passed on to the `dispatch` method
    #' will be avaiable as well. This method will override an existing handler
    #' with the same method and path.
    #' @param method The http method to match the handler to
    #' @param path The URL path to match to
    #' @param handler A handler function
    #' @param reject_missing_methods Should requests to this path that doesn't
    #' have a handler for the specific method automatically be rejected with a
    #' 405 Method Not Allowed response with the correct Allow header informing
    #' the client of the implemented methods. Assigning a handler to `"all"` for
    #' the same path at a later point will overwrite this functionality. Be
    #' aware that setting this to `TRUE` will prevent the request from falling
    #' through to other routes that might have a matching method and path.
    #'
    add_handler = function(
      method,
      path,
      handler,
      reject_missing_methods = FALSE
    ) {
      method <- arg_match0(tolower(method), c(http_methods, "all"))
      check_string(path)
      check_bool(reject_missing_methods)
      path <- sub('\\?.+', '', path)
      check_function_args(handler, '...')
      path <- private$canonical_path(path)

      private$assign_handler(method, path, handler)

      if (
        reject_missing_methods &&
          method != "all" &&
          is.null(private$handlerMap[["all"]][[path]])
      ) {
        self$add_handler("all", path, function(response, ...) {
          current_methods <- http_methods[vapply(
            http_methods,
            function(method) {
              !is.null(private$handlerMap[["all"]][[path]], logical(1))
            }
          )]
          response$status <- 405L
          response$set_header(
            "Allow",
            paste0(toupper(current_methods), collapse = ", ")
          )
          FALSE
        })
      }
      invisible(self)
    },
    #' @description Removes the handler assigned to the specified method and
    #' path. If no handler have been assigned it will silently ignore it.
    #' @param method The http method of the handler to remove
    #' @param path The URL path of the handler to remove
    #'
    remove_handler = function(method, path) {
      method <- arg_match0(tolower(method), c(http_methods, "all"))
      check_string(path)
      path <- private$canonical_path(path)
      private$handlerMap[[method]][[path]] <- NULL
      invisible(self)
    },
    #' @description Returns a handler already assigned to the specified method
    #' and path. If no handler have been assigned it will return NULL.
    #' @param method The http method of the handler to find
    #' @param path The URL path of the handler to find
    #'
    get_handler = function(method, path) {
      method <- arg_match0(tolower(method), c(http_methods, "all"))
      check_string(path)
      path <- private$canonical_path(path)
      private$handlerMap[[method]][[path]]$handler
    },
    #' @description Allows you to loop through all added handlers and reassings
    #' them at will. A function with the parameters `method`, `path`, and
    #' `handler` must be provided which is responsible for reassigning the
    #' handler given in the arguments. If the function does not reassign the
    #' handler, then the handler is removed.
    #' @param .f A function performing the remapping of each handler
    #'
    remap_handlers = function(.f) {
      check_function_args(.f, c('method', 'path', 'handler'))
      old_map <- private$handlerMap
      private$handlerMap <- list()

      lapply(names(old_map), function(method) {
        lapply(names(old_map[[method]]), function(path) {
          .f(
            method = method,
            path = path,
            handler = old_map[[method]][[path]]$handler
          )
        })
      })
      invisible(self)
    },
    #' @description Merge another route into this one, adopting all its handlers.
    #' The other route will be empty after the merge.
    #' @param route A Route object
    #' @param use_root Should the root of `route` be prepended to all paths from
    #' the route before adding them
    #'
    merge_route = function(route, use_root = TRUE) {
      if (!inherits(route, "Route")) {
        stop_input_type(
          route,
          cli::cli_fmt(cli::cli_text("a {.cls Route} object"))
        )
      }
      route$remap_handlers(function(method, path, handler) {
        if (use_root) {
          path <- paste0(gsub("^\\^|/$", "", route$root), path)
        }
        self$add_handler(method, path, handler)
      })
      invisible(self)
    },
    #' @description Based on a [reqres::Request] object the route will find the
    #' correct handler and call it with the correct arguments. Anything passed
    #' in with `...` will be passed along to the handler.
    #' @param request The request to route
    #' @param ... Additional arguments to the handlers
    #' @param .require_bool_output Should the dispatch enforce a boolean output.
    #' Mainly for internal use.
    #'
    dispatch = function(request, ..., .require_bool_output = TRUE) {
      if (self$empty) {
        return(NOMATCH)
      }

      if (!is.Request(request)) {
        stop_input_type(
          request,
          cli::cli_fmt(cli::cli_text("a {.cls Request} object"))
        )
      }

      if (!grepl(self$root, request$path)) {
        return(NOMATCH)
      }

      response <- request$respond()

      method <- request$method
      path <- private$canonical_path(request$path)
      handlerInfo <- private$match_url(path, method)
      if (is.null(handlerInfo)) {
        handlerInfo <- private$match_url(path, 'all')
        if (is.null(handlerInfo)) return(NOMATCH)
      }
      handler <- handlerInfo$handler
      keys <- set_names(handlerInfo$values, handlerInfo$keys)

      with_route_ospan(
        {
          handler(
            request = request,
            response = response,
            keys = keys,
            ...
          )
        },
        handlerInfo = handlerInfo,
        method = method,
        request = request,
        response = response,
        keys = keys,
        check_output = .require_bool_output
      )
    },
    #' @description Method for use by `fiery` when attached as a plugin. Should
    #' not be called directly. This method creates a RouteStack with the route
    #' as the single route and then mounts that to the app. For more flexibility
    #' create the RouteStack manually
    #' @param app The Fire object to attach the router to
    #' @param on_error `r lifecycle::badge('deprecated')` A function for error handling
    #' @param ... Ignored
    #'
    on_attach = function(app, on_error = deprecated(), ...) {
      if (lifecycle::is_present(on_error)) {
        lifecycle::deprecate_soft("0.5.0", "Route$on_attach(on_error)")
      }
      RouteStack$new(route = self)$on_attach(
        app = app,
        ...
      )
    }
  ),
  active = list(
    #' @field root The root of the route. Will be removed from the path of any
    #' request before matching a handler
    root = function(value) {
      if (missing(value)) {
        return(private$ROOT)
      }
      check_string(value)
      private$ROOT <- paste0('/', gsub('(^/)|(/$)', '', value))
      private$update_regexes()
    },
    #' @field name An autogenerated name for the route
    name = function() paste0("single_routr"),
    #' @field empty Is the route empty
    empty = function() {
      sum(lengths(private$handlerMap)) == 0
    }
  ),
  private = list(
    # Data
    handlerMap = list(),
    regexes = list(),
    ROOT = '/',
    ignore_trailing_slash = FALSE,
    # Methods
    assign_handler = function(method, path, handler) {
      method <- tolower(method)
      if (is.null(private$handlerMap[[method]][[path]])) {
        private$handlerMap[[method]][[path]] <- private$path_to_regex(path)
      }
      private$handlerMap[[method]][[path]]$handler <- handler
      private$sort_handlers(method)
    },
    sort_handlers = function(method) {
      n_tokens <- sapply(private$handlerMap[[method]], `[[`, 'n_tokens')
      n_keys <- sapply(private$handlerMap[[method]], `[[`, 'n_keys')
      n_wildcard <- sapply(private$handlerMap[[method]], `[[`, 'n_wildcard')
      sort_order <- order(
        n_wildcard + n_keys == 0,
        n_tokens,
        -n_wildcard,
        -n_keys,
        decreasing = TRUE
      )
      private$handlerMap[[method]] <- private$handlerMap[[method]][sort_order]
      private$update_regexes(method)
    },
    path_to_regex = function(path) {
      path <- sub('^/', '', path)
      terminator <- if (grepl('/$', path)) '/' else ''
      path <- sub('/$', '', path)
      tokens <- strsplit(path, '/')[[1]]
      n_tokens <- length(tokens)
      keys <- grep('^:', tokens)
      wildcard <- which(tokens == '*')
      reg <- tokens
      reg[keys] <- '([^\\/]+?)'
      reg[wildcard] <- '(.*)'
      reg <- paste0(paste0(reg, collapse = '/'), terminator)
      list(
        path = path,
        regex = reg,
        n_tokens = n_tokens,
        n_keys = length(keys),
        n_wildcard = length(wildcard),
        keys = make.unique(sub('^:', '', tokens[sort(c(keys, wildcard))]), "_")
      )
    },
    update_regexes = function(method = NULL) {
      if (is.null(method)) {
        for (m in names(private$handlerMap)) {
          private$update_regexes(m)
        }
        return()
      }
      regexes <- vapply(
        private$handlerMap[[method]],
        `[[`,
        character(1),
        i = 'regex'
      )
      regexes <- paste0(self$root, regexes)
      need_regex <- vapply(
        private$handlerMap[[method]],
        function(p) p$n_keys + p$n_wildcard != 0,
        logical(1)
      )
      private$regexes[[method]] <- list(
        fixed = regexes[!need_regex],
        regex = if (any(need_regex)) paste0("^", regexes[need_regex], "$")
      )
    },
    match_url = function(url, method) {
      if (length(private$handlerMap[[method]]) == 0) {
        return(NULL)
      }
      i <- which(url == private$regexes[[method]]$fixed)[1]
      url_match <- NA
      if (is.na(i)) {
        for (i in seq_along(private$regexes[[method]]$regex)) {
          url_match <- stri_match_first(
            url,
            regex = private$regexes[[method]]$regex[i],
            case_insensitive = TRUE
          )[1, ]
          if (!is.na(url_match[1])) {
            i <- i + length(private$regexes[[method]]$fixed)
            break
          }
        }
      } else {
        url_match <- url
      }
      if (!is.na(url_match[1])) {
        handlerInfo <- private$handlerMap[[method]][[i]]
        handlerInfo$values <- as.list(url_match[-1])
        handlerInfo
      } else {
        NULL
      }
    },
    canonical_path = function(path) {
      if (private$ignore_trailing_slash && path != "/") {
        sub("/$", "", path)
      } else {
        path
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
