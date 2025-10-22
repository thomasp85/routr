#' High performance route for serving static files
#'
#' An `asset_route()` is fundamentally different than the other routes provided
#' by routr. Conceptually it is akin to [resource_route()] in that it is used
#' for serving static file content, but this route circumvents the standard
#' dispatch entirely (the request never enters the R process). This makes it
#' extremely fast but also somewhat limited as you can't pass the request
#' through any middleware. The choice between `asset_route()` and
#' [resource_route()] thus depends on your needs.
#'
#' @param at The url path to listen to requests on
#' @param path The path to the file or directory on the file system
#' @param use_index Should an `index.html` file be served if present when a
#' client requests the folder
#' @param fallthrough Should requests that doesn't match a file enter the
#' request loop or have a 404 response send directly
#' @param html_charset The charset to report when serving html files
#' @param headers A list of headers to add to the response. Will be combined
#' with the global headers of the app
#' @param except One or more url paths that should be excluded from the route.
#' Requests matching these will enter the standard router dispatch. The paths
#' are interpreted as subpaths to `at`, e.g. the final path to exclude will be
#' `at`+`exclude` (see example)
#' @inheritParams httpuv::staticPath
#'
#' @return An [AssetRoute] object
#'
#' @family Route constructors
#'
#' @export
#'
#' @examples
#' asset_route("/wd", "./", except = "/private")
#'
#'
asset_route <- function(
  at,
  path,
  use_index = TRUE,
  fallthrough = FALSE,
  html_charset = "utf-8",
  headers = list(),
  validation = NULL,
  except = NULL
) {
  AssetRoute$new(
    at = at,
    path = path,
    use_index = use_index,
    fallthrough = fallthrough,
    html_charset = html_charset,
    headers = headers,
    validation = validation,
    except = except
  )
}

#' Static file serving
#'
#' @description
#' A class for serving files from the server directly. The `AssetRoute` is
#' fundamentally different than the other routes provided by routr. It is
#' specific to httpuv and circumvents the standard dispatch entirely (the
#' request never enters the R process). This makes it extremely fast but also
#' somewhat limited as you can't pass the request through any middleware.
#'
#' @export
AssetRoute <- R6Class(
  "AssetRoute",
  public = list(
    # Methods
    #' @description Create a new AssetRoute
    #' @param at The url path to listen to requests on
    #' @param path The path to the file or directory on the file system
    #' @param use_index Should an `index.html` file be served if present when a
    #' client requests the folder
    #' @param fallthrough Should requests that doesn't match a file enter the
    #' request loop or have a 404 response send directly
    #' @param html_charset The charset to report when serving html files
    #' @param headers A list of headers to add to the response. Will be combined
    #' with the global headers of the app
    #' @param validation A string for validating incoming requests. See
    #' [httpuv::staticPath]
    #' @param except One or more url paths that should be excluded from the
    #' route. Requests matching these will enter the standard router dispatch.
    #' The paths are interpreted as subpaths to `at`, e.g. the final path to
    #' exclude will be `at`+`exclude`
    initialize = function(
      at,
      path,
      use_index = TRUE,
      fallthrough = FALSE,
      html_charset = "utf-8",
      headers = list(),
      validation = NULL,
      except = NULL
    ) {
      check_string(at)
      check_string(path)
      if (!fs::file_exists(path)) {
        cli::cli_abort(
          "{.arg {path}} does not point to an existing file or directory"
        )
      }
      check_bool(use_index)
      check_bool(fallthrough)
      check_string(html_charset)
      check_named(headers)
      for (i in names(headers)) {
        check_string(headers[[i]], arg = paste0("headers", "[[", i, "]]"))
      }
      check_string(validation, allow_null = TRUE)
      check_character(except, allow_na = FALSE, allow_null = TRUE)

      private$AT <- at
      private$PATH <- path
      private$USE_INDEX <- use_index
      private$FALLTHROUGH <- fallthrough
      private$HTML_CHARSET <- html_charset
      private$HEADERS <- headers
      private$VALIDATION <- validation
      private$EXCEPT <- except %||% character(0)
    },
    #' @description Pretty printing of the object
    #' @param ... Ignored
    #'
    print = function(...) {
      cli::cli_text(
        "A route mapping files from {.file {private$PATH}} to {.field {private$AT}} {cli::qty(private$EXCEPT)} {?/excluding/excluding} {.field {paste0(private$AT, private$EXCEPT)}}"
      )
      cli::cli_h3("Settings:")
      cli::cli_dl()
      cli::cli_li(c(use_index = private$USE_INDEX))
      cli::cli_li(c(fallthrough = private$FALLTHROUGH))
      cli::cli_li(c(html_charset = private$HTML_CHARSET))
      if (length(private$HEADERS) > 0) {
        cli::cli_li(c(headers = ""))
        id <- cli::cli_ul()
        for (h in names(private$HEADERS)) {
          cli::cli_li("{h}: {private$HEADERS[h]}")
        }
        cli::cli_end(id)
      } else {
        cli::cli_li(c(headers = "<none>"))
      }
      cli::cli_li(c(validation = private$VALIDATION %||% "<none>"))
    },
    #' @description Method for use by `fiery` when attached as a plugin. Should
    #' not be called directly. This method creates a RouteStack with the asset
    #' route as the single route and then mounts that to the app. For more
    #' flexibility create the RouteStack manually
    #' @param app The Fire object to attach the router to
    #' @param on_error `r lifecycle::badge('deprecated')` A function for error handling
    #' @param ... Ignored
    #'
    on_attach = function(app, on_error = deprecated(), ...) {
      if (lifecycle::is_present(on_error)) {
        lifecycle::deprecate_soft("0.5.0", "AssetRoute$on_attach(on_error)")
      }
      RouteStack$new(asset = self)$on_attach(
        app = app,
        ...
      )
    }
  ),
  active = list(
    #' @field at The url path to serve the assets on
    at = function(value) {
      if (missing(value)) {
        return(private$AT)
      }
      check_string(value)
      private$AT <- value
      invisible(NULL)
    },
    #' @field path The path to the file or directory to serve
    path = function(value) {
      if (missing(value)) {
        return(private$PATH)
      }
      check_string(value)
      if (!fs::file_exists(value)) {
        cli::cli_abort(
          "{.arg {value}} does not point to an existing file or directory"
        )
      }
      private$PATH <- value
      invisible(NULL)
    },
    #' @field use_index Should an `index.html` file be served if present when a client requests the folder
    use_index = function(value) {
      if (missing(value)) {
        return(private$USE_INDEX)
      }
      check_bool(value)
      private$USE_INDEX <- value
      invisible(NULL)
    },
    #' @field fallthrough Should requests that doesn't match a file enter the request loop or have a 404 response send directly
    fallthrough = function(value) {
      if (missing(value)) {
        return(private$FALLTHROUGH)
      }
      check_bool(value)
      private$FALLTHROUGH <- value
      invisible(NULL)
    },
    #' @field html_charset The charset to report when serving html files
    html_charset = function(value) {
      if (missing(value)) {
        return(private$HTML_CHARSET)
      }
      check_string(value)
      private$HTML_CHARSET <- value
      invisible(NULL)
    },
    #' @field headers A list of headers to add to the response.
    headers = function(value) {
      if (missing(value)) {
        return(private$HEADERS)
      }
      check_named(value)
      for (i in names(value)) {
        check_string(value[[i]], arg = paste0("headers", "[[", i, "]]"))
      }
      private$HEADERS <- value
      invisible(NULL)
    },
    #' @field validation An optional validation pattern to compare to the request headers
    validation = function(value) {
      if (missing(value)) {
        return(private$VALIDATION)
      }
      check_string(value, allow_null = TRUE)
      private$VALIDATION <- value
      invisible(NULL)
    },
    #' @field except One or more url paths that should be excluded from this route
    except = function(value) {
      if (missing(value)) {
        return(private$EXCEPT)
      }
      check_character(value, allow_na = FALSE, allow_null = TRUE)
      private$EXCEPT <- value %||% character(0)
      invisible(NULL)
    },
    #' @field name An autogenerated name for the asset route
    name = function() paste0("asset_routr")
  ),
  private = list(
    AT = character(),
    PATH = character(),
    USE_INDEX = TRUE,
    FALLTHROUGH = FALSE,
    HTML_CHARSET = "utf-8",
    HEADERS = list(),
    VALIDATION = NULL,
    EXCEPT = character(0)
  )
)

is.AssetRoute <- function(x) inherits(x, "AssetRoute")
