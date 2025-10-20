#' Create a route for serving OpenAPI documentation of your server
#'
#' This route facilitates serving the OpenAPI specs for your server, using
#' either [RapiDoc](https://rapidocweb.com), [Redoc](https://redocly.com/redoc)
#' or [Swagger](https://swagger.io) as a UI for it. This function does not help
#' you describe your API - you have to provide the description for it yourself.
#'
#' @param spec The path to the json or yaml file describing your OpenAPI spec
#' @param root The point from which you want to serve your UI from
#' @param ui Either `"rapidoc"`, `"redoc"` or `"swagger"`, setting which UI to
#' use
#' @param ... Further arguments passed on to the ui functions (e.g.
#' rapidoc::rapidoc_spec())
#'
#' @return A [Route] object
#'
#' @export
#'
#' @family Route constructors
#'
openapi_route <- function(
  spec,
  root = "__docs__",
  ui = c("rapidoc", "redoc", "swagger"),
  ...
) {
  if (!fs::file_exists(spec)) {
    cli::cli_abort("{.arg spec} must point to an existing file")
  }
  ui <- arg_match(ui)

  ext <- tolower(fs::path_ext(spec))
  if (ext == "yaml") {
    check_installed("yaml")
    spec_file <- "openapi.yaml"
    spec_type = "text/yaml"
    spec <- yaml::read_yaml(spec)
  } else {
    check_installed("jsonlite")
    spec_file <- "openapi.json"
    spec_type = "application/json"
    spec <- jsonlite::read_json(spec)
  }

  root_depth <- stringi::stri_count_fixed(gsub("^/|/$", "", root), "/") + 1L
  root_rel <- paste0(rep("..", root_depth), collapse = "/")

  route <- Route$new()
  route$add_handler(
    "get",
    paste0("/", spec_file),
    function(request, response, ...) {
      referer <- request$headers$referer
      if (
        !is.null(referer) &&
          (is.null(spec$servers) ||
            identical(spec$servers, list(list(url = ""))))
      ) {
        referer <- strsplit(referer, "://", fixed  = TRUE)[[1]]
        referer[2] <- paste0(fs::path_norm(fs::path(referer[2], root_rel)), "/")
        spec$servers <- list(list(
          url = paste0(referer, collapse = "://")
        ))
      }
      response$status <- 200L
      response$body <- if (ext == "yaml") {
        yaml::as.yaml(spec)
      } else {
        jsonlite::toJSON(spec, auto_unbox = TRUE)
      }
      response$type <- spec_type
      FALSE
    }
  )

  rel_spec <- paste0(root_rel, "/", spec_file)
  rel_spec <- paste0(
    "(new URL(\"",
    rel_spec,
    "\", window.location.origin + window.location.pathname)).toString()"
  )
  if (ui == "rapidoc") {
    check_installed("rapidoc")
    path <- rapidoc::rapidoc_path()
    index <- rapidoc::rapidoc_spec(paste0('" + ', rel_spec, ' + "'), ...)
  } else if (ui == "swagger") {
    check_installed("swagger")
    path <- swagger::swagger_path(...)
    index <- swagger::swagger_spec(rel_spec, ...)
  } else if (ui == "redoc") {
    check_installed("redoc")
    path <- redoc::redoc_path()
    index <- redoc::redoc_spec(paste0("' + ", rel_spec, " + '"), ...)
  }

  for (endpoint in c("/", "/index.html")) {
    index_path <- paste0(sub("/$", "", root), endpoint)
    route$add_handler("get", index_path, function(request, response, ...) {
      response$status <- 200L
      response$body <- index
      response$type <- "text/html"
      FALSE
    })
  }
  route$add_handler(
    "get",
    sub("/$", "", root),
    function(request, response, ...) {
      response$status <- 308L
      response$set_header("Location", sub("/?$", "/", root))
      FALSE
    }
  )

  assets <- resource_route(!!root := path, finalize = function(req, res, ...) {
    res$set_header("Access-Control-Allow-Origin", "*")
  })

  route$merge_route(assets)
  route
}
