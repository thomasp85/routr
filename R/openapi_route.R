#' Create a route for serving OpenAPI documentation of your server
#'
#' This route facilitates serving the OpenAPI specs for your server, using either
#' [Swagger](https://swagger.io) or [Redoc](https://redocly.com/redoc) as a UI
#' for it. This function does not help you describe your API - you have to
#' provide the description for it yourself.
#'
#' @param spec The path to the json or yaml file describing your OpenAPI spec
#' @param root The point from which you want to serve your UI from
#' @param ui Either `"redoc"` or `"swagger"`, setting which UI to use
#' @param ... Further arguments passed on to the ui functions (e.g.
#' swagger::swagger_spec())
#'
#' @return A [Route] object
#'
#' @export
#'
#' @family Route constructors
#'
openapi_route <- function(spec, root = "__docs__", ui = c("redoc", "swagger"), ...) {
  if (!file.exists(spec)) {
    cli::cli_abort("{.arg spec} must point to an existing file")
  }
  ui <- arg_match(ui)

  ext <- tools::file_ext(spec)
  if (tolower(ext) == "yaml") {
    spec_file <- "openapi.yaml"
    spec_type = "text/yaml"
  } else {
    spec_file <- "openapi.json"
    spec_type = "application/json"
  }

  route <- Route$new()
  route$add_handler("get", paste0("/", spec_file), function(request, response, ...) {
    response$status <- 200L
    response$file <- spec
    response$type <- spec_type
    FALSE
  })

  root_depth <- stringi::stri_count_fixed(gsub("^/|/$", "", root), "/") + 1L
  rel_spec <- paste0(paste0(rep("..", root_depth), collapse = "/"), "/", spec_file)
  if (ui == "swagger") {
    check_installed("swagger")
    path <- swagger::swagger_path(...)
    index <- swagger::swagger_spec(paste0('"', rel_spec, '"'), ...)
  } else if (ui == "redoc") {
    check_installed("redoc")
    path <- redoc::redoc_path()
    index <- redoc::redoc_spec(rel_spec, ...)
  }

  for (endpoint in c("", "/", "/index.html")) {
    index_path <- paste0(sub("/$", "", root), endpoint)
    route$add_handler("get", index_path, function(request, response, ...) {
      response$status <- 200L
      response$body <- index
      response$type <- "text/html"
      FALSE
    })
  }

  assets <- ressource_route(!!root := path, finalize = function(req, res, ...) {
    res$set_header("Access-Control-Allow-Origin", "*")
  })

  route$merge_route(assets)
  route
}
