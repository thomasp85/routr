#' Create a route for fetching files
#'
#' This function creates a route mapping different paths to files on the server
#' filesystem. Different subpaths can be mapped to different locations on the
#' server so that e.g. `/data/` maps to `/path/to/data/` and `/assets/` maps to
#' `/a/completely/different/path/`. The route support automatic expansion of
#' paths to a default extension or file, using compressed versions of files if
#' the request permits it, and setting the correct headers so that results are
#' cached.
#'
#' The way paths are resolved to a file is, for every mounted location,
#'
#' 1. Check if the path contains the mount point. If not, continue to the next
#' mount point
#' 2. substitute the mount point for the local location in the path
#' 3. if the path ends with `/` add the `default_file` (defaults to `index.html`)
#' 4. see if the file exists along with compressed versions (versions with
#' `.gz`, `.zip`, `.br`, `.zz` appended)
#' 5. if any version exists, chose the prefered encoding based on the
#' `Accept-Encoding` header in the request, and return.
#' 6. if none exists and the path does not specify a file extension, add
#' `default_ext` to the path and repeat 3-4
#' 7. if none exists still and the path does not specify a file extension, add
#' `default_file` to the path and repeat 3-4
#' 8. if none exists still, continue to the next mount point
#'
#' This means that for the path `/data/mtcars`, the following locations will be
#' tested (assuming the `/data/` -> `/path/to/data/` mapping):
#'
#' 1. `/path/to/data/mtcars`, `/path/to/data/mtcars.gz`,
#' `/path/to/data/mtcars.zip`, `/path/to/data/mtcars.br`,
#' `/path/to/data/mtcars.zz`
#' 2. `/path/to/data/mtcars.html`, `/path/to/data/mtcars.html.gz`,
#' `/path/to/data/mtcars.html.zip`, `/path/to/data/mtcars.html.br`,
#' `/path/to/data/mtcars.html.zz`
#' 3. `/path/to/data/mtcars/index.html`, `/path/to/data/mtcars/index.html.gz`,
#' `/path/to/data/mtcars/index.html.zip`, `/path/to/data/mtcars/index.html.br`,
#' `/path/to/data/mtcars/index.html.zz`
#'
#' Assuming the default values of `default_file` and `default_ext`
#'
#' If a file is not found, the route will simply return `TRUE` to hand of
#' control to subsequent routes in the stack, otherwise it will return the
#' logical value in the `continue` argument (defaults to `FALSE`, thus
#' shortcutting any additional routes in the stack).
#'
#' If a file is found the request headers `If-Modified-Since` and
#' `If-None-Match`, will be fetched and, if exist, will be used to determine
#' whether a `304 - Not Modified` response should be send instead of the file.
#' If the file should be send, it will be added to the response along with the
#' following headers:
#'
#' - `Content-Type` based on the extension of the file (without any encoding
#' extensions)
#' - `Content-Encoding` based on the negotiated file encoding
#' - `ETag` based on [digest::digest()] of the last modified date
#' - `Cache-Control` set to `max-age=3600`
#'
#' Furthermore `Content-Length` will be set automatically by `httpuv`
#'
#' Lastly, if found, the finalize function will be called, forwarding the
#' `request`, `response` and `...` from the `dispatch` method.
#'
#' @param ... Named arguments mapping a subpath in the URL to a location on the
#' file system. These mappings will be checked in sequence
#' @param default_file The default file to look for if the path does not map to
#' a file directly (see Details)
#' @param default_ext The default file extension to add to the file if a file
#' cannot be found at the provided path and the path does not have an extension
#' (see Details)
#' @param finalize An optional function to run if a file is found. The function
#' will recieve the request as the first argument, the response as the second,
#' and anything passed on through `...` in the `dispatch` method. Any return
#' value from the function is discarded.
#' @param continue A logical that should be returned if a file is found.
#' Defaults to `FALSE` indicating that the response should be send unmodified.
#'
#' @return Either `TRUE` if no file is found or `continue = TRUE` or `FALSE` if
#' a file is found and `continue = FALSE`
#'
#' @importFrom tools file_ext file_path_as_absolute file_path_sans_ext
#' @importFrom digest digest
#' @importFrom reqres from_http_date to_http_date
#' @export
#'
#' @family Route constructors
#'
#' @examples
#' # Map package files
#' res_route <- ressource_route(
#'   '/package_files/' = system.file(package = 'routr')
#' )
#'
#' rook <- fiery::fake_request('http://example.com/package_files/DESCRIPTION')
#' req <- reqres::Request$new(rook)
#' res_route$dispatch(req)
#' req$response$as_list()
ressource_route <- function(..., default_file = 'index.html', default_ext = 'html', finalize = NULL, continue = FALSE) {
  assert_that(is.flag(continue))
  assert_that(is.null(finalize) || is.function(finalize))
  assert_that(is.string(default_file))
  assert_that(is.string(default_ext))
  default_ext <- sub('^\\.', '', default_ext)
  route <- Route$new()
  mappings <- list(...)
  names(mappings) <- complete_paths(names(mappings))
  mappings[] <- as.list(complete_paths(unlist(mappings)))
  encodings <- c('identity', .gz = 'gzip', .zip = 'compress', .br = 'br', .zz = 'deflate')
  assert_that(has_attr(mappings, 'names'))
  route$add_handler('get', '/*', function(request, response, keys, ...) {
    path <- request$path
    file_extension <- file_ext(path)
    has_ext <- file_extension != ''
    found <- FALSE
    file <- NA
    enc <- NA
    real_file <- NA
    if (grepl('/$', path)) path <- paste0(path, default_file)
    for (i in seq_along(mappings)) {
      mount <- names(mappings)[i]
      if (!grepl(paste0('^', mount), path)) next
      file <- sub(mount, mappings[i], path)
      files <- paste0(file, names(encodings))
      exist <- file.exists(files)
      if (!any(exist) && !has_ext) {
        file <- paste0(file, '.', default_ext)
        files <- paste0(file, names(encodings))
        exist <- file.exists(files)
      }
      if (!any(exist) && !has_ext) {
        file <- paste0(file_path_sans_ext(file), default_file)
        files <- paste0(file, names(encodings))
        exist <- file.exists(files)
      }
      if (!any(exist)) next
      enc <- request$accepts_encoding(encodings[exist])
      real_file <- files[encodings == enc]
      found <- TRUE
      break
    }
    if (found) {
      m_since <- request$get_header('If-Modified-Since')
      m_time <- file.mtime(real_file)
      etag <- request$get_header('If-None-Match')
      new_tag <- digest(m_time)
      if ((!is.null(m_since) && from_http_date(m_since) < m_time) ||
          (!is.null(etag) && etag == new_tag)) {
        response$status_with_text(304L)
      } else {
        response$body <- c(file = file_path_as_absolute(real_file))
        response$type <- file_extension
        response$set_header('Content-Encoding', enc)
        response$set_header('ETag', new_tag)
        response$set_header('Cache-Control', 'max-age=3600')
        response$set_header('Last-Modified', to_http_date(m_time))
        response$timestamp()
        response$status <- 200L
      }
      if (!is.null(finalize)) finalize(request, response, ...)
      continue
    } else {
      TRUE
    }
  })
  route
}

complete_paths <- function(paths) {
  paths <- ifelse(grepl('^/', paths), paths, paste0('/', paths))
  ifelse(grepl('/$', paths), paths, paste0(paths, '/'))
}
