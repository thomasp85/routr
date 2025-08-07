#' Create a route that renders and serves an Rmarkdown or Quarto report
#'
#' This route allows you to serve a report written as a Quarto/Rmarkdown
#' document. The report will be rendered on demand using the query params as
#' parameters for the report if they match. Depending on the value of the value
#' of `max_age` the rendered report is kept and served without a re-render on
#' subsequent requests. The rendering can happen asynchronously in which case
#' a promise is returned.
#'
#' @details
#' Only the formats explicitely stated in the header of the report are allowed
#' and the will be selected through content negotiation. That means that if
#' multiple formats produces the same file type, only the first will be
#' available. If no format is specified the default for both Quarto and
#' Rmarkdown documents is HTML
#'
#' @param path The url path to serve the report from
#' @param file The quarto or rmarkdown file to use for rendering of the report
#' @param ... Further arguments to `quarto::quarto_render()` or
#' `rmarkdown::render()`
#' @param max_age The maximum age in seconds to keep a rendered report before
#' initiating a re-render
#' @param async Should rendering happen asynchronously (using mirai)
#' @param finalize An optional function to run before sending the response back.
#' The function will receive the request as the first argument, the response as
#' the second, and anything passed on through `...` in the `dispatch` method.
#' Any return value from the function is discarded. The function must accept
#' `...`
#' @param continue A logical that defines whether the response is returned
#' directly after rendering or should be made available to subsequent routes
#' @param ignore_trailing_slash Should `path` be taken exactly or should both a
#' version with and without a terminating slash be accepted
#'
#' @return A [route] object
#'
#' @export
#'
report_route <- function(path, file, ..., max_age = Inf, async = TRUE, finalize = NULL, continue = FALSE, ignore_trailing_slash = FALSE) {
  if (!fs::file_exists(file)) {
    cli::cli_abort("{.arg file} does not point to an existing file")
  }
  check_number_whole(max_age, min = 0, allow_infinite = TRUE)
  check_bool(continue)
  check_function(finalize, allow_null = TRUE)
  if (!is.null(finalize) && !"..." %in% fn_fmls_names(finalize)) {
    cli::cli_abort("{.arg finalize} must be a function taking {.arg ...} as argument")
  }
  check_bool(async)
  if (async) {
    check_installed("mirai")
  }

  is_quarto <- grepl("\\.qmd$", file, ignore.case = TRUE)
  render_params <- list2(input = file, quiet = TRUE, ...)
  if (is_quarto) {
    check_installed("quarto")
    info <- quarto_info(file)
    if (is.null(render_params$execute_dir)) {
      render_params$execute_dir <- fs::path_dir(file)
    }
    render_params$metadata <- list("embed-resources" = TRUE)
  } else {
    check_installed("rmarkdown")
    info <- rmarkdown_info(file)
  }

  cache_dir <- tempfile(pattern = "routr_report")
  fs::dir_create(cache_dir)

  info$accepts <- unlist(format_info$mime_render_types[info$formats], use.names = FALSE)
  info$ext <- unlist(format_info$mime_render_ext[info$formats], use.names = FALSE)
  keep <- !duplicated(info$accepts)
  info$accepts <- info$accepts[keep]
  info$ext <- info$ext[keep]
  info$formats <- info$formats[keep]

  route <- Route$new()
  route$add_handler("get", path, function(request, response, keys, ...) {
    if (length(info$accepts) > 1) {
      response$append_header('Vary', 'Accept')
    }
    type <- request$accepts(info$accepts)
    if (is.null(type)) {
      reqres::abort_not_acceptable("The report does not provide a format producing the requested mime type")
    }
    which_type <- match(type, info$accepts)
    format <- info$formats[which_type]
    ext <- info$ext[which_type]
    response$status <- 307L
    new_loc <- sub("/?$", paste0(".", ext), path)
    if (grepl("/$", path)) {
      new_loc <- paste0("../", sub("^/", "", new_loc))
    }
    response$set_header("location", paste0(new_loc, request$querystring))

    return(FALSE)
  })
  lapply(seq_along(info$ext), function(i) {
    direct_path <- sub("/?$", paste0(".", info$ext[i]), path)
    type <- info$accepts[i]
    ext <- info$ext[i]
    render_params$output_format <- info$formats[i]
    route$add_handler("get", direct_path, function(request, response, keys, ...) {
      report_params <- request$query[intersect(names(request$query), info$params)]
      if (length(report_params) > 0) {
        report_params <- report_params[order(names(report_params))]
      }
      param_hash <- hash(report_params)
      render_path <- fs::path(cache_dir, param_hash, ext = ext)

      link_sub <- paste0(fs::path_file(path), "\\1", request$querystring)
      link_pattern <- paste0(param_hash, "(\\.\\w+)")

      if (!fs::file_exists(render_path) || as.numeric(Sys.time() - fs::file_info(render_path)$modification_time, units = "secs") > max_age) {
        if (async) {
          env <- list2env(list(
            is_quarto = is_quarto,
            render_path = render_path,
            report_params = report_params,
            render_params = render_params,
            type = type,
            link_sub = link_sub,
            link_pattern = link_pattern
          ))
          promise <- mirai::mirai(render_expr, env)
          promise <- promises::then(
            promise,
            function(result) {
              if (!fs::file_exists(render_path)) {
                reqres::abort_internal_error("Failed to render report")
              }
              response$status <- 200L
              response$file <- render_path
              response$type <- type
              if (!is.null(finalize)) {
                finalize(request, response, ...)
              }
              continue
            },
            function(error) {
              reqres::abort_internal_error("Failed to render report", parent = error)
            }
          )
          return(promise)
        } else {
          eval(render_expr)
        }
      }
      if (!fs::file_exists(render_path)) {
        reqres::abort_internal_error("Failed to render report")
      }
      response$status <- 200L
      response$file <- render_path
      response$type <- type
      if (!is.null(finalize)) {
        finalize(request, response, ...)
      }
      continue
    })
  })
  route
}

#' @importFrom brio read_file
render_expr <- quote({
  temp_path <- fs::path_file(render_path)
  if (is_quarto) {
    file_name <- fs::path_ext_remove(temp_path)
    render_params$metadata[["output-file"]] <- fs::path_ext_remove(temp_path)
    rlang::inject(quarto::quarto_render(
      execute_params = report_params,
      !!!render_params
    ))
    if (type == "text/html") {
      file <- brio::read_file(temp_path)
      file <- gsub(link_pattern, link_sub, file)
      brio::write_file(file, temp_path)
    }
  } else {
    rlang::inject(rmarkdown::render(
      output_file = temp_path,
      params = report_params,
      envir = new.env(parent = emptyenv()),
      !!!render_params
    ))
  }
  fs::file_move(temp_path, render_path)
})

#' Get the mime types of the possible outputs for a report
#'
#' @param file The path to the report
#'
#' @return A list with mime types of output and acceptable parameters
#'
#' @export
#' @keywords internal
#'
report_info <- function(file) {
  formats <- if (grepl("\\.qmd$", file, ignore.case = TRUE)) {
    quarto_info(file)
  } else {
    rmarkdown_info(file)
  }
  list(
    mime_types = unique(unlist(format_info$mime_render_types[formats$formats])),
    query_params = formats$params
  )
}

quarto_info <- function(input) {
  res <- quarto::quarto_inspect(input)
  params <- NULL
  formats <- NULL
  if (res$engines == "knitr") {
    params <- names(res$fileInformation[[input]]$metadata$params)
    formats <- tolower(names(res$formats))
  }
  list(
    params = params %||% character(),
    formats = formats %||% "html"
  )
}
rmarkdown_info <- function(input) {
  check_installed("knitr")
  params <- knitr::knit_params(paste0(readLines(input), collapse = "\n"))
  list(
    params = unname(vapply(params, `[[`, character(1), "name")),
    formats = rmarkdown::all_output_formats(input) %||% "html_document"
  )
}

#' Register a new report format
#'
#' The [report_route()] depends on the formats specified in the yaml header to
#' determine which mime types are supported for the `Content-Type` request
#' header. routr comes with a long list of well known formats but in the case a
#' format is unknown, you can register it yourself so that the correct mime type
#' and file extension can be deduced.
#'
#' @param format The name of the report format
#' @param mime_type The mime type of the output it produces
#' @param extension The file extension that the output should have. If `NULL` it
#' will be deduced from the mime type
#' @param force Should already existing formats be overwritten.
#'
#' @return `register_report_format()` is called for its side effect.
#' `show_report_formats()` returns a data frame.
#'
#' @export
#' @keywords internal
#'
register_report_format <- function(format, mime_type, extension = NULL, force = FALSE) {
  if (format %in% names(format_info$mime_render_types) && !force) {
    cli::cli_abort("{.val {format}} already exists. Set {.code force = TRUE} to overwrite")
  }
  if (is.null(extension)) {
    extension <- reqres::mime_type_info(mime_type)$extensions[[1]][[1]]
    if (is.null(extension)) {
      cli::cli_abort(c(
        "Failed to discover default file extension for {.val {mime_type}}",
        i = "Please set it using the {.arg extension} argument"
      ))
    }
  }
  format_info$mime_render_types[[format]] <- mime_type
  format_info$mime_render_ext[[format]] <- extension
  invisible()
}
#' @export
#' @rdname register_report_format
show_report_formats <- function() {
  formats <- data.frame(
    format = names(format_info$mime_render_types),
    mime_type = unlist(format_info$mime_render_types),
    extension = unlist(format_info$mime_render_ext)
  )
  attr(formats, "row.names") <- .set_row_names(nrow(formats))
  formats
}

format_info <- new.env()
format_info$mime_render_types <- list(
  # Quarto
  "html" = "text/html",
  "pdf" = "application/pdf",
  "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  "odt" = "application/vnd.oasis.opendocument.text",
  "epub" = "application/epub+zip",
  "revealjs" = "text/html",
  "pptx" = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  "beamer" = "application/pdf",
  "gfm" = "text/markdown",
  "commonmark" = "text/markdown",
  "hugo-md" = "text/markdown",
  "docusaurus-md" = "text/markdown",
  "markua" = "text/markdown",
  "mediawiki" = "text/plain",
  "dokuwiki" = "text/plain",
  "zimwiki" = "text/plain",
  "jira" = "text/plain",
  "xwiki" = "text/plain",
  "ipynb" = "application/x-ipynb+json",
  "rtf" = "application/rtf",
  "rst" = "text/x-rst",
  "asciidoc" = "text/asciidoc",
  "asciidoctor" = "text/asciidoc",
  "org" = "text/org",
  "muse" = "text/plain",
  "texinfo" = "application/x-texinfo",
  "man" = "text/plain",
  "textile" = "text/x-textile",
  # Rmarkdown
  "html_notebook" = "text/html",
  "html_document" = "text/html",
  "html_vignette" = "text/html",
  "pdf_document" = "application/pdf",
  "word_document" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  "odt_document" = "application/vnd.oasis.opendocument.text",
  "rtf_document" = "application/rtf",
  "md_document" = "text/markdown",
  "github_document" = "text/markdown",
  "ioslides_presentation" = "text/html",
  "revealjs::revealjs_presentation" = "text/html",
  "slidy_presentation" = "text/html",
  "beamer_presentation" = "application/pdf",
  "powerpoint_presentation" = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  "flexdashboard::flex_dashboard" = "text/html",
  "tufte::tufte_handout" = "application/pdf",
  "tufte::tufte_html" = "text/html",
  "tufte::tufte_book" = "application/pdf"
)

format_info$mime_render_ext <- list(
  # Quarto
  "html" = "html",
  "pdf" = "pdf",
  "docx" = "docx",
  "odt" = "odt",
  "epub" = "epub",
  "revealjs" = "html",
  "pptx" = "pptx",
  "beamer" = "pdf",
  "gfm" = "md",
  "commonmark" = "md",
  "hugo-md" = "md",
  "docusaurus-md" = "md",
  "markua" = "md",
  "mediawiki" = "wiki",
  "dokuwiki" = "wiki",
  "zimwiki" = "wiki",
  "jira" = "wiki",
  "xwiki" = "wiki",
  "ipynb" = "ipynb",
  "rtf" = "rtf",
  "rst" = "rst",
  "asciidoc" = "adoc",
  "asciidoctor" = "adoc",
  "org" = "org",
  "muse" = "muse",
  "texinfo" = "texi",
  "man" = "roff",
  "textile" = "textile",
  # Rmarkdown
  "html_notebook" = "nb.html",
  "html_document" = "html",
  "html_vignette" = "html",
  "pdf_document" = "pdf",
  "word_document" = "docx",
  "odt_document" = "odt",
  "rtf_document" = "rtf",
  "md_document" = "md",
  "github_document" = "md",
  "ioslides_presentation" = "html",
  "revealjs::revealjs_presentation" = "html",
  "slidy_presentation" = "html",
  "beamer_presentation" = "pdf",
  "powerpoint_presentation" = "pptx",
  "flexdashboard::flex_dashboard" = "html",
  "tufte::tufte_handout" = "pdf",
  "tufte::tufte_html" = "html",
  "tufte::tufte_book" = "pdf"
)
