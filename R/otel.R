otel_tracer_name <- "r.package.routr"

get_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})

testthat__is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


with_route_ospan <- function(expr, ..., handlerInfo, request, response, keys) {

  tracer <- get_tracer()
  is_enabled <- tracer$is_enabled()

  if (!is_enabled) {
    # Quit early if otel is disabled
    return(force(expr))
  }

  name <- paste0(request$method, "_", handlerInfo$path)

  parent <- request$otel

  # OpenTelemetry
  # TODO: Allow server introspection of actual server host and port (network.local.address and network.local.port)
  # http.response.status_code and http.response.header.<key> can only be set later
  span <- otel::start_span(
    paste0(name, "_route"),
    options = list(kind = "server", parent = parent),
    attributes = list2(
      routr.route = handlerInfo$path,
      !!!set_names(keys, paste0("routr.path.param.", names(keys)))
    ),
    tracer = tracer
  )

  # If the route does not contain any wildcards we deem it specific enough to
  # qualify as the parents main route. The last route with this trait wins
  if (!is.null(parent) && handlerInfo$n_wildcard == 0) {
    parent$name <- name
    parent$set_attribute("http.route", handlerInfo$path)
  }

  needs_cleanup <- TRUE
  cleanup <- function() {
    if (!is.null(response$status)) {
      span$set_attribute("http.response.status_code", response$status)
      if (response$status >= 500) {
        span$set_status("error")
        span$set_attribute("error.type", as.character(response$status))
      }
    }
    end_span(span)
  }
  on.exit(if (needs_cleanup) cleanup(), add = TRUE)

  continue <-
    # Add domain to propagate the currently active span during promise context switching
    promises::with_ospan_promise_domain({
      otel::with_active_span(span, expr)
    })

  if (!promises::is.promising(continue)) {
    span$set_attribute("http.response.status_code", response$status)
    if (response$status >= 500) {
      span$set_status("error")
      span$set_attribute("error.type", as.character(response$status))
    }
    otel::end_span(span)
    check_bool(continue)
  } else {
    needs_cleanup <- FALSE
    continue <- promises::then(continue, function(val) check_bool(val))
    continue <- promises::finally(continue, function(val) {
      cleanup()
    })
  }
}
