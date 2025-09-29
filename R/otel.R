otel_tracer_name <- "thomasp85.routr"

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


with_route_ospan <- function(expr, ..., handlerInfo, method, request, response, keys) {

  tracer <- get_tracer()
  is_enabled <- tracer$is_enabled()

  if (!is_enabled) {
    # Quit early if otel is disabled
    return(force(expr))
  }

  # OpenTelemetry
  # TODO: Allow server introspection of actual server host and port (network.local.address and network.local.port)
  # http.response.status_code and http.response.header.<key> can only be set later
  span <- otel::start_span(
    handlerInfo$path,
    options = list(kind = "server"),
    attributes = list2(
      http.request.method = method,
      url.path = request$path,
      url.scheme = request$protocol,
      http.route = handlerInfo$path,
      network.protocol.name = "http",
      server.port = as.integer(sub("^.*:(.*)$", "\\1", request$host)),
      url.query = request$querystring,
      client.address = request$ip,
      network.protocol.version = 1.1,
      server.address = sub("^(.*):.*$", "\\1", request$host),
      user_agent.original = request$headers[["user_agent"]],
      !!!set_names(
        request$headers,
        paste0(
          "http.request.header.",
          gsub("_", "-", names(request$headers))
        )
      ),
      !!!set_names(keys, paste0("path.param.", names(keys)))
    ),
    tracer = tracer
  )

  continue <-
    # Add domain to propagate the currently active span during promise context switching
    promises::with_ospan_promise_domain({
      otel::with_active_span(span, expr)
    })

  if (!promises::is.promising(continue)) {
    span$set_attribute("http.response.status_code", as.integer(response$status))
    otel::end_span(span)
    check_bool(continue)
  } else {
    continue <- promises::then(continue, function(val) {
      span$set_attribute("http.response.status_code", as.integer(response$status))
      otel::end_span(span)
    })
  }
}
