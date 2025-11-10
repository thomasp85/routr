otel_tracer_name <- "r.package.routr"

routr_otel_tracer <- local({
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


with_route_ospan <- function(
  expr,
  ...,
  handlerInfo,
  request,
  response,
  keys,
  check_output = TRUE,
  call = caller_env()
) {
  tracer <- routr_otel_tracer()
  is_enabled <- tracer$is_enabled()

  if (!is_enabled) {

    continue <-
      # Avoid overhead of possible promise if not utilized
      if (check_output) {
        promises::hybrid_then(
          expr,
          on_success = function(continue) {
            check_bool(continue, call = call)
          },
          tee = TRUE
        )
      } else {
        force(expr)
      }

    return(continue)
  }

  name <- paste0(request$method, "_", handlerInfo$path)
  parent <- request$otel

  # If the route does not contain any wildcards we deem it specific enough to
  # qualify as the parents main route. The last route with this trait wins
  if (!is.null(parent) && handlerInfo$n_wildcard == 0) {
    parent$name <- name
    parent$set_attribute("http.route", handlerInfo$path)
  }

  set_span_status <- function() {
    if (!is.null(response$status)) {
      span <- otel::get_active_span()

      span$set_attribute("http.response.status_code", response$status)

      if (response$status >= 500) {
        span$set_status("error")
        span$set_attribute("error.type", as.character(response$status))
      }
    }
  }

  # OpenTelemetry
  # TODO: Allow server introspection of actual server host and port (network.local.address and network.local.port)
  # http.response.status_code and http.response.header.<key> can only be set later

  # Add handoff promise domain to ensure active ospan propagation
  promises::with_otel_promise_domain({
    # Start ospan and end when `expr` has resolved
    promises::with_otel_span(
      name = paste0(name, "_route"),
      options = list(kind = "server", parent = parent),
      attributes = list2(
        routr.route = handlerInfo$path,
        !!!set_names(keys, paste0("routr.path.param.", names(keys)))
      ),
      tracer = tracer,
      {
        promises::hybrid_then(
          expr,
          on_success = function(continue) {
            set_span_status()

            if (check_output) {
              check_bool(continue, call = call)
            }
          },
          on_failure = function(e) {
            set_span_status()
          },
          tee = TRUE
        )
      }
    )
  })
}
