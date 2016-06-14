
<!-- README.md is generated from README.Rmd. Please edit that file -->
routr
=====

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/routr.svg?branch=master)](https://travis-ci.org/thomasp85/routr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/routr?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/routr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/routr)](http://cran.r-project.org/package=routr) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/routr)](http://cran.r-project.org/package=routr) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/routr/master.svg)](https://codecov.io/github/thomasp85/routr?branch=master)

routr is a simple and versatile router for R based web servers. For people not familiar with back-end development, a router is a piece of middleware that delegates HTTP requests to the correct handler function. The delegation is based in the URL of the request and in essence means that requests directed at */persons/thomas/* ends up in another handler than */packages/routr/*.

routr is heavily inspired by other routers build for other platforms, especially those for [Express.js](https://github.com/expressjs) and [Ruby on Rails](https://github.com/rails/rails), though it doesn't mimick either.

Functionality
-------------

Currently routr supports HTTP requests though WebSocket support is in the pipeline. An HTTP router is build up of several seperate routes that are collected in a route stack. The stack recieves the request and passes it on to the first route in the stack. Depending on whether the route can handle the request and whether the handler signals a fall-though, the request is passed along the stack until a handler signals that no further processing should be done. This means that it is possible to stack different functionality like user verification, static ressource serving, etc. on top of each other.

### The handler

A handler is a function that accepts the arguments `request`, `response`, `keys`, and `...`. The request and response is R6 objects that the handler is free to modify. The handler must return a boolean indicating if the request should be passed down the stack (`TRUE`) or not (`FALSE`). An example of a simple handler is:

``` r
h <- function(request, response, keys, ...) {
    response$status <- 200L
    response$headers[['Content-Type']] <- 'text/html'
    response$body <- 'Hello World!'
    return(FALSE)
}
```

No matter which request is passed to this handler it will return a "Hello World!" to the client. Because it returns `FALSE` it block any other handlers below it to modify the response.

### The route

A route is a collection of handlers. For any given request, only one handler in the route will be called. A route is an object of the R6 Route class and can be created as so:

``` r
route <- Route$new()
route$add_handler('get', '/hello/:what/', h)
#> Called from: private$sort_ids(method)
#> debug at /Users/Thomas/Dropbox/GitHub/routr/R/route.R#108: nTokens <- sapply(private$handlerMap[[method]], `[[`, "nTokens")
#> debug at /Users/Thomas/Dropbox/GitHub/routr/R/route.R#109: nKeys <- sapply(private$handlerMap[[method]], `[[`, "nKeys")
#> debug at /Users/Thomas/Dropbox/GitHub/routr/R/route.R#110: wildcard <- sapply(private$handlerMap[[method]], `[[`, "wildcard")
#> debug at /Users/Thomas/Dropbox/GitHub/routr/R/route.R#111: sortOrder <- order(nTokens, nKeys, !wildcard, decreasing = TRUE)
#> debug at /Users/Thomas/Dropbox/GitHub/routr/R/route.R#112: private$handlerMap[[method]] <- private$handlerMap[[method]][sortOrder]
```

The first argument to `add_handler` defines the [request type](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods) while the second defines the path that the handler responds to. The path need not be static. In the above example the `:what` defines a variable meaning that the handler will respond to any `/hello/<something>/` variation. The variable and the value is available to the handler in the keys argument. For instance, if a request with the URL `/hello/mars/` were passed through the route, the keys argument passed to the handler would contain `list(what = 'mars')`. Variables can only span a single level, meaning that the above handler would not respond to `/hello/jupiter/saturn/`. To match to anything use `/hello/*` for responding to any sub-URL to `hello`. Matches to `*` will not end up in the keys list. If several paths in a route matches a URL the most specific will be used, meaning that `/*` will match everything but always chosen last.

### The route stack

The route stack manages several routes and takes care of receiving a request and returning a response. routr is ROOK compliant meaning that it understands the requests send on from [httpuv](https://github.com/rstudio/httpuv) and returns a compatible response. A route stack is an object of the R6 class `RouteStack` and is created like this:

``` r
router <- RouteStack$new()
router$add_route(route, 'test')
```

The order in which routes are added to the stack determines the calling order, with those added first taking precedence over those added later. Request are handled by the `dispatch` method like so:

``` r
router$dispatch(request)
```
