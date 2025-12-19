# Changelog

## routr 2.0.0

CRAN release: 2025-12-18

- **Breaking:** New routing engine based on the waysign package. All the
  path syntax of the waysign package now supported along with massive
  speedup in handler lookup. This is marked breaking because there may
  be edge cases where the priority of handlers has changed. For the vast
  majority of use cases you shouldn’t expect behavioral changes.
- The return value of handlers are no longer checked in the service of
  micro-performance gains. Any non-`TRUE` value will signal an end to
  routing

## routr 1.1.0

CRAN release: 2025-11-18

- You can now set the cache folder when using
  [`report_route()`](https://routr.data-imaginist.com/reference/report_route.md)
  so that it can be reused between instances
  ([\#26](https://github.com/thomasp85/routr/issues/26))
- Output types for reports can now be selected by name by appending the
  name of the format to the path (e.g. `/report/revealjs`). This is in
  addition to the current options of content negotiation and file
  extension appending
  ([\#28](https://github.com/thomasp85/routr/issues/28))
- Report parameters can now be passed in with the request body using a
  POST request. The parameters must be formatted as JSON
  ([\#27](https://github.com/thomasp85/routr/issues/27))
- Report caching can now be scoped to the client id
  ([\#31](https://github.com/thomasp85/routr/issues/31))
- Quarto reports based on python now works properly with regards to
  extracting output formats and parameters
  ([\#29](https://github.com/thomasp85/routr/issues/29))
- Cached reports can now be deleted explicitly by sending a DELETE
  request. All versions of the report (with regard to parameters) are
  deleted. If you use a format-specific path only cached versions of
  that format is deleted
  ([\#30](https://github.com/thomasp85/routr/issues/30))
- Add support for OpenTelemetry through the otel package
  ([\#33](https://github.com/thomasp85/routr/issues/33))
- The
  [`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md)
  now better handles docs served through a proxy

## routr 1.0.0

CRAN release: 2025-08-21

- Use native rlang type checking instead of assertthat
- Added the ability to merge two routes or route stacks together
- routes can now be used directly as Fiery plugins
- New tidy api for creating routes and route stacks
- Added
  [`asset_route()`](https://routr.data-imaginist.com/reference/asset_route.md)
  for high performance serving of static files
- Fixed a bug that would case keys to be lower cased when provided by
  the handler ([\#16](https://github.com/thomasp85/routr/issues/16))
- The file system path in
  [`ressource_route()`](https://routr.data-imaginist.com/reference/ressource_route.md)
  is no longer expected to be absolute
  ([\#18](https://github.com/thomasp85/routr/issues/18))
- The Content-Type header is now set correctly when serving the default
  file in
  [`ressource_route()`](https://routr.data-imaginist.com/reference/ressource_route.md)
  ([\#19](https://github.com/thomasp85/routr/issues/19))
- Added
  [`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md)
  for serving API spec based on an openapi spec file
- Cleaned up condition handling in the interaction with fiery when used
  as a plug-in
- Added `reject_missing_methods` argument to `Route$add_handler()` to
  automatically catch requests to the handler path that doesn’t have a
  matching method and return 405L
- Added
  [`shared_secret_route()`](https://routr.data-imaginist.com/reference/shared_secret_route.md)
  for creating simple shared secret request rejection
- Added `empty` fields to Route and RouteStack classes
- Added `ignore_trailing_slash` argument to the RouteStack constructor.
  It allows the route stack to ignore the trailing slash of request in
  different ways
- Add redirection functionality to RouteStack
- wilcard matches to paths are now also provided in the key argument
- Support returning promises from handlers
- Added
  [`report_route()`](https://routr.data-imaginist.com/reference/report_route.md)
  to create a route that automatically renders and serves quarto and
  rmarkdown files

## routr 0.4.1

CRAN release: 2022-08-19

- General upkeep

## routr 0.4.0

CRAN release: 2019-10-03

- Add `get_handler()` method to `Route`
  ([\#9](https://github.com/thomasp85/routr/issues/9),
  [@cpsievert](https://github.com/cpsievert))
- Add `root` field to `Route` which will get appended to all paths
  before matching to an incomming request
- Add `remap_handlers()` to loop through all handlers and reassign them
  based on a user provided function.
  ([\#8](https://github.com/thomasp85/routr/issues/8))
- Added pkgdown site at <https://routr.data-imaginist.com>

## routr 0.3.0

CRAN release: 2017-10-26

- Add `on_error()` method to modify how errors are handled. The default
  is now to return `500` without any body and print the error message
  with [`message()`](https://rdrr.io/r/base/message.html).
- Modified `on_attach()` method that uses the new logging system to log
  route errors.

## routr 0.2.0

CRAN release: 2017-08-22

- First release
- Added classes *Request*, *Response*, *Route*, and *RouteStack*
