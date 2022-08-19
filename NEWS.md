# routr 0.4.1

* General upkeep

# routr 0.4.0

* Add `get_handler()` method to `Route` (#9, @cpsievert)
* Add `root` field to `Route` which will get appended to all paths before 
  matching to an incomming request
* Add `remap_handlers()` to loop through all handlers and reassign them based on
  a user provided function. (#8)
* Added pkgdown site at https://routr.data-imaginist.com

# routr 0.3.0

* Add `on_error()` method to modify how errors are handled. The default is now 
  to return `500` without any body and print the error message with `message()`.
* Modified `on_attach()` method that uses the new logging system to log route
  errors.

# routr 0.2.0

* First release
* Added classes *Request*, *Response*, *Route*, and *RouteStack*
