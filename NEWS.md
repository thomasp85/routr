# routr 0.3.0

* Add `on_error()` method to modify how errors are handled. The default is now 
  to return `500` without any body and print the error message with `message()`.
* Modified `on_attach()` method that uses the new logging system to log route
  errors.

# routr 0.2.0

* First release
* Added classes *Request*, *Response*, *Route*, and *RouteStack*
