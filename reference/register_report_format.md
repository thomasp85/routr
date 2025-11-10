# Register a new report format

The
[`report_route()`](https://routr.data-imaginist.com/reference/report_route.md)
depends on the formats specified in the yaml header to determine which
mime types are supported for the `Content-Type` request header. routr
comes with a long list of well known formats but in the case a format is
unknown, you can register it yourself so that the correct mime type and
file extension can be deduced.

## Usage

``` r
register_report_format(format, mime_type, extension = NULL, force = FALSE)

show_report_formats()
```

## Arguments

- format:

  The name of the report format

- mime_type:

  The mime type of the output it produces

- extension:

  The file extension that the output should have. If `NULL` it will be
  deduced from the mime type

- force:

  Should already existing formats be overwritten.

## Value

`register_report_format()` is called for its side effect.
`show_report_formats()` returns a data frame.
