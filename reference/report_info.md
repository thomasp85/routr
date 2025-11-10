# Get the mime types of the possible outputs for a report

Get the mime types of the possible outputs for a report

## Usage

``` r
report_info(file)
```

## Arguments

- file:

  The path to the report

## Value

A list with the formats, mime types, and file extensions of output,
acceptable parameters (a named list with names corresponding to the
parameter name and value corresponding to default value), and the title
of the document. For quarto documents the default values of parameters
are omitted.
