% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_route.R
\name{register_report_format}
\alias{register_report_format}
\alias{show_report_formats}
\title{Register a new report format}
\usage{
register_report_format(format, mime_type, extension = NULL, force = FALSE)

show_report_formats()
}
\arguments{
\item{format}{The name of the report format}

\item{mime_type}{The mime type of the output it produces}

\item{extension}{The file extension that the output should have. If \code{NULL} it
will be deduced from the mime type}

\item{force}{Should already existing formats be overwritten.}
}
\value{
\code{register_report_format()} is called for its side effect.
\code{show_report_formats()} returns a data frame.
}
\description{
The \code{\link[=report_route]{report_route()}} depends on the formats specified in the yaml header to
determine which mime types are supported for the \code{Content-Type} request
header. routr comes with a long list of well known formats but in the case a
format is unknown, you can register it yourself so that the correct mime type
and file extension can be deduced.
}
\keyword{internal}
