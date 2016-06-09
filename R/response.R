#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.count
Response <- R6Class('Response',
    public = list(
        # Data
        extraData = list(),

        # Methods
        initialize = function() {
            private$STATUS = 404L
            private$HEADERS = new.env(parent = emptyenv())
            private$BODY = ''
        },
        set_header = function(name, value) {
            is.scalar(name)
            is.scalar(value)
            assign(as.character(name), as.character(value), envir = private$HEADERS)
        },
        remove_header = function(name) {
            rm(name, envir = private$HEADERS)
        },
        as_list = function() {
            list(
                status = private$STATUS,
                headers = as.list(private$HEADERS),
                body = private$BODY
            )
        }
    ),
    active = list(
        status = function(code) {
            if (missing(code)) return(private$STATUS)
            is.count(code)
            is.scalar(code)
            private$STATUS <- code
        },
        body = function(content) {
            if (missing(content)) return(private$BODY)
            if (is.raw(content)) {
                private$BODY <- content
            } else {
                is.scalar(content)
                private$BODY <- as.character(content)
            }
        }
    ),
    private = list(
        # Data
        STATUS = NULL,
        HEADERS = NULL,
        BODY = NULL
    )
)
