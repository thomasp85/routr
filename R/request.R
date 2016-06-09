#' @importFrom R6 R6Class
#' @importFrom httpuv decodeURIComponent
Request <- R6Class('Request',
    public = list(
        initialize = function(rook) {
            private$ROOK <- rook
            private$TYPE <- rook$REQUEST_METHOD
            private$URL <- rook$PATH_INFO
            private$QUERY <- private$parse_query(rook$QUERY_STRING)
            private$HEADERS <- private$get_headers(rook)
            private$CONTENT <- private$parse_content(rook$rook.input)
        }
    ),
    active = list(
        type = function() {
            private$TYPE
        },
        url = function() {
            private$URL
        },
        query = function() {
            private$QUERY
        },
        headers = function() {
            private$HEADERS
        },
        content = function(value) {
            if(missing(value)) return(private$CONTENT)
            else private$CONTENT <- value
        },
        rook = function() {
            private$ROOK
        }
    ),
    private = list(
        TYPE = '',
        URL = '',
        QUERY = list(),
        HEADERS = list(),
        CONTENT = '',
        ROOK = NULL,
        parse_query = function(query) {
            if(query == '') return(list())
            query <- decodeURIComponent(query)
            query <- sub('^\\?', '', query)
            query <- gsub('\\+', ' ', query)
            query <- strsplit(unlist(strsplit(query, '&|;')), '=')
            ans <- list()
            for(i in query) {
                ans[[i[1]]] <- append(ans[[i[1]]], i[2])
            }
            lapply(ans, type.convert, as.is=TRUE)
        },
        parse_content = function(input) {
            if(inherits(input, 'NullInputStream')) return(raw())

            input$read(input$.length)
        },
        get_headers = function(rook) {
            vars <- ls(rook)
            headers <- vars[grepl('^HTTP_', vars)]
            ans <- lapply(headers, function(head) {
                rook[[head]]
            })
            names(ans) <- sub('^HTTP_', '', headers)
            ans
        }
    )
)
