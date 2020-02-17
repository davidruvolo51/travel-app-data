#'////////////////////////////////////////////////////////////////////////////
#' FILE: utils_99_general.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-15
#' MODIFIED: 2020-02-16
#' PURPOSE: misc functions
#' STATUS: working
#' PACKAGES: NA
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' Define Functions
tools <- list()

#' Evaluated Missing Cases
tools$find_missing_cases <- function(data) {
    d <- sapply(
        seq_len(NCOL(data)), function(col) {
            data.frame(
                name = names(data[col]),
                missing = NROW(data[is.na(data[, col] == TRUE), ]),
                percent = round(
                    (NROW(data[is.na(data[, col]) == TRUE, ]) / 
                         NROW(data) * 100
                    ),
                    2
                )
            )
        }
    )
    return(data.frame(t(d), stringsAsFactors = FALSE))
}

#' Define New Nominatim Query
tools$nominatim <- list()
tools$nominatim$new_request <- function(
    id, type, format = "json", address_details = TRUE, name_details = TRUE) {
    
    #' Return NWR
    evalType <- function(x) {
        if (tolower(x) %in% c("node", "nodes", "n")) {
            return("N")
        } else if (tolower(x) %in% c("Way","way", "ways", "w")) {
            "W"
        } else if (tolower(x) %in% c("relations", "relation", "r")){ 
            return("R")
        } else {
            return(NA)
        }
    }
    
    # form prefix and output defaults
    prefix <- "https://nominatim.openstreetmap.org/lookup?"
    output <- paste0("&format=", format)
    
    # process address arg
    if (isTRUE(address_details)) {
        output <- paste0(output, "&addressdetails=1")
    }
    
    # process name arg
    if (isTRUE(name_details)) {
        output <- paste0(output, "&namedetails=1")
    }
    
    #'//////////////////////////////////////
    #' Process Query Based on Length of Input
    if (length(id) == 1) {
        ids <- id
        type <- evalType(x = type)
        return(paste0(prefix, "osm_ids=", type, id, output))
    }
    
    if (length(id) > 1) {
        ids <- as.character(
            sapply(
                seq_len(length(id)), function(i) {
                    t <- evalType(x = type[i])
                    paste0(t,id[i])
                }
            )
        )
        return(
            paste0(
                prefix,
                "osm_ids=", 
                paste0(ids, collapse = ","),
                output
            )
        )
    }
}

# tools$nominatim$new_request(id = "1234", type = "Node")
# tools$nominatim$new_request(id = c("1234","4556"), type = c("n","w"))

