#'////////////////////////////////////////////////////////////////////////////
#' FILE: utils_1_brewery.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-05
#' MODIFIED: 2020-02-05
#' PURPOSE: utils for wrangling brewery data
#' STATUS: working
#' PACKAGES: tidyverse; osmdata; 
#' COMMENTS:
#'////////////////////////////////////////////////////////////////////////////

#' FUNCTIONS
#' define a set of functions to be used for sourcing brewery data
brew <- list()

#' json => data.frame
brew$list_to_df <- function(x) {
    x %>%
        jsonlite::toJSON(.) %>%
        jsonlite::fromJSON(.) %>%
        sapply(., unlist) %>%
        as.data.frame()
}

#' extract bounding box coordinates
brew$bb_centroid <- function(x) {
    d <- as.data.frame(x)
    return(
        data.frame(
            lng = (d$min[1] + d$max[1]) / 2,
            lat = (d$min[2] + d$max[2]) / 2,
            stringsAsFactors = FALSE
        )
    )
}

#' FUNCTIONS FOR OVERPASS QUERIES
brew$overpass <- list()

#' create a function that generates the overpass script. This function takes the
#' following arguments
#' radius: search around a given point in meters (default 35km)
#' lat: latitude (req)
#' lng: longitude (req)
brew$overpass$new_query <- function(radius = 35000, lat, lng, timeout = 750) {
    paste0(
        "[out:json][timeout:", timeout, "];",
        "(",
        "nwr",
        "['craft'='brewery']",
        "(around:", radius, ",", lat, ",", lng, ");",
        "nwr",
        "['amenity'='restaurant']",
        "['microbrewery'='yes']",
        "(around:", radius, ",", lat, ",", lng, ");",
        "nwr",
        "['amenity'='pub']",
        "['microbrewery'='yes']",
        "(around:", radius, ",", lat, ",", lng, ");",
        "nwr",
        "['amenity'='bar']",
        "['microbrewery'='yes']",
        "(around:", radius, ",", lat, ",", lng, ");",
        "nwr",
        "['building'='brewery']",
        "(around:", radius, ",", lat, ",", lng, ");",
        ");",
        "out;",
        ">;",
        "out skel qt;",
        sep = ""
    )
}