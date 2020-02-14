#'////////////////////////////////////////////////////////////////////////////
#' FILE: utils_2_museums.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-14
#' MODIFIED: 2020-02-14
#' PURPOSE: utils used in the sourcing and cleaning of museums data
#' STATUS: working
#' PACKAGES: NA
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' Define a list object for functions
museums <- list()

#' Create a function that generates a new overpass query using the following
#' arguments.
#'      - radius: search around a given point in meters (35km)
#'      - lat: latitude (required)
#'      - lng: longitude (required)
museums$new_query <- function(radius = 35000, lat, lng, timeout = 750) {
    paste0(
       "[out:json][timeout:", timeout, "];",
        "(",
        "nwr['tourism'='museum']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='history']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='art']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='railway']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='local']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='open_air']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='technology']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='nature']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='military']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='archaeological']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='local']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='transport']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='living_history']", "(around:", radius, ",", lat, ",", lng, ");",
        "nwr['museum'='children']", "(around:", radius, ",", lat, ",", lng, ");",
        ");",
        "out;",
        ">;",
        "out skel qt;",
        sep = ""
    )
}