#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_2a_source_breweries.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-05
#' MODIFIED: 2020-02-06
#' PURPOSE: source brewery data for European Cities
#' STATUS: in.progress
#' PACKAGES: tidyverse; osmdata
#' COMMENTS:
#'      The purpose of this script is to source data on breweries using the
#'      cities gathered in scripts 1a and 1b. To do this, all cities will need
#'      to be geocoded or transformed into bounding boxes. Overpass API is used
#'      to run queries and all data is saved in json format into this folder:
#'      data/breweries_raw/*.
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

# utils
source("scripts/utils/utils_1_brewery.R")

# load city reference data
city_info <- readRDS("data/cafe_cities_geocoded.RDS")

#'//////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' SOURCE BREWERIES FOR CITIES
#' The rationale for geocoding cities is that we can now use accurate coords
#' for querying overpass API with radius. This section will build and run
#' queries using the city data and return json file with all breweries in
#' a city. Run loop to fetch data from overpass API
#'
#' In this script, I will query for breweries in cities where there is specialty
#' coffee. The search radius is 35km and the following OSM tags are used to find
#' "breweries"
#'      - ["craft" = "brewery"]
#'      - ["amenity" = "restaurant"]["microbrewery" = "yes"]
#'      - ["amenity" = "pub"]["microbrewery" = "yes"]
#'      - ["amenity" = "bar"]["microbrewery" = "yes"]
#'      - ["building" = "brewery"]
#'
#' All queries are `nwr` (node, way, relations)

#' For failed items, run the following lines. Adjust `d` and reps accordingly
#' with row name
#' failures <- failed
#' failed_cities <- sapply(seq_len(length(failures)), function(x) {
#'    failed[[x]][["city"]]
#' })
#' city_info[city_info$city %in% failed_cities,] %>% rownames()

# init loop vars
d <- 1
fails <- 0
reps <- NROW(city_info)
failed <- list()

# run
while (d <= reps) {

    cat("Starting new city (", d, "of", reps, ")\n")
    # prep query and shell command
    q <- brew$overpass$new_query(
        lat = city_info$lat[d],
        lng = city_info$lng[d]
    )

    # send request
    cat("\tSending GET request...")
    response <- httr::GET(
        url = "http://overpass-api.de/",
        path = paste0("api/interpreter?data=", URLencode(q, reserved = T))
    )

    # process response
    if (response$status_code == 200) {
        cat("Sucess!\n")

        # extract body
        json <- httr::content(response, "text", encoding = "utf-8")

        # form file name
        name <- paste0(city_info$city[d], " ", city_info$country[d])
        n <- gsub(
            pattern = "[[:space:]]",
            replacement = "_",
            x = name
        )

        # build file path with name
        file <- paste0(
            "data/breweries/",
            "raw_", d, "_",
            tolower(n),
            ".json"
        )

        # write json to file
        cat("\tSaving file...")
        readr::write_file(x = json, path = file)
        cat("Success!\n")

    } else {

        # notify fail and update object fails
        cat("\tFailed!\n")
        cat("\t\tAdding to object 'failed' (fails:", fails, ")")
        fails <- fails + 1
        failed[[fails]] <- list(
            city = city_info$city[d],
            country = city_info$country[d]
        )
    }

    # update counter and display message
    cat("\tCompleted!\n")
    d <- d + 1

    # met een kleine pauze
    time <- round(runif(1, 45, 120),0)
    msg <- paste0("\tPausing for '", time, "' seconds at '", as.character(Sys.time()), "'\n")
    cat(msg)
    Sys.sleep(time)
}
