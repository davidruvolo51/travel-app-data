#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_3a_source_museums.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-14
#' MODIFIED: 2020-02-14
#' PURPOSE: source museums data from overpass
#' STATUS: in.progress
#' PACKAGES: tidyverse; httr
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

# load cities data
city_info <- readRDS("data/cafe_cities_geocoded.RDS")

# load utils
source("scripts/utils/utils_2_museums.R")

#'////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' SOURCE MUSEUMS FOR CITIES
#' Like the breweries script (data_2a_...), I will create a loop that finds
#' all of the museums in cities where the is also specialty coffee. The
#' search radius is the same (35km) and the following OSM tags are used to
#' in the query.
#'      - ['tourism'='museum']
#'      - ["museum"="history"]
#'      - ["museum"="art"]
#'      - ["museum"="railway"]
#'      - ["museum"="local"]
#'      - ["museum"="open_air"]
#'      - ["museum"="technology"]
#'      - ["museum"="nature"]
#'      - ["museum"="military"]
#'      - ["museum"="archaeological"]
#'      - ["museum"="local"]
#'      - ["museum"="transport"]
#'      - ["museum"="living_history"]
#'      - ["museum"="children"]
#' For ease, these tags were combined into one ["museum" ~ "^(history|art|...)"]
#'

#' init loop vars
d <- 1
fails <- 0
reps <- 1#NROW(city_info)
failed <- list()

# run
while (d <= reps) {
    cat("Starting new city (", d, "of", reps, ")\n")

    # prep query
    q <- museums$new_query(
        lat = city_info$lat[d],
        lng = city_info$lng[d]
    )

    # Send request
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
            "data/museums/",
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
    time <- round(runif(1, 45, 120), 0)
    msg <- paste0("\tPausing for '", time, "' seconds at '", as.character(Sys.time()), "'\n")
    cat(msg)
    Sys.sleep(time)
}