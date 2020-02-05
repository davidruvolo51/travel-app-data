#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_2a_source_breweries.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-05
#' MODIFIED: 2020-02-05
#' PURPOSE: source brewery data for European Cities
#' STATUS: in.progress
#' PACKAGES: tbd
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

#'////////////////////////////////////////

#' ~ 99 ~
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

#'//////////////////////////////////////////////////////////////////////////////

#' ~ 0 ~
#' DEFINE UNIQUE CITIES & GEOCODE CITY
#' Load the file coffee_all_cafes.RDS file and build an object that contains
#' unique cities by country.

#' Load master dataset
cafes <- readRDS("data/coffee_all_cafes.RDS")


#' recode cafes where countries was incorrect
cafes$country[cafes$cafeId == "cafe_1197"] <- "Belgium"


#' find distinct cities and countries and remove missing rows. One cafe,
#' "Wilekaffee Rosterei" in Germany that is missing a value for city. The
#' website indicates that it is located in "Garmisch-Partenkirchen", but
#' I did not see it anywhere on their website. Perhaps the location moved
#' or the site is outdated. Since I cannot confirm the location, I will
#' remove it from the dataset.
cities <- cafes %>%
    distinct(city, country) %>%
    filter(city != "", country != "")


#' There are a few options to get coordinates for cities. We could either
#' grab random coordinates from the dataset for each city, use google places
#' API to geocode each city using a search query, or calcuate the centroids
#' of all cafes. Using google places API, might be a bit of an overkill and
#' it is difficult to know the location of a cafe in relation to center point
#' of a city so pulling random coordinates might not be accurate. Instead,
#' I will calculate the centroid of each city using the coordinates of all
#' cafes. This will give us a better search radius for running overpass API
#' queries that is aligned with where the cafes are located.

city_geo <- list()
sapply(seq_len(NROW(cities)), function(index) {

    # filter master cafe list for current city and return vars of interest
    # leave entries with missing lat and lng as these cities will be geocoded
    # manually
    tmp <- cafes %>%
        filter(city == cities$city[index]) %>%
        select(city, country, lng, lat)
    if (NROW(tmp) == 1 & NROW(tmp[is.na(tmp$lat), ]) == 0) {
        l <- list(
            city = cities$city[index],
            country = cities$country[index],
            lat = tmp$lat,
            lng = tmp$lng
        )
    } else if (NROW(tmp) == 2 & NROW(tmp[is.na(tmp$lat), ]) == 0) {
        l <- list(
            city = cities$city[index],
            country = cities$country[index],
            lat = mean(tmp$lat),
            lng = mean(tmp$lng)
        )
    } else if (NROW(tmp) > 2 | NROW(tmp[is.na(tmp$lat), ]) > 0) {
        # build bounding box
        bb <- osmdata::getbb(
            paste0(
                cities$city[index],
                " ",
                cities$country[index]
            )
        )

        # calculate centroid of bounding box
        centroid <- brew$bb_centroid(x = bb)

        # return as data.frame
        l <- list(
            city = cities$city[index],
            country = cities$country[index],
            lat = centroid$lat,
            lng = centroid$lng
        )
    } else {
        cat("ERROR: This should never happen")
    }

    # out
    cat("Completed: ", index, "of", NROW(cities), "\n")
    city_geo[[index]] <<- l
})

#' convert city_geo to data.frame
city_info <- brew$list_to_df(city_geo)
city_info$lat <- as.numeric(city_info$lat)
city_info$lng <- as.numeric(city_info$lng)

#'//////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' SOURCE BREWERIES FOR CITIES
#' The rationale for geocoding cities is that we can now use accurate coords
#' for querying overpass API with radius. This section will build and run
#' queries using the city data and return json file with all breweries in
#' a city

brew$overpass <- list()
#' create a function that generates the overpass script. This function takes the
#' following arguments
#' radius: search around a given point (default 35km)
#' lat: latitude (req)
#' lng: longitude (req)
brew$overpass$new_query <- function(radius = 35000, lat, lng, timeout = 750) {
    q <- paste0(
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

    # encode query
    # query <- URLencode(q, reserved = TRUE)
    paste0("http://overpass-api.de/api/interpreter?data=", q)
}

#' create a function that runs a query
brew$overpass$new_command <- function(name, query, index) {
    n <- gsub(pattern = "[[:space:]]", replacement = "_", x = tolower(name))
    exec <- paste0(
        "wget -O ",
        "\"data/breweries/raw_index_", n, ".json\" ",
        "\"", query, "\""
    )
    return(exec)
}

#' ~ A ~
#' Run loop to fetch data from overpass API
d <- 1
reps <- NROW(city_info)
while (d <= reps) {

    # prep query and shell command
    q <- brew$overpass$new_query(
        lat = city_info$lat[d],
        lng = city_info$lng[d]
    )

    cmd <- brew$overpass$new_command(
        name = paste0(city_info$city[d], " ", city_info$country[d]),
        query = q
    )

    # run command
    system(cmd)

    # update data
    cat("Completed:", d, "of", reps)
    d <- d + 1

    # add a kleine pauze
    Sys.sleep(runif(1, 0, 1.25))
}


# test
# query <- brew$overpass$new_query(
#     radius = 35000, 
#     lat = city_info$lat[1],
#     lng = city_info$lng[1]
# )

# cmd <- brew$overpass$new_command(
#     name = city_info$city[1],
#     query = query
# )

# system(cmd)