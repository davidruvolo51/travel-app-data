#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_1c_build_cities.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-14
#' MODIFIED: 2020-02-14
#' PURPOSE: index of all cities and coordinates
#' STATUS: complete; osmdata
#' PACKAGES: tidyverse
#' COMMENTS:
#'      The purpose of this file is create an object of all cities by country
#'      with geocode data of each city. This will be used to define the
#'      coordinates and search radius in the other scripts.
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

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

city_geo <- lapply(seq_len(NROW(cities)), function(index) {

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
    return(l)
})

#' convert city_geo to data.frame
city_info <- brew$list_to_df(city_geo)
city_info$lat <- as.numeric(city_info$lat)
city_info$lng <- as.numeric(city_info$lng)

#' save to tmp dir incase of restart. This would eliminate the need to rerun
#' the previous code.
saveRDS(city_info, "data/cafe_cities_geocoded.RDS")
