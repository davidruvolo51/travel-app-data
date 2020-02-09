#' ////////////////////////////////////////////////////////////////////////////
#' FILE: data_2b_merge_breweries.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-08
#' MODIFIED: 2020-02-08
#' PURPOSE: merge breweries data located in data/breweries/* in 1 file
#' STATUS: complete; working
#' PACKAGES: tidyverse
#' COMMENTS: NA
#' ////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

#' //////////////////////////////////////

#' ~ 0 ~
# Create Object with All File Paths
files <- data.frame(
    id = NA,
    path = list.files("data/breweries", full.names = TRUE),
    stringsAsFactors = FALSE
)

# append city id as listed in file name
files$id <- as.character(
    sapply(seq_len(length(files$path)), function(n) {
        substring(
            text = files$path[n],
            first = as.numeric(
                gregexpr(
                    pattern = "_",
                    text = files$path[n]
                )[[1]][1]
            ) + 1,
            last = as.numeric(
                gregexpr(
                    pattern = "_",
                    text = files$path[n]
                )[[1]][2]
            ) - 1
        )
    })
)

#' //////////////////////////////////////

#' ~ 1 ~
# LOAD AND EXTRACT DATA BY FILE

# load geo cities object
geo <- readRDS("tmp/cafe_cities_geocoded.RDS")
geo$id <- rownames(geo)

# set loop params
n <- 1
reps <- NROW(files)
fails <- 0

# set columns of interest
cols <- c(
    "type",
    "id",
    "tags.name",
    "tags.alt_name",
    "lat",
    "lon",
    "tags.addr:city",
    "tags.addr:housenumber",
    "tags.addr:street",
    "tags.addr:postcode",
    "tags.phone",
    "tags.website",
    "tags.contact:website",
    "tags.amenity",
    "tags.food",
    "tags.real_ale",
    "tags.real_cider",
    "tags.outdoor_seating",
    "tags.shop",
    "tags.cuisine",
    "tags.note"
)

# run loop
while (n <= reps) {

    # msg
    # cat("\nStarting", n, "of", reps, "...")
    # load file => fromJSON => flatten
    f <- readLines(files$path[n])
    json <- jsonlite::fromJSON(f)

    # if elements isn't blank...
    if (length(json$elements) > 0) {

        data <- jsonlite::flatten(json$elements)

        #' select all the names from the `cols` object that apply
        extracted <- data %>% select_if(names(.) %in% cols)

        #' find and create missing cols in new object (apply a
        #' NA for missing columns)
        missing_cols <- cols[!cols %in% names(extracted)]
        sapply(seq_len(length(missing_cols)), function(c) {
            extracted[[missing_cols[c]]] <<- NA_character_
        })

        # order extracted dataset
        extracted <- extracted %>% select(cols)

        # append city, country, and id
        extracted$city_id <- files$id[n]
        extracted$city <- geo$city[geo$id == files$id[n]]
        extracted$country <- geo$country[geo$id == files$id[n]]

        # create master object based on n count
        if (n == 1) {
            master <- extracted
        } else {
            master <- rbind(master, extracted)
        }

    } else {

        # save failed cases to object `failed`
        fails <- fails + 1
        failed_city <- geo[geo$id == files$id[n], ]

        # append current failed item
        if (fails == 1) {
            failed <- failed_city
        } else {
            failed <- rbind(failed, failed_city)
        }
    }

    # update counter
    cat("\nCompleted", n, "of", reps)
    n <- n + 1
}

#' For failed cities, it is difficult to determine if a city does not have
#' brewery(ies), the buildings were not tagged correctly on OSM, or they were
#' excluded due to the structure of the query. It might also be due to the
#' search radius. I used a search radius of 35km so perhaps some breweries
#' were outside of the search radius or that the city gecodes were centered
#' outside of the city center or were skewed to any direction (i.e., north,
#' south, etc.).
#'
#' Save failed object for documentation
saveRDS(failed, "tmp/failed_brewery_cities.RDS")

#' Save master object
saveRDS(master, "data/breweries_1_merged_raw.RDS")