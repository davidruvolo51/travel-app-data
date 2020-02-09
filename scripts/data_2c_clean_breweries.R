#'//////////////////////////////////////////////////////////////////////////////
#' FILE: data_2c_clean_breweries.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-09
#' MODIFIED: 2020-02-09
#' PURPOSE: reduce the raw breweries dataset
#' STATUS: complete; working
#' PACKAGES: tidyverse
#' COMMENTS: NA
#'//////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

# data
raw <- readRDS("data/breweries_1_merged_raw.RDS")

#'//////////////////////////////////////

#' ~ 1 ~
#' CLEAN DATASET
#' In this script, a number of transformations will be applied to the
#' merged raw dataset in order to get it into shape for use in the
#' visualizations. This includes renaming columns, condensing multiple
#' columns (i.e., website and website2), and re-ordering columns. Post-cleaning
#' it may be useful run another google maps API script to get reviews
#' and missing information.

#' ~ a ~
#' Print the head of the dataset and select the variables of interest
#' and define a new name for the variables. This will eliminate var
#' names such as "tags.addr:housenumber". Use select(., !!vars) to
#' select and rename variables in one go. Adjust these variables as needed.
vars <- c(
    "city_id" = "city_id",
    "city" = "city_id",
    "country" = "country",
    "id" = "id",
    "name" = "tags.name",
    # "name_alt" = "tags.alt_name", # removing this one
    "lat" = "lat",
    "lon" = "lon",
    "addr_num" = "tags.addr:housenumber",
    "addr_street" = "tags.addr:street",
    "addr_city" = "tags.addr:city",
    "addr_postcode" = "tags.addr:postcode",
    "phone" = "tags.phone",
    "website" = "tags.website",
    "website2" = "tags.contact:website",
    "amenity" = "tags.amenity",
    "has_food" = "tags.food",
    "has_real_ale" = "tags.real_ale",
    "has_real_cider" = "tags.real_cider",
    "has_outdoor_seating" = "tags.outdoor_seating",
    "shop" = "tags.shop",
    "cuisine" = "tags.cuisine",
    "note" = "tags.note"
)

# Reduce, Rename, and Reorder Dataset
breweries <- raw %>% select(., !!vars)


#' ~ b ~
#' Remove missing names. Since the queries returned `nodes` and `ways`, it
#' also returned a series of extra nodes for a given place (i.e., set of
#' coordinates). This means that a given place may have multiple coordinates
#' in the database. The `fromJSON` and `flatten` fuctions keep these
#' records so now we must remove them. I wouldn't use NROW or DIM outputs as
#' an indicator of the final dataset as these ouputs would be skewed by these
#' entries.
breweries <- breweries %>% filter(!is.na(name))


#' ~ c ~
#' At this point, the number of breweries is around 4k. Let's check the missing
#' variables.
sapply(seq_len(NCOL(breweries)), function(col) {
    list(
        name = names(breweries[col]),
        missing = NROW(breweries[is.na(breweries[, col]) == TRUE, ])
    )
})

#' There are quite a few missing values for many of the variables that I thought
#' were originally important or may be of use. In fact, the only variables that
#' could be used (and that I originally wanted) are name and coordinates.
#' Instead of cleaning this data, I will reduce the data to a few key variables
#' and run a google places API script to populate the rest of the data. Since I
#' wanted to get reviews and ratings data, I might as well grab place data while
#' I'm sending queries.

breweries <- breweries %>% select(city, country, id, name, lat, lon)

#' For missing geocoordinates, make sure these are retrieved from places API.
#' I'm not removing this script or eliminating any of the above code, in case
#' I want to add something in later.
saveRDS(breweries, "data/breweries_2_reduced.RDS")