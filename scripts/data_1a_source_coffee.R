#' ////////////////////////////////////////////////////////////////////////////
#' FILE: data_1a_source_coffee.R
#' AUTHOR: David Ruvolo
#' CREATED: 2017-04-14
#' MODIFIED: 2020-02-05
#' PURPOSE: source data on specialty coffee in Europe
#' PACKAGES: rvest, magrittr, stringr
#' STATUS: working
#' COMMENTS:
#'      The purpose of this script is to source data on the European Specialty
#'      Coffee scene. This script will work by visiting the "all city guides"
#'      page and then extracting the country level data (name, links to country
#'      -level guides, etc.). This data will be used to pull city level data
#'      and then cafe level data. Ideally, this script should be run in the
#'      cloud and have good system pauses to prevent sever blacklisting. See
#'      the scripts/utils_0_scrape.R for data cleansing functions.
#' ////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))

# source functions
source("scripts/utils/utils_0_scrape.R")

# ////////////////////////////////////////

# ~ 0 ~
# SOURc_indexNG COUNTRIES
# get complete list of coutries + page urls

# set url
url <- "https://europeancoffeetrip.com/city-guides/"

# countries
countries_raw <- read_html(url, encoding = "utf8") %>%
    html_nodes(".countries-list .cg-single-item")


# loop through countries_raw object to evaluate if any text or urls is missin
c_index <- 1
c_max_reps <- length(countries_raw)
while (c_index <= c_max_reps) {

    # set tmp object for node
    tmp_country_raw <- countries_raw[c_index]
    tmp_out <- utils$getCountryInfo(tmp_country_raw)

    # create countries object
    if (c_index == 1) {
        countries_df <- tmp_out
    } else {
        countries_df <- rbind(countries_df, tmp_out)
    }

    # update counter + print status
    cat("Finished Country:", c_index, " of ", c_max_reps, "\n")
    c_index <- c_index + 1
}

# clean: remove vars
rm(
    list = c(
        "c_index",
        "c_max_reps",
        "tmp_country_link",
        "tmp_country_name",
        "tmp_country_raw",
        "tmp_out"
    )
)

# save countries
saveRDS(countries_df, "data/coffee_1_countries.RDS")

#' ////////////////////////////////////////

# ~ 1 ~
# SOURCING CAFES BY COUNTRY
#' get a complete list of coffee shops by country and city using the object
#' `countries` (defined above), we need to create a loop that pulls the list
#' of cafes per country guide. As this will allow us to extract cafe, url,
#' and cafe index in a single call. Inside the loop, there will also be a child
#' loop that extracts cafe-level data and evaluates it for missing data before
#' adding it to a country-level object, and then a European-wide object.
#' Depending on the size of the data, google maps api might be a better choice
#' for pulling additional geodata rather than loading each page, extracting
#' coordinates, etc.


# define parent and child loop
parent_index <- 1
parent_max_reps <- length(unique(countries_df$country))
css <- paste0(
    "#first-cafes a:not([target='_blank']):not([href='https://europeancoffeetrip.com/add-cafe/']),",
    "#rest-cafes a:not([target='_blank']):not([href='https://europeancoffeetrip.com/add-cafe/'])"
)
while (parent_index <= parent_max_reps) {

    # isolate data for current iteration
    tmp_data <- countries_df[parent_index, ]

    # query site and use error bounding to prevent breaking of loop
    response <- tryCatch({
            read_html(tmp_data$link, encoding = "utf8")
        },
        error = function(e) {
            FALSE
        }
    )

    # logic depending on response
    if (!isFALSE(response)) {

        # select cafe elements
        tmp_c_indexty_cafes_raw <- response %>% html_nodes(css)

        # run child loop to evaluate individual cafe titles and links
        child_index <- 1
        child_max_reps <- length(tmp_c_indexty_cafes_raw)
        while (child_index <= child_max_reps) {

            # filter parent object to current i
            tmp <- tmp_c_indexty_cafes_raw[chI]

            # pull cafe data
            out <- utils$getCafeInfo(data = tmp, country = tmp_data$country)

            # if bind data or create master data if it doesn't exist
            if (exists("cafes")) {
                cafes <- rbind(cafes, out)
            } else {
                cafes <- out
            }

            # update counter
            cat("\tBuilding data for cafe ", chI, " of ", child_max_reps, "\n")
            child_index <- child_index + 1
        }
    } else if (isFALSE(response)) {

        # log failed cases
        if (exists("failed")) {
            failed <- c(failed, tmp_data$country)
        } else {
            failed <- tmp_data$country
        }
    } else {
        cat("FATAL ERROR: this should never happen")
    }

    # update counter + display message + randomize sleep
    cat("Built data for country ", parent_index, " of ", parent_max_reps, "\n")
    parent_index <- parent_index + 1
    Sys.sleep(sample(2:25, 1))
}

# clean up old vars
rm(
    list = c(
        "parent_index",
        "parent_max_reps",
        "response",
        "tmp_data",
        "child_index",
        "child_max_reps",
        "out",
        "tmp",
        "tmp_c_indexty_cafes_raw"
    )
)

# malta guide does not exist on the site
# failed
# maltaLink <- countries_df$link[countries_df$country == "Malta"]
# maltaGuide <- read_html(maltaLink, encoding = "utf8")
# rm(failed)


# let's generate for cafes
cafes$cafeId <- paste0("cafe_", seq(1, NROW(cafes)))

# save cafe data
saveRDS(cafes, "data/coffee_2_countries_cities.RDS")


#' ////////////////////////////////////////

# ~ 2 ~
# PULL CAFE LEVEL DATA
# now that data for countries and c_indexties are pulled, it is possible to pull
# data for all cafes.

cafe_index <- 1
cafe_max_reps <- NROW(cafes)
while (cafe_index <= cafe_max_reps) {

    # print init msg
    cat("Geting data for cafe", cafe_index, "of", cafe_max_reps, "\n")

    # isolate data for current iteration
    tmp_cafe <- cafes[cafe_index, ]

    # response
    response <- tryCatch({
            read_html(tmp_cafe$link, encoding = "utf8")
        },
        error = function(e) {
            FALSE
        }
    )

    # process response
    if (!isFALSE(response)) {
        cat("\tSuccess: pulling data\n")
        info <- utils$getAllCafeLocation(response)
    } else {
        cat("\tFailed: sending blank DF instead\n")
        info <- utils$blankAllCafeLocation()
    }

    # cbind with tmp_cafe
    out <- cbind(tmp_cafe, info)

    # bind to master object
    if (exists("euCoffee")) {
        eu_coffee <- rbind(euCoffee, out)
    } else {
        eu_coffee <- out
    }

    # update counter + print msg
    cat("\tDone!\n")
    cafe_index <- cafe_index + 1
    Sys.sleep(runif(1, 1, 21.38))
}

# clean up vars
rm(list = c("cafe_index", "cafe_max_reps", "out", "tmp_cafe"))

# save
saveRDS(eu_coffee, "data/coffee_3_countries_cafes.RDS")