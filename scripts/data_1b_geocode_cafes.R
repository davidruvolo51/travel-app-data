#' ////////////////////////////////////////////////////////////////////////////
#' FILE: data_1b_geocode_cafes.R
#' AUTHOR: David Ruvolo
#' CREATED: 2019-11-22
#' MODIFIED: 2020-02-05
#' PURPOSE: fetch place information via google maps api
#' PACKAGES: tidyverse; googleway;
#' STATUS: working
#' COMMENTS:
#'      This script is designed to be run after script data_1a_source. Using
#'      the last output from that script, this script will fetch further
#'      information about each cafe using the google way package. This will
#'      extract information like hours, ratings, price, and links to the cafe's
#'      location on google maps. The output from this script will then be used
#'      to extract data on breweries in each city. After running this script,
#'      you can now start pulling data on breweries.
#' ////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(googleway))

# utils
source("scripts/utils/utils_0_scrape.R")
rm(utils)

# data
df <- readRDS("data/eu_coffee.RDS")

# init vars
df$place_id <- NA
df$maps_link <- NA
df$cafe_website <- NA
df$user_ratings_total <- NA
df$rating <- NA
df$price_level <- NA
df$monday <- NA
df$tuesday <- NA
df$wednesday <- NA
df$thursday <- NA
df$friday <- NA
df$saturday <- NA
df$sunday <- NA
df$google_status <- NA

#' ////////////////////////////////////////


# ~ 1 ~
# GOOGLE PLACE DETAILS
#' build a loop that queries google places API and retrieves data about the
#' place this includes ratings, price level, opening hours, maps url, as well
#' as updates missing address and location data

# set key
key <- "ENTER_API_KEY_HERE"

# build loop
i <- 1
maxreps <- NROW(df)
while (i <= maxreps) {

    # starting message
    cat("Querying Cafe", i, "of", maxreps, "\n")

    # set current item to local object
    tmpData <- df[i, ]

    # build query
    q <- google$buildQuery(tmpData)

    # run initial query to grab place id
    response <- google_places(search_string = q, key = key)

    if (response$status == "OK") {

        # response ok
        cat("\tResponse is okay\n")
        cat("\tRunning second query...")

        # request
        Sys.sleep(runif(1, 0, 3))
        details <- google_place_details(response$results$place_id, key = key)

        if (details$status == "OK") {

            # response ok
            cat("success!\n")
            cat("\tPulling data...")

            # build data
            df$place_id[i] <- ifelse(is.null(details$result$place_id), NA, details$result$place_id)
            df$maps_link[i] <- ifelse(is.null(details$result$url), NA, details$result$url)
            df$cafe_website[i] <- ifelse(is.null(details$result$website), NA, details$result$website)
            df$user_ratings_total[i] <- ifelse(is.null(details$result$user_ratings_total), NA, details$result$user_ratings_total)
            df$rating[i] <- ifelse(is.null(details$result$rating), NA, details$result$rating)
            df$price_level[i] <- ifelse(is.null(details$result$rating), NA, details$result$rating)

            # process opening hours
            tmpOpeningHours <- google$formatPlaceHours(details$result$opening_hours$weekday_text)
            df$monday[i] <- tmpOpeningHours$monday
            df$tuesday[i] <- tmpOpeningHours$tuesday
            df$wednesday[i] <- tmpOpeningHours$wednesday
            df$thursday[i] <- tmpOpeningHours$thursday
            df$friday[i] <- tmpOpeningHours$friday
            df$saturday[i] <- tmpOpeningHours$saturday
            df$sunday[i] <- tmpOpeningHours$sunday

            # evaluate if location is missing
            if (is.na(tmpData$address)) {
                df$address[i] <- details$result$formatted_address
            }

            if (is.na(tmpData$lng)) {
                df$lng[i] <- details$result$geometry$location$lng
            }

            if (is.na(tmpData$lat)) {
                df$lat[i] <- details$result$geometry$location$lat
            }

            # set google status
            df$google_status[i] <- "success"

            cat("Done!\n")
        } else {

            # response false
            cat("failed!\n")
            cat("\tSending blank data instead.\n")

            # send blank data
            df$place_id[i] <- NA
            df$maps_link[i] <- NA
            df$cafe_website[i] <- NA
            df$user_ratings_total[i] <- NA
            df$rating[i] <- NA
            df$price_level[i] <- NA
            df$monday[i] <- NA
            df$tuesday[i] <- NA
            df$wednesday[i] <- NA
            df$thursday[i] <- NA
            df$friday[i] <- NA
            df$saturday[i] <- NA
            df$sunday[i] <- NA
            df$google_status[i] <- "failed"
        }
    } else {

        # response false
        cat("\tResponse failed")

        # sending blank data instead
        cat("\tSending blank data instead\n")
        df$place_id[i] <- NA
        df$maps_link[i] <- NA
        df$cafe_website[i] <- NA
        df$user_ratings_total[i] <- NA
        df$rating[i] <- NA
        df$price_level[i] <- NA
        df$monday[i] <- NA
        df$tuesday[i] <- NA
        df$wednesday[i] <- NA
        df$thursday[i] <- NA
        df$friday[i] <- NA
        df$saturday[i] <- NA
        df$sunday[i] <- NA
        df$google_status[i] <- "failed"
    }

    # update counter
    cat("\tFinished.\n")
    i <- i + 1
}


# save data
df$google_status <- NULL
saveRDS(df, "data/coffee_all_cafes.RDS")
