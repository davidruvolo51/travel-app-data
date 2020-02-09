#'//////////////////////////////////////////////////////////////////////////////
#' FILE: data_2d_geocode_breweries.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-09
#' MODIFIED: 2020-02-09
#' PURPOSE: run google places API to geocode breweries
#' STATUS: in.progress
#' PACKAGES: googleway
#' COMMENTS: NA
#'//////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(googleway))

# source functions
source("scripts/utils/utils_0_scrape.R")

# data
df <- readRDS("data/breweries_2_reduced.RDS")

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

#'//////////////////////////////////////

#' ~ 1 ~
#' GOOGLE PLACE DETAILS
#' build a loop that sends a query to google places API and returns data about
#' a given location (i.e., ratings, price level, hours, urls, etc.).

# set key
key <- "ENTER_API_KEY_HERE"


# build loop
i <- 1
reps < NROW(df)

# loop
while (i <= reps) {

    # starting message
    cat("Querying Place", i, "of", "reps")

    # set current item to local object
    tmpData <- df[i, ]

    # build query
    q <- paste0(tmpData$name, " ", tmpData$city, " ", tmpData$country)

    # run query
    response <- google_places(search_string = q, key = key)

    # process data based on response
    if (response$status == "OK") {

        # send notice
        cat("\tResponse is okay\n")
        cat("\tRunning second query...")

        # send request
        Sys.sleep(runif(1, 0, 3))
        details <- google_place_details(response$results$place_id, key = key)

        # process based on response
        if (details$status == "OK") {

            # send message
            cat("success!\n")
            cat("\tExtracting data")

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