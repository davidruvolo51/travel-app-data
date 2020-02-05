#' ////////////////////////////////////////////////////////////////////////////
#' FILE: data_04_analyze.R
#' AUTHOR: David Ruvolo
#' CREATED: 22 November 2019
#' MODIFIED: 22 November 2019
#' PURPOSE: analyze coffee data
#' PACKAGES: see below
#' STATUS: in.progress
#' COMMENTS: NA
#' ////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

# load data
coffee <- readRDS("data/eu_coffee_final.RDS")

#' ////////////////////////////////////////

# ~ 0 ~
# CREATE META OBJECT

meta <- list()
meta$summary <- list()

# the number of failed and successful responses
meta$summary$status <- coffee %>%
    group_by(status) %>%
    count() %>%
    as.data.frame() %>%
    bind_rows(
        data.frame(
            status = "total",
            n = NROW(coffee)
        )
    )


# number of missing values by column
meta$summary$na_count <- coffee %>%
    sapply(., function(x) {
        NROW(which(is.na(x)))
    }) %>%
    as.data.frame(optional = TRUE) %>%
    bind_cols(vars = row.names(.)) %>%
    select(vars, count = V1) %>%
    mutate(
        rate = round(count / NROW(coffee), 2)
    )


#' ////////////////////////////////////////

# ~ 1 ~
# CREATE ANALYSIS OBJECT

analysis <- list()
analysis$highlights <- list()
analysis$summary <- list()

# total number of countries
analysis$highlights$countries <- coffee %>%
    distinct(country) %>%
    count() %>%
    pull()

# total number of cities
analysis$highlights$cities <- coffee %>%
    distinct(city) %>%
    count() %>%
    pull()

# total number of cafes
analysis$highlights$cafes <- coffee %>%
    distinct(name) %>%
    count() %>%
    pull()

# average rating across all cafes
analysis$highlights$avgRating <- coffee %>%
    filter(!is.na(rating)) %>%
    summarize(
        rating = mean(rating),
        n = NROW(coffee) - (NROW(coffee) - n())
    )

# summarize data by country
# the number of cities, cafes, number of reviews, avg rating, sd rating, price level
# and the most popular opening hours
analysis$summary$coffeeByCountry <- coffee %>%
    group_by(country) %>%
    summarize(
        cities = length(unique(city)),
        cafes = length(unique(name)),
        user_ratings_total = sum(user_ratings_total, na.rm = TRUE),
        rating_avg = mean(rating, na.rm = TRUE),
        rating_sd = sd(rating, na.rm = TRUE),
        price_level = median(price_level, na.rm = TRUE)
    ) %>%
    inner_join(

        # join the most frequent opening hours
        coffee %>%

            # select columns
            select(
                country,
                monday,
                tuesday,
                wednesday,
                thursday,
                friday,
                saturday,
                sunday
            ) %>%

            # group by country + transform wide to long
            group_by(country) %>%
            gather(
                var,
                time,
                monday,
                tuesday,
                wednesday,
                thursday,
                friday,
                saturday,
                sunday
            ) %>%

            # remove extra variables, ungroup and remove odd cases
            select(country, time) %>%
            ungroup() %>%
            filter(!is.na(time), time != "Closed") %>%

            # regroup by country and time to find the most popular time
            group_by(country, time) %>%
            count() %>%
            arrange(country, -n) %>%

            # regroup by country and select the top time
            ungroup() %>%
            group_by(country) %>%
            top_n(1) %>%

            # select final vars for join
            select(country, "opening_hours" = time),
        by = c("country" = "country")
    ) %>%
    as.data.frame()

# save
saveRDS(analysis, "data/coffee_analysis_country.RDS")
