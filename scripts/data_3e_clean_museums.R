#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_3e_clean_museums.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-17
#' MODIFIED: 2020-02-17
#' PURPOSE: merge and clean geocoded data to create master object
#' STATUS: working; complete
#' PACKAGES: tidyverse;
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

#' pkgs
suppressPackageStartupMessages(library(tidyverse))

#' Data
museums_complete <- readRDS("tmp/museums_complete.RDS")
museums_incomplete <- readRDS("tmp/museums_incomplete.RDS")
museums_incomplete_geo <- readRDS("tmp/museums_incomplete_geocoded.RDS")


#'/////////////////////////////////////

#' ~ 0 ~
#' Clean Incomplete Geocoded Data
#' Remove incomplete cases or cases that failed during geocoding

#' Select and Rename Variables
vars <- c(
    "id" = "ids",
    "lat",
    "lon",
    "name",
    "addr_num" = "address.house_number",
    "addr_street" = "address.road",
    "addr_state" = "address.state",
    "addr_postcode"  = "address.postcode",
    "addr_country" = "address.country",
    "addr_country_code" = "address.country_code",
    "status"
)

#' Reduce Incomplete Geocoded Dataset
tomerge <- museums_incomplete_geo %>% 
    filter(status == "completed") %>%
    mutate(
        lat = ifelse(is.na(lat), NA_real_ ,as.numeric(lat)),
        lon = ifelse(is.na(lon), NA_real_, as.numeric(lon)),
        name = case_when(
            !is.na(namedetails.name) & !is.na(display_name) ~ namedetails.name,
            !is.na(namedetails.name) & is.na(display_name) ~ namedetails.name,
            is.na(namedetails.name) & !is.na(display_name) ~ substring(
                text = display_name,
                first = 1,
                last = as.numeric(
                    gregexpr(
                        pattern = ",",
                        text = display_name
                    )[[1]][1]
                ) - 1
            ),
            is.na(namedetails.name) & is.na(display_name) ~ NA_character_,
            TRUE ~ NA_character_
        )
    ) %>%
    select(., !!vars) %>%
    distinct(id, .keep_all = TRUE)

#'/////////////////////////////////////

#' ~ 1 ~
#' Merge Geocoded with Incomplete
#' Using the cleaned geocoded dataset, merge with the incomplete dataset
museums_incomplete <- museums_incomplete %>%
    left_join(tomerge, by = "id")


#' Write a function that evaluates x and y variables
tools <- list()
tools$eval_values <- function(x, y) {
    case_when(
        x == y ~ x,
        !is.na(x) & !is.na(y) ~ x,
        !is.na(x) & is.na(y) ~ x,
        is.na(x) & !is.na(y) ~ y,
        is.na(x) & is.na(y) ~ NA_character_,
        TRUE ~ NA_character_
    )
}

#' Process Variables
museums_incomplete <- museums_incomplete %>% 
    unite("lat", c(lat.x, lat.y), na.rm = TRUE) %>%
    unite("lon", c(lon.x, lon.y), na.rm = TRUE) %>%
    mutate(
        lat = as.numeric(str_replace_all(lat, "_NA", "")),
        lon = as.numeric(str_replace_all(lon, "_NA", "")),
        name = tools$eval_values(name.x, name.y),
        addr_num = tools$eval_values(addr_num.x, addr_num.y),
        addr_street = tools$eval_values(addr_street.x, addr_street.y),
        addr_postcode = tools$eval_values(addr_postcode.x, addr_postcode.y)
    ) %>%
    select(
        -type,
        -name.x, -name.y,
        -addr_num.x, -addr_num.y, 
        -addr_street.x, -addr_street.y, 
        -addr_postcode.x, -addr_postcode.y,
        -hasName, -hasCoords, -isPastRadius, -status,
    )

#'/////////////////////////////////////

#' ~ 2 ~ 
#' Merge Incomplete and Complete Datasets

#' Make sure Complete and Complete datasets have the same columns
NCOL(museums_complete) == NCOL(museums_incomplete)

#' Fix Museums Complete?
names(museums_incomplete)[!names(museums_incomplete) %in% names(museums_complete)]
museums_incomplete$addr_state <- NULL
museums_incomplete$addr_country <- NULL
museums_incomplete$addr_country_code <- NULL

#' Fix Museums Incomplete
names(museums_complete)[!names(museums_complete) %in% names(museums_incomplete)]
museums_complete$type <- NULL
museums_complete$hasName <- NULL
museums_complete$hasCoords <- NULL
museums_complete$isPastRadius <- NULL

#' Do they match?
sort(names(museums_complete)) == sort(names(museums_incomplete))

#' Merge
museums <- rbind(museums_complete, museums_incomplete) %>%
    arrange(city_id, id)




