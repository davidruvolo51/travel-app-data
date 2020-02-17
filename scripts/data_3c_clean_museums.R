#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_3b_clean_museums.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-15
#' MODIFIED: 2020-02-16
#' PURPOSE: clean museums data
#' STATUS: working; complete
#' PACKAGES: tidyverse; geosphere
#' COMMENTS: The purpose of this file is to reduce the dataset to a manageable
#'     size for geocoding.
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))

# load data
museums <- readRDS("tmp/raw_merged_museums.RDS")

# load utils
source("scripts/utils/utils_99_general.R")

#'/////////////////////////////////////

#' ~ 0 ~
#' Evaluate Dataset
#' Unlike the breweries dataset, I'm cleaning the dataset prior to running
#' additional geocoding (which I haven't figured out if I'm running or not).

#' Change ID to Character
sapply(museums, class)
museums$id <- as.character(museums$id)

#' Evaluate Missing Cases
tools$find_missing_cases(data = museums)


#' Check Data To Identify Missing Information
#' verify records of type way and relation to see if they exist in nodes.
#' Could the data be split and then merged?

#' way
museums %>%
    filter(type == "way") %>%
    tools$find_missing_cases(data = .)

#' relation
museums %>%
    filter(type == "relation") %>%
    tools$find_missing_cases(data = .)

museums %>%
    filter(type == "relation") %>%
    head()

museums %>% filter(id == 29795)
museums %>% filter(id == "10624287")

#' The issue is that there are multiple entries for the same location and some
#' enties do not have a name. It is impossible to check all entries as it would
#' require manual verifcation of each location. At this point, I think defining
#' selection criteria would help manage this. 
#' 
#' 1. Identify duplicated entries by evaluating if a given id appears in the a
#'    list of unique IDs more than once.
#' 2. Determine which cases have names and which
#'    ones do not.
#' 3. Determine which cases that have names also have coordinates.
museums <- museums %>% 
    mutate(
        duplicated = duplicated(id),
        hasName = case_when(
            is.na(name) ~ FALSE,
            !is.na(name) ~ TRUE
        ),
        hasCoords = case_when(
            !is.na(lat) & !is.na(lon) ~ TRUE,
            TRUE ~ FALSE
        )
    )

#' Summarize Flags
museums %>% 
    group_by(duplicated, hasName, hasCoords) %>% 
    summarize(
        n = n(),
        rate = paste0(round(n() / NROW(.), 2) * 100, "%")
    )

#'/////////////////////////////////////

#' ~ 1 ~
#' Remove the Least Helpful Cases
#' The least helpful cases are the entries that are duplicate ids and do not
#' have neither names nor coordinates. The simplest option is to remove them.
#' However, these cases need to be identified a bit further to make sure
#' we are removing the right cases.
museums %>%
    filter(duplicated == TRUE, hasName == TRUE, hasCoords == TRUE) %>% 
    head()
    # tail()
    # .[4000,]

#' Check a few random cases
museums %>% filter(id == "743300794")
museums %>% filter(id == "5346672322")
museums %>% filter(id == "2720969403")

#' It's likely that -- like the brewery dataset -- there are multiple entries
#' that are duplicated, but have slightly different addresses or due to the
#' search radius, the same location was captured multiple times for cities
#' close in proximity. Create a flag that determines if an ID is one of the
#' 6795 cases. An additional flag has to be created as we do not want to
#' remove the first instance of the entry. 
museums <- museums %>%
    mutate(
        shouldRemove = case_when(
            (duplicated == TRUE & hasName == TRUE & hasCoords == TRUE) ~ TRUE,
            TRUE ~ FALSE
        )
    )

#' Double Check
museums %>% group_by(shouldRemove) %>% count()
museums %>% filter(shouldRemove == TRUE) %>% distinct(id) %>% pull() %>% length()
museums %>% filter(shouldRemove == TRUE) %>% head()
museums %>% filter(shouldRemove == TRUE) %>% tail()

#' Remove true duplicated cases
museums <- museums[!(museums$duplicated == TRUE & museums$shouldRemove == TRUE),]

#'/////////////////////////////////////

#' ~ 2 ~
#' Remove the Next Least Helpful Duplicates
#' After removing TRUE duplicates, let's see what else can be excluded. First,
#' update the flags to re-evaluate
museums <- museums %>% 
    mutate(
        duplicated = duplicated(id),
        hasName = case_when(
            is.na(name) ~ FALSE,
            !is.na(name) ~ TRUE
        ),
        hasCoords = case_when(
            !is.na(lat) & !is.na(lon) ~ TRUE,
            TRUE ~ FALSE
        )
    )

#' Summarize Flags
museums %>% 
    group_by(duplicated, hasName, hasCoords) %>% 
    summarize(
        n = n(),
        rate = paste0(round(n() / NROW(.), 2) * 100, "%")
    )


#' I think it's safe to remove the cases that are duplicated and do not have
#' a name or coordinates. These are pretty useless. Sorry, data!
museums <- museums[!(
        museums$duplicated == TRUE & 
            museums$hasName == FALSE & 
            museums$hasCoords == FALSE
        ),]

#' Rerun the flags and re-evaluate

#'/////////////////////////////////////

#' ~ 3 ~
#' Salvage Remaining Duplicates
#' The key here is to merge the duplicated cases with the non-duplicated
#' cases to reduce the number of cases that need to be geocoded; this will
#' save a lot of time! This will happen in a few steps.
#'
#'     1. Isolate entries WITH names
#'     2. Isolate entries WITH coordinates
#'     3. Remove duplicated entries from the main dataset
#'     4. Merge isolated objects with main dataset
#'     5. Rerun flags and summarize
#'
#' Create isolated objects by defining a filter for each flag. Also
#' reduce dataset to the appropriate columns only, as well as selecting
#' cases where there are distinct values
dup_names_only <- museums %>%
    filter(duplicated == TRUE, hasName == TRUE, hasCoords == FALSE) %>%
    select(id, name) %>%
    distinct(id, name)

dup_coords_only <- museums %>%
    filter(duplicated == TRUE, hasName == FALSE, hasCoords == TRUE) %>%
    select(id, lat, lon) %>%
    distinct(id, lat, lon)

#' Check Objects - make sure dims and IDs match
dup_names_only %>% dim()
length(unique(dup_names_only$id))

dup_coords_only %>% dim()
length(unique(dup_coords_only$id))


#' Safely remove values missing these from the master data
museums <- museums[!(museums$duplicated == TRUE), ]

#' Merge Names with Master Data
museums_names_joined <- museums %>% 
    left_join(dup_names_only, by = "id", suffix = c("_a","_b"))
    
#' Fix Multiple Name Columns
museums_names_joined <- museums_names_joined %>%
    mutate(
        name = case_when(
            name_a == name_b ~ name_a,
            !is.na(name_a) & !is.na(name_b) ~ name_a,
            !is.na(name_a) & is.na(name_b) ~ name_a,
            is.na(name_a) & !is.na(name_b) ~ name_b,
            is.na(name_b) & is.na(name_b) ~ NA_character_,
            TRUE ~ NA_character_
        )
    ) %>%
    select(-name_a, -name_b)

#' Check Outputs
dim(museums)
dim(museums_names_joined)
tools$find_missing_cases(data = museums_names_joined)


#' Merge Coords Data with Master Data
museums_coords_joined <- museums_names_joined %>%
    left_join(dup_coords_only, by = "id", suffix = c("_a", "_b"))

#' Collapse Multiple Columns
museums_coords_joined <- museums_coords_joined %>%
    mutate(
        lat = case_when(
            lat_a == lat_b ~ lat_a,
            !is.na(lat_a) & !is.na(lat_b) ~ lat_a,
            !is.na(lat_a) & is.na(lat_b) ~ lat_a,
            is.na(lat_a) & !is.na(lat_b) ~ lat_b,
            is.na(lat_a) & is.na(lat_b) ~ NA_real_,
            TRUE ~ NA_real_
        ),
        lon = case_when(
            lon_a == lon_b ~ lon_a,
            !is.na(lon_a) & !is.na(lon_b) ~ lon_a,
            !is.na(lon_a) & is.na(lon_b) ~ lon_a,
            is.na(lon_a) & !is.na(lon_b) ~ lon_b,
            is.na(lon_a) & is.na(lon_b) ~ NA_real_,
            TRUE ~ NA_real_
        )
    ) %>%
    select(-lat_a, -lat_b,-lon_a, -lon_b)

#' Check Dims
dim(museums_names_joined)
dim(museums_coords_joined)

#' Update flags and re-evaluate
museums_coords_joined <- museums_coords_joined %>% 
    mutate(
        duplicated = duplicated(id),
        hasName = case_when(
            is.na(name) ~ FALSE,
            !is.na(name) ~ TRUE
        ),
        hasCoords = case_when(
            !is.na(lat) & !is.na(lon) ~ TRUE,
            TRUE ~ FALSE
        )
    )

#' Summarize Flags
museums_coords_joined %>% 
    group_by(duplicated, hasName, hasCoords) %>% 
    summarize(
        n = n(),
        rate = paste0(round(n() / NROW(.), 2) * 100, "%")
    )

#' After all of this, none of the duplicated cases could be salvaged.
#' Let's attempt geocoding of the dataset, it might go smoothly given
#' that 7% of the data is complete. However, I think we can trim these
#' cases down a bit more.

#'/////////////////////////////////////


#' ~ 4 ~
#' Calculate Distances of each point and filter
#' Part of the problem with the original request is that the radius of 35km
#' was a too large. For larger cities, a 35km radius would help capture places
#' on the outside of the cities, but that also meant that nearby cities would
#' be included too. For smaller countries, the search span is greater. I'm
#' going to calcuate the distance from the center point to about 16km (~10mi)
#' to see if that can trim the data down a bit further and provide a better
#' search radius for the app. Alternatively, it would be possible to look up
#' each city and set a radius, but it might be better to do it systematically
#' or at least standarize the search radius for all cities.
library(geosphere)

#' First, attach the city level coorinates to the master dataset
museums_geo <- museums_coords_joined %>%
    mutate(city_id = as.character(city_id)) %>%
    left_join(
        geo %>% select("city_id" = id, "city_lat" = lat, "city_lon" = lng), 
        by = c("city_id")
    )

#' Next, calcuate the distance of each point from the center
museums_geo <- museums_geo %>%
    mutate(
        dist_from_city = purrr::pmap_dbl(., ~ 
               distm(
                x = c(..16, ..15),
                y = c(..18, ..17),
                fun = distHaversine
            )
        )
    )

#' And create a logical flag for 16km
museums_geo$isPastRadius <- ifelse(museums_geo$dist_from_city > 16000, TRUE, FALSE)

#' Find unique cases
museums_geo %>% distinct(isPastRadius)
museums_geo %>% filter(is.na(isPastRadius)) %>% head()

#' Summarize
museums_geo %>%
    group_by(isPastRadius) %>%
    count()

#' I think I'm going to have to draw a line here. It's ok to work with a
#' dataset that is reduced to those within the search radius. For NA cases, I
#' will keep them for now. It's difficult to say if they will be updated during
#' geocoding, but it's worth a shot. When the geocoding is complete, limit
#' the data by search radius to 16k meters before sending the data for use
#' in the app. A 16km radius seems to be fair as this would capture most places
#' for larger cities and for smaller cities it would also include museums in
#' the surround area. The rationale for reducing the dataset this much is to
#' provide a accurrate city-level recommendation and balancing the time for
#' additional geocoding.
#'
#' I can now safely remove cases and drop flag columns. Save the hasName and
#' hasCoords columns as this would be useful for running the query
museums_final <- museums_geo %>% 
    filter(isPastRadius %in% c(FALSE, NA)) %>%
    select(-duplicated, -shouldRemove)

dim(museums_final)
tools$find_missing_cases(data = museums_final)

#' Save data
saveRDS(museums_final, "tmp/museums_merged_pre_geocode.RDS")

