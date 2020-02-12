#'//////////////////////////////////////////////////////////////////////////////
#' FILE: data_2c_clean_breweries.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-09
#' MODIFIED: 2020-02-12
#' PURPOSE: geocode breweries and prepare for viz
#' STATUS: complete; working
#' PACKAGES: tidyverse
#' COMMENTS: NA
#'//////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(httr))

# data
raw <- readRDS("data/breweries_1_merged_raw.RDS")

#'//////////////////////////////////////

#' ~ 0 ~
#' Define functions
tools <- list()

#' define a function that evaluates the `website` and `website2` vars
tools$evaluate_websites <- function(x, y) {
    
    # if both are NA || only X is not NA || only Y is not NA
    if (is.na(x) & is.na(y)) return(NA)
    if (!is.na(x) & is.na(y)) return(x)
    if (is.na(x) & !is.na(y)) return(y)
    
    # if both aren't NA
    if (!is.na(x) & !is.na(y)) {
        
        # if both are identical return x
        if (x == y) return(x)
        
        # if strings aren't identical
        if (x != y) {
            
            # if https and http are removed, are they still the same?
            x2 <- gsub(pattern = "http://|https://|/", replacement = "", x = x)
            y2 <- gsub(pattern = "http://|https://|/", replacement = "", x = y)
            
            # if they are the same
            if (x2 == y2) {
                return(x)
            }
            
            # if they aren't the same, return both
            if (x2 != y2) {
                return(paste0(x, "; ", y))
            }
        }
    }
}

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
    "city" = "city",
    "country" = "country",
    "id" = "id",
    "name" = "tags.name",
    # "name_alt" = "tags.alt_name", # removing this one
    "lat" = "lat",
    "lon" = "lon",
    # "addr_num" = "tags.addr:housenumber",
    # "addr_street" = "tags.addr:street",
    # "addr_city" = "tags.addr:city",
    # "addr_postcode" = "tags.addr:postcode",
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
        missing = NROW(breweries[is.na(breweries[, col]) == TRUE, ]),
        percent = round(
            (NROW(breweries[is.na(breweries[, col]) == TRUE, ]) / NROW(breweries) * 100),
            2
        )
    )
})


#' ~ d ~
#' Removing uncessary/incomplete data from breweries data. Originally, I wanted
#' to use google places API. However, after clicking around in the accounts and
#' billing pages, I must have changed something that triggered my daily quota to
#' stop at 1 request per day. As a result, I'm not longer using google's API as
#' the documentation is too confusing to determine what is free and what I will
#' have to pay for. Instead, I've decided not to pursue ratings for breweries
#' (for now). There are a number of options to get additional dat though. I will
#' keep information about the place (i.e., has food, cider, etc.), but I will
#' use photon.komoot.de to reverse geocode coordinates. Let's do this in a few
#' steps. First, remove all addresses (in the cols var above). Second, write a
#' function that evaluates website vars and returns a single website. Then
#' replace address by reverse geocoding.

# evaluate website links
breweries$link <- as.character(
    sapply(
        seq_len(length(breweries$website)),
        function(d) {
            tools$evaluate_websites(
                x = breweries$website[d],
                y = breweries$website2[d]
            )
        }
    )
)

# remove old website vars
breweries$website <- NULL
breweries$website2 <- NULL

#'//////////////////////////////////////

#' ~ b ~
#' Reverse geocode data

# init vars - uncomment if running for the first time
# breweries$addr_housenumber <- NA
# breweries$addr_street <- NA
# breweries$addr_postcode <- NA
# breweries$addr_state <- NA
# breweries$addr_city <- NA
# breweries$addr_country <- NA
# breweries$addr_country_code <- NA
# breweries$addr_neighborhood <- NA
# breweries$status <- NA


# define a function that standarizes GET request
tools$new_request <- function(id, type = "N", format = "json") {
    prefix <- "https://nominatim.openstreetmap.org/lookup?"
    output <- paste0("&format=", format)
    return(paste0(prefix, "osm_ids=", type, id, output))
}

# load tmp object (if failed)
breweries <- readRDS("tmp/breweries_first_900.RDS")

# set loop props
index <- 901
reps <- NROW(breweries)
save_array <- seq.int(from = 1000, to = reps, by = 200)
while (index <= reps) {
    
    # start
    cat("\nStarting entry", index, "of", reps)
    
    # build new query and send query
    cat("\n\tSending request...")
    query <- tools$new_request(id = breweries$id[index])
    response <- httr::GET(query, httr::timeout(20))
    
    # process response
    if (response$status_code == 200) {
        
        # update user
        cat("Sucess!")
        cat("\n\tPulling data...")
        
        # pull data
        result <- httr::content(response, "text", encoding = "utf-8")
        
        # validating result
        if(result != "[]") {
            
            # continue extracting result
            result <- result %>% 
                jsonlite::fromJSON(.) %>%
                jsonlite::flatten(.)
            
            # define variables to pull
            cols <- c(
                "osm_id", "lat", "lon",
                "address.house_number", "address.road", "address.neighbourhood",
                "address.city", "address.state", "address.postcode",
                "address.country", "address.country_code"
            )
            
            # pull standard variables and fill in missing if applicable
            extracted <- result %>% select_if(names(.) %in% cols)
            missing <- cols[!cols %in% names(extracted)]
            if (length(missing) > 0) {
                cat("\n\tFixing missing colums...")
                sapply(seq_len(length(missing)), function(d) {
                    extracted[[missing[d]]] <<- NA_character_
                })
            }
            
            # append extracted to parent object
            cat("\n\tAppending Data...")
            breweries$addr_housenumber[index] <- extracted$address.house_number
            breweries$addr_street[index] <- extracted$address.road
            breweries$addr_postcode[index] <- extracted$address.postcode
            breweries$addr_state[index] <- extracted$address.state
            breweries$addr_city[index] <- extracted$address.city
            breweries$addr_country[index] <- extracted$address.country
            breweries$addr_country_code[index] <- extracted$address.country_code
            breweries$addr_neighborhood[index] <- extracted$address.neighbourhood
            breweries$status[index] <- "success"
            
            # append lat and lon if missing
            if (is.na(breweries$lat[index]) & !is.na(extracted$lat)) {
                cat("\n\tAdding missing latitude value...")
                breweries$lat[index] <- extracted$lat
            }
            if (is.na(breweries$lon[index]) & !is.na(extracted$lon)) {
                cat("\n\tAdding missing longitude value...")
                breweries$lon[index] <- extracted$lon
            }
            
            # DONE!
            cat("\n\tDone!")
        } else {
            cat("\n\tResponse was empty... :-(")
            breweries$status[index] <- "empty_response"
        }
        
    } else {
        # update user
        cat("Failed!")
        breweries$status[index] <- response$status_code
    }
    
    # save data in case of emergency
    if(index %in% save_array) {
        cat("\nSTATUS: Saving 'breweries' object in case of emergency.\n")
        saveRDS(breweries, paste0("tmp/breweries_first_",index,".RDS"))
    }
    
    # update
    cat("\nComplete!")
    index <- index + 1
    
    # add pause
    Sys.sleep(runif(1, 10, 15))
}
