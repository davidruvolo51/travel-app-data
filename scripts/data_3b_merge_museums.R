#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_3b_merge_museums.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-15
#' MODIFIED: 2020-02-15
#' PURPOSE: merges raw museums file into a single object
#' STATUS: working
#' PACKAGES: tidyverse
#' COMMENTS: This script loads and merges all json files into a single object.
#'     The data is parsed from json format into R list objects. The returned
#'     data is processed based on the existence of json$elements and element
#'     type (i.e., node, way, relation) returned by overpass turbo. The
#'     output is a merged dataset which is read into the next file data_3c-*.
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
suppressPackageStartupMessages(library(tidyverse))


# tools
source("scripts/utils/utils_99_general.R")

#'////////////////////////////////////////////////////////////////////////////

#' ~ 0 ~
#' Create a list of all data files
files <- data.frame(
    id = NA,
    path = list.files("data/museums", full.names = TRUE),
    stringsAsFactors = FALSE
)

#' append city id as listed in the file name
files$id <- as.numeric(
    sapply(
        seq_len(length(files$path)), function(n) {
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
        }
    )
)

#' reorder data
files <- files[order(files$id),]

#' For sanity, check to make sure all of the ids in the file paths match the
#' total number of cities
#' seq.int(1, 431, 1) == files$id

#'////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' LOAD AND EXTRACT DATA BY FILE

#' load geo cities data
geo <- readRDS("data/cafe_cities_geocoded.RDS")
geo$id <- rownames(geo)

# set loop params
n <- 1
reps <- 1#NROW(files)
fails <- 0

#' define columns of interest
cols <- c(
    "type",
    "id",
    "tags.name",
    "lat",
    "lon",
    "tags.addr:city",
    "tags.addr:housenumber",
    "tags.addr:street",
    "tags.addr:postcode"
)

#' Run Loop
while (n <= reps) {
    #' load file
    f <- readLines(files$path[n])
    json <- jsonlite::fromJSON(f)
    
    #' if 'elements' element is not blank...
    if (length(json$elements) > 0) {
        
        #' extract NRWs from json$elements and bind to master object
        available_elements <- unique(json$elements$type)
        
        #' Process Nodes
        if ("node" %in% available_elements) {
            nodes <- json$elements %>%
                filter(type == "node") %>%
                jsonlite::flatten() %>%
                select_if(names(.) %in% cols)
            
            #' Find Missing Columns in Nodes and Create NA Columns
            if(NCOL(nodes) != length(cols)) {
                missing_nodes <- cols[!cols %in% names(nodes)]
                sapply(seq_len(length(missing_nodes)), function(node) {
                    nodes[[missing_nodes[node]]] <<- NA
                })
            }
            
            #' Reorder and Push Data
            #' (since it's the first object, assign to parent)
            nodes <- nodes %>% select(cols)
            data <- nodes
        }
        
        #' Process Way
        if ("way" %in% available_elements) {
            ways <- json$elements %>%
                filter(type == "way") %>%
                jsonlite::flatten() %>%
                select_if(names(.) %in% cols)
            
            #' Find Missing Columns in Ways and Create NA Columns
            if (NCOL(ways) != length(cols)) {
                missing_ways <- cols[!cols %in% names(ways)]
                sapply(seq_len(length(missing_ways)), function(way) {
                    ways[[missing_ways[way]]] <<- NA
                })
            }
            
            #' Reorder and Push Data
            ways <- ways %>% select(cols)
            data <- rbind(data, ways)
        }
        
        #' Process Relation
        if ("relation" %in% available_elements) {
            relations <- json$elements %>%
                filter(type == "relation") %>%
                jsonlite::flatten() %>%
                select_if(names(.) %in% cols)
            
            #' Find Missing Columns in Relations and Create NA columns
            if (NCOL(relations) != length(cols)) {
                missing_relations <- cols[!cols %in% names(relations)]
                sapply(seq_len(length(missing_relations)), function(relation) {
                  relations[[missing_relations[relation]]] <<- NA  
                })
            }
            
            #' Reorder and Push Data
            relations <- relations %>% select(cols)
            data <- rbind(data, relations)
        }
        
        #' Add Final Variables
        data$city_id <- files$id[n]
        data$city <- geo$city[geo$id == files$id[n]]
        data$country <- geo$country[geo$id == files$id[n]]
        
        #' Create Master Object
        if (n == 1) {
            master <- data
        } else {
            master <- rbind(master, data)
        }
    } else {
        #' Save Failed Cases to Object `failed`
        fails <- fails + 1
        failed_city <- geo[geo$id == files$id[n], ]
        
        #' Append Current Failed Item
        if (fails == 1) {
            failed <- failed_city
        } else {
            failed <- rbind(failed, failed_city)
        }
    }
    
    #' Update Counter and Print Progress
    cat("\nCompleted", n, "of", reps)
    n <- n + 1
}

#' Rename data
vars <- c(
    "type" = "type",
    "city_id" = "city_id",
    "city" = "city",
    "country" = "country",
    "id" = "id",
    "name" = "tags.name",
    "lat" = "lat",
    "lon" = "lon",
    "addr_num" = "tags.addr:housenumber",
    "addr_street" = "tags.addr:street",
    "addr_postcode" = "tags.addr:postcode",
    "addr_city" = "tags.addr:city"
)
master <- master %>% select(., !!vars)

#' Save Data For Cleaning
saveRDS(master, "tmp/raw_merged_museums.RDS")
