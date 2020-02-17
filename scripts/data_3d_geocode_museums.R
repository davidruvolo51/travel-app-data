#'////////////////////////////////////////////////////////////////////////////
#' FILE: data_3d_geocode_museums.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-02-16
#' MODIFIED: 2020-02-16
#' PURPOSE: geocode merged museums dataset
#' STATUS: working; complete
#' PACKAGES: tidyverse; httr;
#' COMMENTS: Geocoding data to update place information
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

#' pkgs
suppressPackageStartupMessages(library(tidyverse))

#' load data
museums <- readRDS("tmp/museums_merged_pre_geocode.RDS")

#' source tools
source("scripts/utils/utils_99_general.R")


#'////////////////////////////////////////////////////////////////////////////

#' ~ 0 ~
#' Isolate Cases
#' Isolate Cases that have id, name, and coordinates. These are complete and
#' do not need to be included in this script. In the following section, I will
#' create a nominatim request with the maximum 50 ids. To make things easier,
#' I will create a new dataset containing the cases that need to be queried
#' as it would be difficult to write a function that evaluates in a batch of
#' 50 cases, which ones need to be run and which ones do. Rather than causing
#' potential errors, it's easier to separate these cases and combine them
#' later on. I'm going to say that address is not that important here as I
#' won't be providing directions only the location of the place on the map.
#' It would be easier to refer the user to an external source for address
#' and directions as this is beyond the scope of the project. Given the
#' problems with using OSM IDs, it is likely that the API will return
#' many results.
museums_complete <- museums %>% filter(hasName == TRUE, hasCoords == TRUE)

#' Save these in case something crashes
saveRDS(museums_complete, "tmp/museums_complete.RDS")


#' Reduce the main dataset to the opposite cases
museums_incomplete <- museums %>% filter(!id %in% unique(museums_complete$id))
saveRDS(museums_incomplete, "tmp/museums_incomplete.RDS")


#'////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' Run Loop

#' Set Columns of Interest
vars <- c(
    "display_name",
    "lat",
    "lon",
    "address.museum",
    "address.house_number",
    "address.road",
    "address.neighbourhood",
    "address.city",
    "address.state",
    "address.postcode",
    "address.country",
    "address.country_code",
    "namedetails.name"
)

#' Define Data.frame containing all stop and end points by 50
seqdf <- data.frame(
    start = seq.int(from = 1, to = NROW(museums_incomplete), by = 49),
    end = c(seq.int(from = 50, to = NROW(museums_incomplete), by = 49), NROW(museums_incomplete)),
    stringsAsFactors = FALSE
); seqdf %>% head()

#' Adjust Loop As necessary using the seqdf and n counts, as well as tmp files
n <- 1402
reps <- NROW(seqdf)
save_array <- seq.int(from = 1, to = NROW(seqdf), by = 200)
save_array <- save_array[save_array != 1]

#' Run
while (n <= reps) {
    
    #' Start
    start <- seqdf$start[n]
    end <- seqdf$end[n]
    cat("\nStarting Batch:", n, "of", reps)
    cat("\n\tRunning with Cities", start, "to", end)
    
    #' Building Query
    cat("\n\tRunning Query...")
    q <- tools$nominatim$new_request(
        id = museums_incomplete$id[start:end], 
        type = museums_incomplete$type[start:end]
    )
    response <- httr::GET(q)
    
    #' Process Response
    if (response$status_code == 200) {
        cat("\n\tResult OK")
        cat("\n\tEvaluating Results...")
        
        #' Extract Response Body
        body <- httr::content(response, "text", encoding = "utf-8")
        
        #' Check to See if Response is Blank
        if (body != "[]") {
            cat("\n\tBody OK")
            cat("\n\tExtracting Data...")
            
            #' Extract Data
            result <- jsonlite::fromJSON(body) %>% jsonlite::flatten(.)
            extracted <- result %>% select_if(names(.) %in% vars)
            extracted$ids <- result$osm_id
            status <- "completed"
            
        } else {
            cat("\n\tBody FAILED")
            extracted <- data.frame(
                ids = museums_incomplete$id[start:end],
                stringsAsFactors = FALSE
            )
            status <- "blank_response"
        }
    } else {
        cat("\n\tResponse FAILED")
        extracted <- data.frame(
            ids = museums_incomplete$id[start:end],
            stringsAsFactors = FALSE
        )
        status <- "failed_response"
    }
    
    #' Fix Columns
    # if (NCOL(extracted) != length(vars)) {
    cat("\n\tEvaluating Columns...")
    missing <- vars[!vars %in% names(extracted)]
    if (length(missing) > 0) {
        cat("\n\t\tAdding Missing Columns...")
        sapply(seq_len(length(missing)), function(d) {
            extracted[[missing[d]]] <<- NA_character_
        })
    }
    
    #' Add  Status to Local Object
    extracted$status <- status
    
    #' Bind to Parent Object
    if (n == 1) {
        master <- extracted
    } 
    if (n > 1){
        master <- rbind(master, extracted)
    }
    
    #' Run Save Check
    if (n %in% save_array) {
        saveRDS(master, paste0("tmp/museums_geocoded_first_",n,".RDS"))
    }
    
    #' Done
    cat("\n\tDone!")
    n <- n + 1
    
    #' Set pause
    time <- runif(1, 6, 16)
    cat("\n\nPausing for", round(time,1), "seconds @", as.character(Sys.time()),"\n")
    Sys.sleep(time)
}

#' Save Data
saveRDS(master, "tmp/museums_incomplete_geocoded.RDS")



#' 
#' 
#' 
#' 
#' 
#' #' Set Loop Params
#' n <- 1
#' reps <- 100 #NROW(museums)
#' savearray <- seq.int(from = 1, to = reps, by = 50)
#' while (n <= reps) {
#'     
#'     #' Message
#'     cat("\nStarting", n, "of", reps)
#'     
#'     #' determine if query should be run
#'     tmp <- museums[n, ]
#'     if (isFALSE(tmp$hasName) | isFALSE(tmp$hasCoords)) {
#'         
#'         #' Build Query
#'         cat("\n\tBuilding Query...")
#'         q <- tools$nominatim$new_request(
#'             id = tmp$id,
#'             type = tmp$type
#'         )
#'         
#'         #' Send Request
#'         cat("\n\tSending Request...")
#'         response <- httr::GET(q)
#'         
#'         #'/////////////////////////////////////////////////
#'         #' Process Response
#'         if (response$status_code == 200) {
#'             
#'             #' Extract Request Body
#'             body <- httr::content(response, type = "text", encoding = "utf-8")
#'             
#'             #' Make Sure Result isn't empty
#'             if (body != "[]"){
#'                 result <- jsonlite::fromJSON(body) %>% jsonlite::flatten()
#'                 
#'                 #' Extract Results Based on Existence of Columns
#'                 extracted <- result %>% select_if(names(.) %in% vars)
#'                 missing <- vars[!vars %in% names(extracted)]
#'                 if (length(missing) > 0) {
#'                     cat("\n\tFixing Missing Columns...")
#'                     sapply(seq_len(length(missing)), function(d) {
#'                         extracted[[missing[d]]] <<- NA_character_
#'                     })
#'                 }
#'                 
#'                 #'/////////////////////////////////////////////////
#'                 #' Append Data to Master Object
#'                 cat("\n\tAppending Data...")
#'                 
#'                 #' Is Lat Missing and Available?
#'                 if (is.na(tmp$lat) & !is.na(extracted$lat)) {
#'                     cat("\n\t...lat")
#'                     museums$lat[n] <- extracted$lat
#'                 }
#'                 
#'                 #' Is Lon Missing and Available?
#'                 if (is.na(tmp$lon) & !is.na(extracted$lon)) {
#'                     cat("\n\t...lon")
#'                     museums$lon[n] <- extracted$lon
#'                 }
#'                 
#'                 #' Is Address Number Missing & Available?
#'                 if (is.na(tmp$addr_num) & !is.na(extracted$address.house_number)) {
#'                     cat("\n\t...house number")
#'                     museums$addr_num[n] <- extracted$address.house_number
#'                 }
#'                 
#'                 #' Is Street Missing & Available?
#'                 if (is.na(tmp$addr_street) & !is.na(extracted$address.road)) {
#'                     cat("\n\t...street name")
#'                     museums$addr_street[n] <- extracted$address.road
#'                 }
#'                 
#'                 #' Is Postcode Missing & Available?
#'                 if (is.na(tmp$addr_postcode) & !is.na(extracted$address.postcode)) {
#'                     cat("\n\t...postcode")
#'                     museums$addr_postcode[n] <- extracted$address.postcode
#'                 }
#'                 
#'                 #' Is Neighborhood Missing & Available?
#'                 if (is.na(tmp$addr_neighborhood) & !is.na(extracted$address.neighbourhood)) {
#'                     cat("\n\t...neighborhood")
#'                     museums$addr_neighboorhood[n] <- extracted$address.neighbourhood
#'                 }
#'                 
#'                 #' Is City Missing & Available?
#'                 if (is.na(tmp$addr_city) & !is.na(extracted$address.city)) {
#'                     cat("\n\t...city")
#'                     museums$addr_city[n] <- extracted$address.city
#'                 }
#'                 
#'                 #' Is Name Missing And Available in one of the variables?
#'                 if (is.na(tmp$name)) {
#'                     
#'                     #' The Name Details Name is the most important column
#'                     if (!is.na(extracted$namedetails.name)) {
#'                         cat("\n\t...name (method 1)")
#'                         museums$name[n] <- extracted$namedetails.name
#'                     } 
#'                     
#'                     #' Extract Name from the Display_name Column
#'                     if (is.na(extracted$namedetails.name) & !is.na(extracted$display_name)) {
#'                         cat("\n\t...name (method 2)")
#'                         display_name <- substring(
#'                             text = extracted$display_name,
#'                             first = 1,
#'                             last = as.numeric(
#'                                 gregexpr(
#'                                     pattern = ",",
#'                                     text = extracted$display_name
#'                                 )[[1]][1]
#'                             ) - 1
#'                         )
#'                         museums$name[n] <- display_name
#'                         rm(display_name)
#'                     }
#'                 }
#'                 
#'                 #' Print Complete Message and update status
#'                 cat("\n\tUpdated All Variables")
#'                 rm(list = c("body", "result", "extracted", "missing"))
#'                 musuems$status[n] <- "complete"
#'                 
#'             } else {
#'                 cat("\n\tNo result returned... :-(")
#'                 museums$status[n] <- "completed"
#'             }
#'             
#'         } else {
#'             #' Print Message and Update status
#'             cat("\n\tRequest Failed...")
#'             museums$status[n] <- "failed"
#'         }
#'         
#'         #' Add System Sleep after all requests iterations only
#'         cat("\n\tFinished processing...")
#'         rm(list = c("q", "response", "tmp"))
#'         Sys.sleep(runif(1, 6, 16))
#'         
#'     } else {
#'         cat("\n\tNo need to run query. Data is okay.")
#'         museums$status[n] <- "complete"
#'         rm(tmp)
#'         Sys.sleep(0.4)
#'     }
#'     
#'     #' Save Data Incase of Loop Failure
#'     if (n %in% savearray) {
#'         saveRDS(
#'             object = museums, 
#'             paste0("tmp/raw_geocoded_museums_first_", n,".RDS")
#'         )
#'     }
#'     
#'     # update
#'     cat("\nCity Complete!")
#'     n <- n + 1
#' }
