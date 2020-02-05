#' /////////////////////////////////////////////////////////////////////////////
#' FILE: utils_0_scrape.R
#' AUTHOR: David Ruvolo
#' CREATED: 2019-11-21
#' MODIFIED: 2020-02-03
#' PURPOSE: functions to aid the webscrapping process + querying
#' PACKAGES: NA
#' STATUS: working
#' COMMENTS: NA
#' /////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:
options(stringsAsFactors = FALSE)

# nest the functions
utils <- list()


# ~ 0 ~
# GET COUNTRY INFO
# this function is designed to be run on the country guide page. this pulls all
# of the countries and the links to the country-level guide
utils$getCountryInfo <- function() {
    # pull name
    country <- tmpCountryRaw %>%
        html_nodes("div > a") %>%
        html_text(trim = TRUE)

    # pull link
    link <- tmpCountryRaw %>%
        html_nodes("div > a") %>%
        html_attr("href")

    # build data
    data.frame(
        country = ifelse(!length(country), NA, country),
        link = ifelse(!length(link), NA, link),
        stringsAsFactors = FALSE
    )
}

# ~ 1 ~
# GET CAFE INFO
# this function is designed to be run on the country guide pages
# define a function that pulls cafe level data from country guide
# data must be a single node, country is a string
utils$getCafeInfo <- function(data, country) {

    # get cafe city
    city <- data %>%
        html_nodes(".cg-list-single-meta") %>%
        html_text(trim = TRUE)

    # get cafe name
    name <- data %>%
        html_nodes(".cg-list-single-title") %>%
        html_text(trim = TRUE)

    # get cafe link
    link <- data %>% html_attr("href")

    # return as data.frame
    data.frame(
        country = country,
        city = ifelse(!length(city), NA, city),
        name = ifelse(!length(name), NA, name),
        link = ifelse(!length(link), NA, link),
        stringsAsFactors = FALSE
    )
}

# ~ 2 ~
# GET CAFE LOCATION
# this function is designed to be run on the cafe page. It will extract the
# address, facebook, and twitter link. Data is returned as a data frame.
utils$getAllCafeLocation <- function(data) {

    # cafe address
    address <- data %>%
        html_nodes(".cafe-address-content") %>%
        html_text(trim = TRUE) %>%
        gsub("\r\n", "", .) %>%
        gsub("\\s+", " ", .)

    # get facebook link
    facebook <- data %>%
        html_nodes(".cafe-online a[href*='facebook']") %>%
        html_attr("href")

    # get twitter link
    twitter <- data %>%
        html_nodes(".cafe-online a[href*='twitter']") %>%
        html_attr("href")

    # coordinates
    lat <- utils$getCafeLatitude(data)
    lng <- utils$getCafeLongitude(data)

    # return data.frame
    data.frame(
        address = ifelse(!length(address), NA, address),
        lat = lat,
        lng = lng,
        facebook = ifelse(!length(facebook), NA, facebook),
        twitter = ifelse(!length(twitter), NA, twitter),
        status = "success",
        stringsAsFactors = FALSE
    )
}


# ~ 3 ~
# GET CAFE COORDINATES - Longitude
# this function is to run on the cafe page. It will extract the coordinates from
# an inline script element by parsing the text for coordinates. Data must be a
# html node (i.e., page created by response)
utils$getCafeLongitude <- function(data) {

    # find script
    script <- data %>%
        html_nodes(".single-cafe-info script") %>%
        html_text()

    # make sure script exists before cleaning
    if (length(script)) {
        js <- script[[1]]
        lng <- substr(js, regexpr("longitude", js) + 0, regexpr("longitude", js) + 30) %>%
            gsub("longitude \\= ", "", .) %>%
            gsub("[;\r\n\\+ ]", "", .) %>%
            as.numeric()
    } else {
        lng <- NA
    }

    # return
    lng
}

# ~ 4 ~
# GET CAFE COORDINATES - LATITUDE
utils$getCafeLatitude <- function(data) {

    # find script
    script <- data %>%
        html_nodes(".single-cafe-info script") %>%
        html_text()

    # make sure script exists before cleaning
    if (length(script)) {
        js <- script[[1]]
        lat <- substr(js, regexpr("latitude", js) + 0, regexpr("latitude", js) + 30) %>%
            gsub("latitude \\= ", "", .) %>%
            gsub("[;\r\n\\+ ]", "", .) %>%
            as.numeric()
    } else {
        lat <- NA
    }

    # return
    lat
}


# ~ 5 ~
# SEND BLANK CAFE INFO DATAFRAME
# in the instance where a request fails, return a blank data.frame
utils$blankAllCafeLocation <- function() {
    data.frame(
        address = NA,
        lat = NA,
        lng = NA,
        facebook = NA,
        twitter = NA,
        status = "failed",
        stringsAsFactors = FALSE
    )
}

#' ////////////////////////////////////////////////////////////////////////////

# ~ B ~
# CREATE UTILS FOR GOOGLEWAY API

# create object
google <- list()


# ~ 1 ~
# BUILD QUERY
# define a function that builds a query to pass into the google places API
# by default it will use three variables, but there's the option to add more

google$buildQuery <- function(data, vars = c("name", "city", "country")) {

    # process vars
    out <- array(dim = length(vars))
    sapply(1:length(vars), function(x) {
        out[x] <<- data[, names(data) == vars[x]]
    })

    # collapse
    paste(out, collapse = " ")
}



# ~ 2 ~
# FORMAT TIME STRING
# define a function that parses time string object. Day of the week and opening
# hours need to be converted into a wide data frame. The function must account
# for potential missing days and times. This will require two functions: one that
# parses the string for either day of the week or opening hours and another one that
# cleans the entire object


# parse individual strings
google$formatTimeString <- function(string, type = "time") {

    # extract time
    if (type == "time") {
        out <- trimws(
            substring(
                text = string,
                first = regexpr(pattern = ": ", text = string)[[1]] + 1
            ),
            "both"
        )
    }

    # extract day
    if (type == "day") {
        out <- trimws(
            substring(
                text = string,
                first = 1,
                last = regexpr(pattern = ":", text = string)[[1]] - 1
            ),
            "both"
        )
    }

    # return
    out
}


# parse objects
google$formatPlaceHours <- function(data) {

    # build object
    out <- data.frame(
        monday = NA,
        tuesday = NA,
        wednesday = NA,
        thursday = NA,
        friday = NA,
        saturday = NA,
        sunday = NA,
        stringsAsFactors = FALSE
    )
    if (!is.null(data)) {

        # evaluate each element + account for potential missing days/times
        sapply(1:length(data), function(d) {
            if (!is.na(data[d]) || !data[d] == "" || !is.null(data[d])) {

                # extract day of the week and hours for each index
                day <- tolower(google$formatTimeString(data[d], type = "day"))
                time <- google$formatTimeString(data[d])

                # match day to reference df and assign extracted time
                out[names(out) == day] <<- time
            }
        })

        # return out
        out
    } else {
        out
    }
}
