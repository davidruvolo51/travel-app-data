#' ////////////////////////////////////////////////////////////////////////////
#' FILE: data_02_summarize.R
#' AUTHOR: David Ruvolo
#' CREATED: 2019-11-22
#' MODIFIED: 2020-02-03
#' PURPOSE: create summary objects for app
#' PACKAGES: see below
#' STATUS: in.progress
#' COMMENTS: NA
#' ////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:
options(stringsAsFactors = FALSE)


# pkgs
suppressPackageStartupMessages(library(tidyverse))


# data
df <- readRDS("data/eu_coffee.RDS")

#' ////////////////////////////////////////

# ~ 0 ~
# VERIFY DATA

meta <- list()
meta$summary <- list()

# the number of failed and successful responses
meta$summary$status <- df %>%
  group_by(status) %>%
  count() %>%
  as.data.frame()


# number of missing values by column
meta$summary$na_count <- df %>%
  sapply(., function(x) {
    NROW(which(is.na(x)))
  }) %>%
  as.data.frame(optional = TRUE) %>%
  bind_cols(vars = row.names(.)) %>%
  select(vars, missing = V1)

#' ////////////////////////////////////////

# ~ 1 ~
# FILTER DATA
# based on the summaries, all failed cases will be removed
# and all cases with missing addresses and coordinates will
# be removed. We will keep them anyways, but not include them
# in the analysis

# filter data
coffee <- df %>%
  filter(status == "success", !is.na(lng))

# check
coffee %>%
  sapply(., function(x) {
    NROW(which(is.na(x)))
  })

# there are still missing values for social media links, but that
# isn't essential to this project