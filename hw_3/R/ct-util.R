setwd("/Users/sebastianjin/Documents/hw_3/R")
library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
source("test.R")
source("reference.R")
# Create the connection to a database and "studies" and "sponsors" tables.

con = dbConnect(
  duckdb(
    file.path("ctgov.duckdb"),
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.

# Q1 : this is the new phase uniform histogram which always displays all the phases regardless whether their count is 0 or not
plot_phase_histogram = function(x) {
  x$phase[is.na(x$phase)] = "NA"
  phases <- c("Early phase 1", "NA", "Not Applicable", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Phase 4")
  
  x <- x %>%
    mutate(phase = factor(phase, levels = phases))
  
  x <- x %>%
    count(phase) %>%
    complete(phase = phases, fill = list(n = 0))
  
  # Transfer the value from the last column to the first column
  x <- x %>%
    mutate(n = ifelse(phase == "Early phase 1", n + last(n), n))
  
  p <- ggplot(x, aes(x = factor(phase, levels = phases), y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
  
  return(p)
}




#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  # Get all of the unique dates.
  all_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}


plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}

# Q2 Get condition data from the database
get_condition_data <- function(d) {
  # Query the conditions table in the ctgov.duckdb database
  query <- "
    SELECT name, COUNT(*) AS count
    FROM conditions
    GROUP BY name
    ORDER BY count DESC
  "
  conditions_data <- dbGetQuery(d, query)
  return(conditions_data)
}

# Create a histogram of conditions
plot_condition_histogram <- function(data) {
  library(ggplot2)
  
  # Create a ggplot object to plot the histogram
  p <- ggplot(data, aes(x = reorder(name, -count), y = count)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    xlab("Condition") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(p)
}
