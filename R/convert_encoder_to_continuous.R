#' Pull chronojump data and convert to continuous data
#'
#' @param start_folder string: path to the folder containing the file(s). No trailing slash.
#' @param end_folder string: path to the destination folder to relocate the file(s). No trailing slash.
#' @returns Writes transformed data to a new file.
#' @examples
#' # Use default file paths
#' convert_encoder_to_continuous (start_folder = NA, end_folder = NA)
#'
#'@export

convert_encoder_to_continuous <- function(user = Sys.info()[["user"]], start_folder = NA, end_folder = NA){

  # Load Packages -----------------------------------------------------------

  library(ggplot2)
  library(magrittr)
  library(dplyr)
  library(neon)
  library(janitor)
  library(lubridate)
  library(tidyr)
  library(getPass)

  # Load files --------------------------------------------------------------

  if(is.na(start_folder)){start_folder <- paste0("C:/Users/", user, "/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Data Science/ChronoJump In")}
  if(is.na(end_folder)){end_folder <- paste0("C:/Users/", user, "/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Data Science/ChronoJump Out")}

  files <- list.files(start_folder)
  file_locs <- list.files(start_folder, full.names = T)
  names <- gsub(".tcx", "", files)

  # Load data from file -----------------------------------------------

  read_and_write_data <- function(file_in){

  file_out <- gsub("In", "Out", file_in)

  data <- read.csv(file_in) %>%
    clean_names() %>%
    slice(-c(1:5)) %>%
    pivot_longer(
      !x,
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      variable = gsub("m_s_2", "ms2", variable),
      variable = gsub("m_s", "ms", variable),
      variable = gsub("mm_2", "mm2", variable),
      x = as.numeric(x)
    ) %>%
    separate(variable, into = c("variable", "rep", "unit"), sep = "_") %>%
    unite("variable", variable, unit, sep = "_") %>%
    mutate(
      rep = as.numeric(rep)
    ) %>%
    pivot_wider(
      names_from = "variable",
      values_from = "value"
    ) %>%
    filter(
      !(is.na(dist_mm) & is.na(dist_mm2) & is.na(speed_ms) & is.na(accel_ms2) & is.na(force_n) & is.na(power_w))
    ) %>%
    arrange(rep, x) %>%
    mutate(
      time = 1:n(),
    ) %>%
    relocate(time, 1) %>%
    rename(
      time_in_rep = x
    )

  write.csv(data, file_out, row.names = F)
}

  lapply(file_locs, read_and_write_data)

}

