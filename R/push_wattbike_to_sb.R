#' Pull wattbike time trial data from the shared drive and push to Smartabase
#'
#' @param start_folder string: path to the folder containing the files to upload. No trailing slash.
#' @param end_folder string: path to the destination folder to relocate the files following the upload. No trailing slash.
#' @param test_duration_secs numeric: duration of the time trial in seconds.
#' @param sb_user_id numeric: uploader's Smartabase ID. Contact Smartabase administrator to obtain.
#' @returns Pushes data to Smartabase.
#' @examples
#' # Use default file paths
#' push_wattbike_to_sb(test_duration_secs = 180, sb_user_id = 22803)
#'
#'# Specify file path
#'push_wattbike_to_sb(
#'user = Sys.info()[["user"]],
#'start_folder = NA,
#'end_folder = NA",
#'test_duration_secs = 180,
#'sb_user_id = 22803
#')
#'
#'@export

push_wattbike_to_sb <- function(user = Sys.info()[["user"]], start_folder = NA, end_folder = NA, test_duration_secs = 180, sb_user_id
){

  # Load Packages -----------------------------------------------------------

  library(dplyr)
  library(magrittr)
  library(neon)
  library(XML)
  library(janitor)
  library(lubridate)
  library(tidyr)
  library(getPass)

  # Load files --------------------------------------------------------------

  if(is.na(start_folder)){start_folder <- paste0("C:/Users/", user, "/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Profiling/wattbike_to_smartabase_pending")}
  if(is.na(end_folder)){end_folder <- paste0("C:/Users/", user, "/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Profiling/2022 RB Profiling/Raw Bike Data")}

  files <- list.files(start_folder)
  file_locs <- list.files(start_folder, full.names = T)
  names <- gsub(".tcx", "", files)

  # Load data from smartabase -----------------------------------------------

  personal_details <- neon::pull_smartabase(
    form = "Personal Details",
    start_date = "01/01/2000",
    end_date = "01/01/3000"
  )

  weights <- neon::pull_smartabase(
    form = "ForceDecks Trials",
    start_date = "01/01/2022",
    end_date = "01/01/3000"
  ) %>%
    mutate(
      date = as.Date(start_date, format = "%d/%m/%Y")
    ) %>%
    group_by(user_id) %>%
    rename(weight = `Weight (kg)`) %>%
    filter(
      weight > 35,
      date == max(date),
      !is.na(weight)
    ) %>%
    slice(1) %>%
    select(
      user_id, about, weight
    ) %>%
    mutate(
      about = tolower(about)
    )

  # Loop through each file --------------------------------------------------

  for(i in 1:length(files)){

    doc <- XML::xmlParse(file_locs[i])
    nodes <- XML::getNodeSet(doc, "//ns:Trackpoint", "ns")
    rows <-  lapply(nodes, function(x) data.frame(XML::xmlToList(x)))

    library(plyr)

    #data <- do.call("rbind", rows) %>%
    data <- do.call("rbind.fill", rows)

    detach("package:plyr", unload = TRUE)

    data <- data %>%

      janitor::clean_names() %>%
      rename(speed = extensions_tpx_speed, watts = extensions_tpx_watts) %>%
      mutate_at(2:5, as.numeric) %>%
      mutate(
        time = lubridate::as_datetime(time),
        time_secs = as.numeric(time - min(time)),
        dancer = names[i],
        profiling = "Pre-season 2022-23",
        date = as.Date(time),
        date = as.character(date),
        date = paste(sep = "/", formatC(lubridate::day(date), width = 2, flag = 0, format = "d"), formatC(lubridate::month(date), width = 2, flag = 0, format = "d"), lubridate::year(date)),
      )

    smoothed <- loess(watts ~ time_secs, data=data, span=0.1)
    smoothed <- predict(smoothed)
    data$smoothed_watts <- smoothed

    hr <- data$value
    data$dancer <- unlist(strsplit(data$dancer, split = "_"))[3]

    if(ncol(data) == 10){data$value <- NA}

    data <- data %>%
      rename(heart_rate = value) %>%
      mutate(
        mean_watts = mean(watts)
      )

    if(i == 1){wattbike_data <- data}else{
      wattbike_data <- rbind(wattbike_data, data)
    }

    #new_location <- paste0(end_folder, "/", files[i])
    #file.rename(from = file_locs[i], to = new_location)

  }

  # Prep for Smartabase -----------------------------------------------------

  type <- paste("TT", test_duration_secs, "s")

  wattbike_datanew <- wattbike_data %>%
    mutate(
      about = tolower(dancer),
      firstname = about,
      about = gsub("_", " ", about)
    ) %>%
    separate(firstname, into = c("firstname", "surname")) %>%
    left_join(weights, by = "about") %>%
    rename(
      #weight = 'Weight (kg)',
      date_time = time,
    ) %>%
    mutate(
      watts_kg = watts / weight,
      test_time = test_duration_secs,
      test_type = type,
      test_pct = time_secs / test_time,
    ) %>%
    filter(!is.na(user_id))%>%
    ungroup() %>%
    neon::push_smartabase(
      form = "Time Trial Raw",
      entered_by_user_id = sb_user_id,
      #start_date = date
    )

  wattbike_summary <- wattbike_data %>%
    mutate(
      about = tolower(dancer),
      firstname = about,
      about = gsub("_", " ", about)
    ) %>%
    separate(firstname, into = c("firstname", "surname")) %>%
    left_join(weights, by = "about") %>%
    rename(
      #weight = 'Weight (kg)',
      date_time = time,
    ) %>%
    mutate(
      watts_kg = watts / weight,
      test_time = test_duration_secs,
      test_type = type,
      test_pct = time_secs / test_time,
    ) %>%
    filter(!is.na(user_id)) %>%
    group_by(dancer, date, user_id) %>%
    summarise(
      Exercise = "Bike",
      'Weight (kg)' = weight[1],
      'Distance (m)' = max(distance_meters, na.rm = T),
      'Time (s)' = max(time_secs, na.rm = T),
      'Max HR'  = max(heart_rate, na.rm = T),
      'Average HR'  = mean(as.numeric(heart_rate), na.rm = T),

    ) %>%
    ungroup() %>%
    neon::push_smartabase(
      form = "MAS",
      entered_by_user_id = sb_user_id,
      #start_date = date
    )


}

