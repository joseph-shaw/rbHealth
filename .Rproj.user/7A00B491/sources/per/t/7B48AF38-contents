#' Push appointments to Smartabase
#'
#'
#' Make sure your credentials have been added to your R environment on the
#'   device you're using, and neon and Tidyverse have been installed. Speak
#'   to Joe if this hasn't been done. At this point you should also have run
#'   the Excel spreadsheet to produce the required files.
#'
#' @param user string: uploader's Windows user. Should pull automatically.
#' @param sb_user_id numeric: uploader's Smartabase ID. Contact Smartabase administrator to obtain.
#' @param date string: formatted "dd-mm-YYYY". Will automatically use next Monday unless overwritten.
#' @returns Pushes appointments to Smartabase.
#' @examples
#'
#' push_appointments_to_smartabase(sb_user_id = 22803)
#'
#' @export

push_appointments_to_sb <- function(
  user = Sys.info()[["user"]],
  sb_user_id,
  date = Sys.Date() + which(str_split(lubridate::wday(Sys.Date() + 0:6, label = TRUE), " ") == "Mon")-1
){

# Load packages ----------------------------------------------------------
  library(neon)
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(getPass)


# Pull user IDs -----------------------------------------------------------
ids <-  neon::pull_smartabase(
  form = "Personal Details",
  type = "event",
  start_date = "01/01/2014",
  end_date = "20/03/2030",
  password = "prompt",
) %>%
  select(
    about, Surname, user_id
  ) %>%
  mutate(
    Surname = tolower(Surname)
  )

# Physiotherapy Appointments ----------------------------------------------

physio_apt <- read.csv(paste0("C:/Users/", user, "/Documents/physio_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    Physiotherapist != ""
  ) %>%
  rename(
    Surname = user_id
  ) %>%
  mutate(
    Surname = tolower(Surname),
    Physio = Physiotherapist,
    form = "Physiotherapy Appointment"
  ) %>%
  left_join(ids, by = "Surname") %>%
  select(-c(Surname, about)) %>%
  distinct() %>%
  push_smartabase(
    form = "Physiotherapy Appointment",
    entered_by_user_id = sb_user_id
  )

# S&C Appointments ----------------------------------------------

sc_apt <- read.csv(paste0("C:/Users/", user, "/Documents/sc_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    S.C != ""
  ) %>%
  rename(
    'S&C' = S.C,
    Surname = user_id
  ) %>%
  mutate(
    Surname = tolower(Surname),
    form = "S&C Appointment"
  ) %>%
  left_join(ids, by = "Surname") %>%
  select(-c(Surname, about)) %>%
  distinct() %>%
  push_smartabase(
    form = "S&C Appointment",
    entered_by_user_id = sb_user_id
  )


# Pilates Appointments ----------------------------------------------

pilates_apt <- read.csv(paste0("C:/Users/", user, "/Documents/pilates_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    Pilates != ""
  ) %>%
  rename(
    Surname = user_id
  ) %>%
  mutate(
    Surname = tolower(Surname),
    form = "Pilates & Gyrotonic Appointment"
  ) %>%
  left_join(ids, by = "Surname") %>%
  select(-c(Surname, about)) %>%
  distinct() %>%
  rename("Pilates/Gyrotonic" = Pilates) %>%
  push_smartabase(
    form = "Pilates & Gyrotonic Appointment",
    entered_by_user_id = sb_user_id
  )

# Doctors Appointments ----------------------------------------------

doctor_apt <- read.csv(paste0("C:/Users/", user, "/Documents/doctor_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    Doctor != ""
  ) %>%
  rename(
    Surname = user_id
  ) %>%
  mutate(
    Surname = tolower(Surname),
    form = "Doctor Appointment"
  ) %>%
  left_join(ids, by = "Surname") %>%
  select(-c(Surname, about)) %>%
  distinct() %>%
  push_smartabase(
    form = "Doctor Appointment",
    entered_by_user_id = sb_user_id
  )

# Coaching Appointments ----------------------------------------------

coach_apt <- read.csv(paste0("C:/Users/", user, "/Documents/coach_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    Coach != ""
  ) %>%
  rename(
    Surname = user_id
  ) %>%
  mutate(
    Surname = tolower(Surname),
    form = "Coach Appointment"
  ) %>%
  left_join(ids, by = "Surname") %>%
  select(-c(Surname, about)) %>%
  distinct() %>%
  push_smartabase(
    form = "Coaching Appointment",
    entered_by_user_id = sb_user_id
  )

# Soft Tissue Appointments ----------------------------------------------

soft_tissue_apt <- read.csv(paste0("C:/Users/", user, "/Documents/soft-tissue_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    Soft.Tissue != ""
  ) %>%
  rename(
    Surname = user_id
  ) %>%
  mutate(
    Surname = tolower(Surname),
    form = "Soft Tissue Therapy Appointment"
  ) %>%
  left_join(ids, by = "Surname") %>%
  select(-c(Surname, about)) %>%
  rename("Soft Tissue Therapy" = Soft.Tissue) %>%
  distinct() %>%
  push_smartabase(
    form = "Soft Tissue Therapy Appointment",
    entered_by_user_id = sb_user_id
  )


# Rehab Planning ----------------------------------------------------------


rehab_planning <- read.csv(paste0("C:/Users/", user, "/Documents/rehab-planning_appts_", date, ".csv"), fileEncoding = "UTF-8-BOM") %>%
  filter(
    Practitioner == "Rehab Planning",
    Is.Dancer
  ) %>%
  rename(
    Surname = About
  ) %>%
  mutate(
    Surname = tolower(Surname),
    form = "Rehab Planning Appointment"
  ) %>%
  select(-user_id) %>%
  left_join(ids, by = "Surname") %>%
  select(
    Practitioner, user_id, Date, Start.Time, Finish.Time, form
  ) %>%
  distinct() %>%
  rename(
    "Rehab Planning" = 1,
    start_date = Date,
    start_time = Start.Time,
    end_time = Finish.Time
  ) %>%
  mutate(
    end_date = start_date
  ) %>%
  push_smartabase(
    form = "Rehab Planning Appointment",
    entered_by_user_id = sb_user_id
  )

return(
# Manually upload staff unavailable form via:
# 'data entry' -> 'Import data' -> 'Staff Unavailable'
)
}
