#' Process local file containing blood results and push to smartabase
#'
#' @param results.path string: path to the folder containing the results file to upload. No trailing slash.
#' @param id.path string: path to the folder containing the id file to join. No trailing slash.
#' @param female.path string: path to the folder containing the female info file to join. No trailing slash.
#' @param reference.path string: path to the folder containing the reference ranges file to join. No trailing slash.
#' @param sb_user_id numeric: uploader's Smartabase ID. Contact Smartabase administrator to obtain.
#' @param date string: date on which bloods were taken. "yyyy-mm-dd"
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

read_bloods <- function(id.path, results.path, female.path, reference.path, sb_user_id, date){

  library(tidyverse)
  library(neon)

  # download personal details
  personal.details <- neon::pull_smartabase(
    form = "Personal Details",
    start_date = "01/01/2010",
    end_date = "01/01/2040"
  ) %>%
    janitor::clean_names() %>%
    select(first_name, surname, date_of_birth, user_id) %>%
    rename(last_name = surname) %>%
    mutate(
      last_name_lower = tolower(last_name)
    ) %>%
    select(-c(first_name, last_name))

  # read ids
  ids <- readxl::read_xlsx(id.path) %>%
    janitor::clean_names() %>%
    select(id:age) %>%
    mutate(
      last_name_lower = tolower(sb_surname)
    ) %>%
    left_join(personal.details, by = "last_name_lower") %>%
    distinct()

  # read and process data
  data <- readxl::read_xlsx(results.path) %>%
    janitor::clean_names()

  print(names(data))
  first_test <- readline(prompt="Enter name of first variable: ")
  last_test <- readline(prompt="Enter name of last variable: ")

  first_test <- which(names(data) == first_test)
  last_test <- which(names(data) == last_test)


  data <- data  %>%
    select(reg_no, date_time_received, first_test:last_test) %>%
    mutate(
      across(!date_time_received, as.character)
    ) %>%
    filter(!is.na(reg_no)) %>%
    pivot_longer(
      !reg_no:date_time_received,
      names_to = "test",
      values_to = "result"
    )

  ref_ranges <- read_csv(reference.path) %>%
    janitor::clean_names() %>%
    fill(test:units, .direction = "down") %>%
    mutate(
      test = tolower(test),
      test = gsub(" ", "_", test)
    )

  female <- read.csv(female.path) %>%
    mutate(fh.name = tolower(About)) %>%
    select(fh.name, Are.you.currently.using.any.form.of.contraception, On.which.day.did.your.current.menstrual.cycle.start., Day.of.Cycle)


  test <- data %>%
    mutate(
      test = gsub("testosterone_ia", "testosterone", test)
    ) %>%
    left_join(ref_ranges, by = "test") %>%
    left_join(ids, by = c("reg_no" = "id")) %>%
    mutate(
      sex = ifelse(sex == "M", "male", "female"),
      comments = ifelse(is.na(comments), sex, comments),
      fh.name = tolower(paste(first_name, sb_surname)),
  ) %>%
  filter(
    !(comments == "male" & sex == "female"),
    !(comments == "female" & sex == "male"),
  ) %>%
    left_join(female, by = "fh.name") %>%
    janitor::clean_names() %>%
    rename(
      first_day_current_cycle = on_which_day_did_your_current_menstrual_cycle_start,
      contraception = are_you_currently_using_any_form_of_contraception
    ) %>%
    mutate(
      first_day_current_cycle = as.Date(first_day_current_cycle, format = "%d-%m-%Y"),
      test_date = as.Date(date_time_received, format = "%Y-%m-%d"),
      day_of_cycle = test_date - first_day_current_cycle
    ) %>%
    pivot_wider(
      #c(lower_ref_limit, upper_ref_limit, phase),
      names_from = phase,
      values_from = c(lower_ref_limit, upper_ref_limit)
    ) %>%
    rename(
      low = lower_ref_limit_NA,
      high = upper_ref_limit_NA,
      low_follicular = lower_ref_limit_follicular,
      high_follicular = upper_ref_limit_follicular,
      low_luteal = lower_ref_limit_luteal,
      high_luteal = upper_ref_limit_luteal,
      low_mid.cycle = `lower_ref_limit_mid-cycle`,
      high_mid.cycle = `upper_ref_limit_mid-cycle`,
    ) %>%
    mutate(
      result_numeric = tidyr::extract_numeric(result),
      flag = case_when(
        test == "adjusted_calcium" ~ NA,
        is.na(low)~"phase-specific",
        result_numeric <= low ~ "Low",
        result_numeric >= high ~ "High",
      ),
      date_of_birth = as.Date(date_of_birth, format = "%d-%m-%Y"),
      age = round(as.numeric((test_date - date_of_birth) / 365))
    ) %>%
    select(
      first_name, last_name, user_id, sex, date_of_birth, age, contraception, test_date, first_day_current_cycle, day_of_cycle, test, result, units,
      flag, low , low_follicular , low_luteal , low_mid.cycle , high , high_follicular , high_luteal , high_mid.cycle

    ) %>%
    rename(
      `First Name` = first_name,
      `Last Name`  = last_name,
      Date = test_date,
      Test = test,
      SEX = sex,
      Value = result,
      Units = units,
      Abnormal = flag,
    ) %>%
    rowwise() %>%
    mutate(
      `Reference Range` = paste(low, "-", high),
      Date = as.character(
        paste(substr(Date, 9, 10), substr(Date, 6, 7), substr(Date, 1, 4), sep = "/")
      ),
      value = gsub("<", "", Value),
      value = as.numeric(value),
      value = round(value, digits = 2),
      Value = ifelse(substr(Value, 1, 1) == "<", paste0("<", value), as.character(value))
    )

  # Push to SB
  neon::push_smartabase(
    df = test,
    form = "TDL Results"
  )


 # Export all results
  flags <- test %>%
    filter(!is.na(Abnormal)) %>%
    arrange(`Last Name`)

  flags_wo_phase <- flags %>%
    filter(
      Abnormal != "phase-specific"
    )


  all_data <- test %>%
    select(-c(user_id, date_of_birth, `Reference Range`)) %>%
    mutate(
      `First Name` = ifelse(`First Name` == lag(`First Name`, 1), "", `First Name`),
      `Last Name` = ifelse(`Last Name` == lag(`Last Name`, 1), "", `Last Name`),
      SEX = ifelse(`First Name` == "", "", SEX),
      age = ifelse(`First Name` == "", "", age),
      contraception = ifelse(`First Name` == "", "", contraception),
      day_of_cycle = ifelse(`First Name` == "", "", day_of_cycle),
    ) %>%
    select(-c(first_day_current_cycle)) %>%
    relocate(day_of_cycle, .before = Date) %>%
    mutate(
      across(low:high_mid.cycle, as.character),
      contraception = as.character(contraception),
      day_of_cycle = as.character(day_of_cycle),
      across(Abnormal:high_mid.cycle, as.character),
    ) %>%
    tidyr::replace_na(
      list(
        contraception = "",
        day_of_cycle = "",
        Abnormal = "-",
        low = "-",
        low_follicular = "-",
        low_luteal = "-",
        low_mid.cycle = "-",
        high = "-",
        high_follicular = "-",
        high_luteal = "-",
        high_mid.cycle = "-"
      )
    )

  all_data[1,1] <- test[1,1]
  all_data[1,2] <- test[1,2]
  all_data[1,3] <- test[1,4]
  all_data[1,4] <- as.character(test[1,6])
  all_data[1,5] <- test[1,7]



 # reds <- data %>%
 #   filter(
 #     test %in% c("fer", "free_t4", "lh", "testia", "fsh", "e2d")
 #   )

  write.csv(all_data, "C:/Users/joseph.shaw/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Research/Bloods/results/all_data.csv", row.names = F)
  write.csv(flags, "C:/Users/joseph.shaw/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Research/Bloods/results/flags.csv", row.names = F)
  write.csv(flags_wo_phase, "C:/Users/joseph.shaw/Royal Opera House/Ballet Healthcare (private) - Healthcare Shared/Research/Bloods/results/flags_wo_phase.csv", row.names = F)




}












