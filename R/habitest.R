#' ---
#' title: habitest.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.17"
#' purpose:
#' ---

#' @export
habitest_read_file <- function(f) {
  col_types = "ccccccc"
  col_names <- c("index", "timestamp", "input_type", "event",
                 "name", "parameter", "unknown")
  df = readr::read_csv(f, col_names = col_names, col_types = col_types)

  # NOTE(David): we have to get the column of the metadata dynamically
  #              because graphicstate change the column in an upgrade.
  #              (we ugraded in the winter of 2016-2017)
  col = c(ifelse((!is.na(df[1,2])) & (df[1,2]=="Subject"), 4, 6))
  df %<>%
    dplyr::mutate(subject_name=as.character(.[1,col]),
                  protocol_name=as.character(.[2,col]),
                  date=as.character(.[6,col]))  %>%
    tail(-7) %>%
    dplyr::filter(input_type!="Exit",
                  name!="ActivityMonitorOn", name!="ActivityMonitorOff") %>%
    dplyr::mutate(timestamp=as.double(timestamp),
                  parameter=as.double(parameter),
                  parameter_id = !is.na(parameter)) %>%
    dplyr::select(protocol_name, subject_name, date, timestamp,
                  input_type, event, name, parameter_id, parameter)

  return(df)
}

#' @export
habitest_read_files <- function(files) {
  df <- purrr::map_df(files, habitest_read_file) %>%

    # get date in correct format
    dplyr::mutate(date = lubridate::mdy(date)) %>%

    # get unique parameter_ids and event codes
    dplyr::mutate(list_or_register = input_type %in% c("List", "Reg")) %>%
    tidyr::unite("tmp_event", input_type, name) %>%
    dplyr::mutate(parameter_id = ifelse(list_or_register, tmp_event, NA),
                  event = ifelse(!list_or_register, tmp_event, NA)) %>%
    dplyr::select(-list_or_register) %>%

    # all *_read_file must return the this...
    dplyr::select(protocol_name, subject_name, date,
                  timestamp, event, parameter_id, parameter)
  return(df)
}
