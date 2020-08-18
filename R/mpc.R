#' ---
#' title: mpc.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.17"
#' purpose:
#' ---
#'
#' @export
mpc_read_file <- function(file) {
  turn_off_warnings()
  df <- readr::read_csv(file, col_names = c("raw"), col_types = "c") %>%

    # Get the flush number, so we can group by it to get the rest of the info
    dplyr::mutate(flush_number = c_flushes(as.numeric(raw), length(raw))) %>%
    dplyr::group_by(flush_number) %>%

    # Convert to a time event file
    dplyr::summarize(flush_to_tec(raw)) %>%

    # Cleanup
    dplyr::ungroup() %>%
    dplyr::select(-flush_number)

  turn_on_warnings()
  return(df)
}

#' @export
mpc_read_files <- function(files) {
  purrr::map_df(files, mpc_read_file)
}

#' @export
flush_to_tec <-  function(raw) {
  protocol_name <- raw[8]
  subject_name <- raw[7]
  date <- lubridate::ymd(glue::glue("{raw[6]}-{raw[4]}-{raw[5]}"))

  start <- 1 + 18 + as.numeric(raw[18])
  end <- length(raw)

  timeevent <- as.numeric(raw[start:end])
  timestamp <- as.integer(timeevent)
  event <- round(1000 * (timeevent - timestamp))

  # TODO(David): Get these from the database...
  start_code <- 100
  end_code <- 110
  parameter_ids <- c_parameter_ids(event, length(event),
                                   start_code, end_code)

  is_parameter = parameter_ids > 0
  event[is_parameter] <- NA
  timestamp[is_parameter] <- NA
  timestamp %<>% zoo::na.fill("extend") # fill time forward

  timeevent[!is_parameter] <- NA
  parameter <- timeevent

  result <- dplyr::tibble(protocol_name = protocol_name,
                          subject_name = subject_name,
                          date = date,
                          timestamp = timestamp,
                          event = event,
                          parameter_id = parameter_ids,
                          parameter = parameter)
  return(result)
}
