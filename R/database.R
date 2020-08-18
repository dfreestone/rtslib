#' ---
#' title: database.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.17"
#' purpose:
#' ---

#' @export
db_add_events <- function(db_path, data){

  connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # TODO(David): Cleanup!
  # date_id
  #   insert date into dates if it doesn't exist.
  data %<>% dplyr::mutate(date = as.character(date))
  tbl_col <- as.character(unique(data$date))
  sql <- glue::glue("SELECT date FROM dates")
  db_tbl <- as.vector(as.matrix(DBI::dbGetQuery(connection, sql)))
  new <- dplyr::tibble(date = setdiff(tbl_col, db_tbl))
  if (nrow(new) > 0) {
    DBI::dbWriteTable(connection, "dates", new, append = TRUE)
  }

  # insert eventcodes if it doesn't exist
  #   TODO(David): what to do if eventcodes already
  #                 has some values, but not all?
  sql <- glue::glue("SELECT event FROM eventcodes")
  db_tbl <- as.vector(as.matrix(DBI::dbGetQuery(connection, sql)))
  if ((length(db_tbl) == 0) && !is.character(data$event)) {
    stop("No event codes specified, and data 'event' column is not a character vector")
  } else {
    name <- as.character(unique(data$event))
    name <- factor(name[!is.na(name)])
    event <- as.numeric(name)
    new <- dplyr::tibble(event, name)
    DBI::dbWriteTable(connection, "eventcodes", new, append = TRUE)

    data %<>% dplyr::mutate(event = as.numeric(factor(event, levels = levels(name))))
  }

  # insert parameter_ids if it doesn't exist
  sql <- glue::glue("SELECT parameter_name FROM parameters")
  db_tbl <- as.vector(as.matrix(DBI::dbGetQuery(connection, sql)))
  if ((length(db_tbl) == 0) && !is.character(data$parameter_id)) {
    stop("No parameter codes specified, and data 'parameter_id' column is not a character vector")
  } else {
    parameter_name <- as.character(unique(data$parameter_id))
    parameter_name <- factor(parameter_name[!is.na(parameter_name)])
    parameter_id <- as.numeric(parameter_name)
    new <- dplyr::tibble(parameter_id, parameter_name)
    DBI::dbWriteTable(connection, "parameters", new, append = TRUE)

    data %<>% dplyr::mutate(parameter_id = as.numeric(factor(parameter_id, levels = levels(parameter_name))))
  }


  # TODO(David): Cleanup!
  # Now recode date_id
  data %<>%
    dplyr::left_join(DBI::dbGetQuery(connection, "SELECT row_id, date FROM dates"),
              by = "date") %>%
    dplyr::rename(date_id = row_id) %>%
    dplyr::select(-date) %>%

    # Recode protocol_id
    dplyr::left_join(DBI::dbGetQuery(connection, "SELECT row_id, protocol_name FROM protocols"),
                     by = "protocol_name") %>%
    dplyr::rename(protocol_id = row_id) %>%
    dplyr::select(-protocol_name) %>%

    # Recode subject_id
    dplyr::left_join(DBI::dbGetQuery(connection, "SELECT row_id, subject_name FROM subjects"),
                     by = "subject_name") %>%
    dplyr::rename(subject_id = row_id) %>%
    dplyr::select(-subject_name) %>%

    # and reorder
    dplyr::select(protocol_id, date_id, subject_id,
                  parameter_id, timestamp, event, parameter)

  # Write the data file to the events table
  DBI::dbWriteTable(connection, "events", data, append = TRUE)
  DBI::dbDisconnect(connection)
}

#' @export
db_add_script <- function(db_path, script) {
  script <- tibble(file = script,
                   contents = readr::read_file(script))

  connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(connection, "scripts", script, append = TRUE)
  DBI::dbDisconnect(connection)
}

#' @export
db_create_empty_database <- function(db_path) {

  sql_script <- fs::path_package("rtslib", "exec", "create_tables.sql")
  script <- glue::glue("sqlite3 '{db_path}' < '{sql_script}'")

  # TODO(David): Is there a tidy way to use system?
  system(script)
}

#' @export
db_events <- function(db_path) {
  connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  df <- DBI::dbGetQuery(connection,
                        "SELECT protocols.protocol_name AS protocol,
                                subjects.subject_name AS subject,
                                dates.date AS date,
                                parameters.parameter_name AS parameter,
                                events.parameter AS value,
                                events.timestamp,
                                eventcodes.name AS event
                         FROM events
                         LEFT JOIN protocols  ON events.protocol_id  = protocols.row_id
                         LEFT JOIN subjects   ON events.subject_id   = subjects.row_id
                         LEFT JOIN dates      ON events.date_id      = dates.row_id
                         LEFT JOIN parameters ON events.parameter_id = parameters.parameter_id
                         LEFT JOIN eventcodes ON events.event        = eventcodes.event")

  DBI::dbDisconnect(connection)

  df <- dplyr::as_tibble(df) %>%
    mutate(protocol = as.factor(protocol),
           subject = as.factor(subject),
           date = lubridate::ymd(date))
  return(df)
}

#' @export
db_setup <- function(db_path, excel_file, protocol_file) {
  # Read in the excel files
  subjects <- readxl::read_excel(excel_file, sheet = "subjects")
  eventcodes <- readxl::read_excel(excel_file, sheet = "eventcodes")
  parameters <- readxl::read_excel(excel_file, sheet = "parameters")
  protocols <- readxl::read_excel(excel_file, sheet = "protocols")

  # Get the protocol code
  # TODO(David): Get code to work for binary files...
  #               use readr::read_file_raw(file) for it.
  #               can't be converted to text (for mpc files) with
  #               readr::read_file(raw)
  # TODO(David): Get code to work robustly, by matching the filename
  #                with the protocol_name. have to handle cases where
  #                the user wants them to be different (does this case exist?)
  # protocols$protocol_code <- readr::read_file_raw(protocol_file)

  # create and populate database
  db_create_empty_database(db_path)

  connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(connection, "subjects", subjects, append = TRUE)
  DBI::dbWriteTable(connection, "eventcodes", eventcodes, append = TRUE)
  DBI::dbWriteTable(connection, "parameters", parameters, append = TRUE)
  DBI::dbWriteTable(connection, "protocols", protocols, append = TRUE)
  DBI::dbDisconnect(connection)
}
