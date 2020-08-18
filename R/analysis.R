#' ---
#' title: analysis.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.17"
#' purpose:
#' ---

#' @export
changes_in <- function(parameter, value, par) {
  result <- rep(FALSE, length(parameter))

  idx <- which(parameter == par)
  value <- value[idx]
  changes <- value != lag(value, default = -1)
  result[idx] <- changes

  return(result)
}





#' @export
raster <- function(event, pattern)
{
  pattern = codesfor(event, pattern)
  events = as.numeric(event)
  return(rasterc(events, pattern, length(events)))
}

#' @export
trialdef  <- function(events, pattern, from_first = FALSE)
{
  fct_events <- factor(events)
  fct_pattern <- factor(pattern, levels = levels(fct_events))

  trial <- c_trialdef(as.numeric(fct_events),
                      as.numeric(fct_pattern),
                      length(events),
                      length(pattern),
                      from_first = from_first)
}
