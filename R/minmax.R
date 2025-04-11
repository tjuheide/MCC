#' Longest shared follow-up
#'
#' @description
#' Function to identify the longest follow-up in the group with shortest follow-up
#'
#' @param data data.frame from the MCC-function with grp-parameter specified (i.e. with variable grp)
#'
#' @returns numeric value of the longest time observed in all groups
#' @export
#'
#' @examples
#' mccOutput <- mcc(data = mcc_events,
#'                  idvar = "id",
#'                  timevar = "time",
#'                  eventvar = "event",
#'                  grpvar = "grp",
#'                  weightvar = "weight")
#' minmax(mccOutput)
minmax <- function(data) {
  mm <- unique(data[, .(maxtime = max(time)),
                    by = grp][,.(minmax = min(maxtime))])

  return(mm)
}
