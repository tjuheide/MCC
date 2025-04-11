#' Expand MCC-estimates to all times
#'
#' @description
#' If MCCs are to be compared across groups it is nice to have an estimate in both groups at the relevant time point. Likewise, if bootstrapping is used to estimate uncertainty there should be the same number of bootstrap estimates at all times. This function ensures that there is an observation in all groups at all relevant times. Additional times can be requested by specifying the times parameter. Estimates are only provided in a group if there is at least one individual at risk at the given time. As the input should come from the mcc-function it is assumed to contain variables time and grp.
#'
#' @param mcc_est data.table from the MCC-function.
#' @param times single value or vector of specific times for which an estimate of the MCC is needed. Default NULL only times observed in the data will be outputted.
#'
#' @returns data.table with an MCC-estimate at each time-point observed in either level of grp, or specified by paramter times. E.g., if there is an observation at time T in grp A, then in the other levels of grp there will be created an observation at time T if it does not exist already. If a vector of times is provided, an observation will be created for these times as well in all levels of grp.
#' @export
#'
#' @examples
#' data("mcc_events")
#' mccOutput <- mcc(data = mcc_events,
#'                  idvar = "id",
#'                  timevar = "time",
#'                  eventvar = "event",
#'                  grpvar = "grp",
#'                  weightvar = "weight")
#' mccOutputExpand <- expandMCC(mccOutput)
#' mccOutputExpand <- expandMCC(mccOutput, c(1, 5, 10))
expandMCC <- function(mcc_est, times = NULL){

  if(!is.data.table(mcc_est)) stop("data is not a data.table")

  if(!is.null(times)){
    if(minmax(mcc_est) < max(times)){
      print("Warning:")
      print("A specified time point exceeds the follow-up in a group.")
      print("Estimates beyond the observed follow-up will not be made.")
    }
  }

  # all group-time-combinations
  time <- unique(c(mcc_est$time,times))
  grp <- unique(mcc_est$grp)
  gt <- CJ(grp, time)

  # last observed time within each group
  maxtimes <- mcc_est[, .(maxtimegrp = max(time)), by = grp]

  # joining MMC estimates onto group-time-combinations
  # then joining group specific maximum times and excluding times beyond last
  # observed time within groups.
  dt <- mcc_est[gt,on = c("grp", "time")]

  dt <- maxtimes[
    mcc_est[gt,on = c("grp", "time")],on = c("grp")
  ][time <= maxtimegrp][, maxtimegrp := NULL]

  dt$atrisk <- carryforward(dt$atrisk)
  dt$mcc <- carryforward(dt$mcc)
  dt$km <- carryforward(dt$km)
  dt$censored <- carryforward(dt$censored)
  dt$event_of_interest <- carryforward(dt$event_of_interest)
  dt$competing_event <- carryforward(dt$competing_event)

  return(dt)
}
