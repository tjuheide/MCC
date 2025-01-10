#' Mean Cumulative Count for Recurrent Events
#'
#' @description
#' Function estimating the mean cumulative count of a recurrent event taking competing risks into account
#' The function allows for weighted observations and can computed counts with levels of a group variable
#'
#' @param data a data.table with study id, time, and event indicator, optionally group or weight variables. The last observed event for each id must be either censoring or a competing event while all intermediate events must be event of interest for the function to return the correct estimate.
#' @param idvar variable identifying the unique individuals, there should be one observation within each id for each event of interest plus one. The last observation should correspond to what happens at the end of follow-up, either the individual is censored or experiences a competing event
#' @param timevar Time variable, the time points from start of follow-up at which the events happen and when the individual is no longer followed
#' @param eventvar Event indicator, 1 = event of interest, 2 = competing event at end of follow-up, 0 = individual censored at end of follow-up
#' @param grpvar (optional) Variable defining groups within which the MCC should be computed. It is assumed individuals will not switch between groups during follow-up.
#' @param weightvar (optional) Variable specifying the weight of the individual. This is assumed to be constant throughout follow-up
#'
#' @returns A data.table with one observation for each time an event has happened. The output has the following variables:
#' * time, time of follow-up
#' * atrisk, (weighted) number at risk at the current time
#' * km, (weighted) Kaplan-Meier estimator of (competing risk-free) survival
#' * mcc, (weighted) the mean cumulative count of the event of interest
#' * censored, (weighted) number of censored at a given time
#' * event_of_interest, (weighted) number of events of interest at a given time
#' * competing_event (weighted) number of competing events at a given time
#' * grp: (only if grp parameter is specified), the value within with the MCC is estimated
#' @export
#'
#' @examples
#' data("mcc_events")
#' x <- mcc(data = mcc_events,
#'          idvar = "id",
#'          timevar = "time",
#'          eventvar = "event",
#'          grpvar = "grp",
#'          weightvar = "weight")
#'
#' @references Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.. Estimating the burden of recurrent events in the presence of competing risks: the method of mean cumulative count. Am J Epidemiol. 2015;181(7):532-540. doi:10.1093/aje/kwu289

mcc <- function(data, idvar, timevar, eventvar, grpvar, weightvar){
  # check if input data is data.table
  if(!is.data.table(data)) stop("data is not a data.table")

  # if grp and/or weight are unspecified the variables are
  # added to the data
  # regardless the data is passed to a new data frame and variables
  # are given specific names
  ms <- c(grpmiss = missing(grpvar), weightmiss = missing(weightvar))

  if(ms['grpmiss'] == TRUE & ms['weightmiss'] == TRUE) {
    .data <- data[, .(id = get(idvar),
                      time = get(timevar),
                      event = factor(get(eventvar),
                                     levels = 0:2,
                                     labels = c("event0", "event1", "event2")),
                      grp = 1,
                      weight = 1)]
  } else if(ms['grpmiss'] == FALSE & ms['weightmiss'] == TRUE) {
    .data <- data[, .(id = get(idvar),
                      time = get(timevar),
                      event = factor(get(eventvar),
                                     levels = 0:2,
                                     labels = c("event0", "event1", "event2")),
                      grp = get(grpvar),
                      weight = 1)]
  } else if(ms['grpmiss'] == TRUE & ms['weightmiss'] == FALSE) {
    .data <- data[, .(id = get(idvar),
                      time = get(timevar),
                      event = factor(get(eventvar),
                                     levels = 0:2,
                                     labels = c("event0", "event1", "event2")),
                      grp = 1,
                      weight = get(weightvar))]
  } else if(ms['grpmiss'] == FALSE & ms['weightmiss'] == FALSE) {
    .data <- data[, .(id = get(idvar),
                      time = get(timevar),
                      event = factor(get(eventvar),
                                     levels = 0:2,
                                     labels = c("event0", "event1", "event2")),
                      grp = get(grpvar),
                      weight = get(weightvar))]
  }

  # numbers at risk at time 0 within groups, all event types are set to 0
  initialn <- unique(.data[, .(grp, id, weight)])
  initialn <- initialn[, .(time = 0,
                           event0 = 0,
                           event1 = 0,
                           event2 = 0,
                           atrisk = sum(weight),
                           first = 1),
                       by = grp]

  # summarizing the different types of events (weighted) at each time
  events <- .data[, .(n = sum(weight)),
                  by = c("grp", "time", "event")]

  # turning the data to wide format with numbers of events at each time,
  # one column for each type of event
  # (...and group and time and atrisk filler variable)
  # one row for each group-time-combination
  wide <- dcast(events,
                grp + time ~ event,
                value.var = "n", fill = 0)[,
                                           `:=`(atrisk = 0,
                                                first = 2)]

  # initial numbers at risk and number of event at each time is combined to
  # one data frame
  # return(list(initialn, wide))
  alltimes <- rbindlist(list(initialn, wide))
  setorder(alltimes, grp, first, time)

  # numbers at risk update for each time,
  # proportion surviving and number of events at each time computed
  # the latter 2 are aggregated to the KM- and MCC-estimators
  # redundant variables removed
  mcc <- alltimes[
    ,`:=`(atrisk = max(atrisk, na.rm = TRUE) -
            cumsum(event0) -
            cumsum(event2)),
    by = grp
  ][
    ,`:=` (kmprime = ifelse(first == 1,
                            1,
                            1 - event2 /
                              shift(atrisk,type = "lag"))),
    by = grp
  ][
    , `:=`(km = cumprod(kmprime)),
    by = grp
  ][, `:=`(mccprime = ifelse(first == 1,
                             0,
                             event1 /
                               shift(atrisk,type = "lag") *
                               shift(km, type = "lag"))),
    by = grp
  ][, `:=`(mcc = cumsum(mccprime),
           censored = event0,
           event_of_interest = event1,
           competing_event = event2),
    by = grp
  ][, `:=`(event0 = NULL,
           event1 = NULL,
           event2 = NULL,
           mccprime = NULL,
           kmprime = NULL,
           first = NULL)]


  return(mcc)
}
