#' Generic data check for obvious errors
#'
#' @description
#' a check for obvious errors in the data, e.g., negative time, disallowed values of the event indicator, (potentially) illogical events: 1. death or censoring before last observation, 2. an event of interest on the last day of follow-up (the latter can be seen if the individual gets an event and is censored or died on the same day)
#'
#' @param data name of input data with an id, time, and event variable
#' @param idvar name of id variable in data
#' @param timevar name of time variable in data
#' @param eventvar name of event variable in data
#' @param weightvar (optional) name of weight variable
#'
#' @returns a tibble with observations that appear faulty (if any), otherwise NULL
#' @export
#'
#' @examples
#' df <- data.table::data.table(id = c(1,1,1,2,2),
#'                              time = c(231,53,253,41,87),
#'                              event = c(1,1,2,1,0),
#'                              weight = c(.9,.9,.9,1.07,1.07))
#' df2 <- data.table::data.table(id = c(1,1,1,2,2),
#'                               time = c(-31,53,253,41,87),
#'                               event = c(3,2,2,3,1),
#'                               weight = c(.9,.9,.92,1.07,1.07))
#' f <- checkData(df, "id", "time", "event", "weight")
#' f2 <- checkData(df2, "id", "time", "event", "weight")

checkData <- function(data, idvar, timevar, eventvar, weightvar){

  cdata <- data

  # the individual should either be censored (0) or have a competing event (2) on their last day of follow-up,

  endsInEvent <- cdata[,
                       .(id = get(idvar),
                         time = get(timevar),
                         event = ifelse(get(eventvar) == 0,
                                        9,
                                        get(eventvar)),
                         comment = "Event for last obs must be 0 or 2")]
  setorder(endsInEvent, id, time, event)
  endsInEvent <- endsInEvent[endsInEvent[,
                                         is.bad := (.I == last(.I) & event == 1),
                                         by = id]$is.bad] |>
    _[,is.bad := NULL]

  # intermediate observations should all be 1
  eventAfterCensComp <- cdata[,
                              .(id = get(idvar),
                                time = get(timevar),
                                event = ifelse(get(eventvar) == 0,
                                               9,
                                               get(eventvar)),
                                comment = "Events before last obs must be 1")]
  setorder(eventAfterCensComp, id, time, event)
  eventAfterCensComp <- eventAfterCensComp[
    eventAfterCensComp[,is.bad := (.I < last(.I) & event != 1),
                       by = id]$is.bad] |>
    _[,event := ifelse(event == 9, 0, event)] |>
    _[,is.bad := NULL]


  # events should be 0, 1, or 2 and times must be non-negative
  badEventCodesNegTime <- cdata[
    ,
    .(id = get(idvar),
      time = get(timevar),
      event = get(eventvar),
      comment = ifelse(event %in% c(0, 1, 2),
                       "Time must be non-negative",
                       ifelse(time >= 0,
                              "Events must be coded 0, 1, 2",
                              "Time must be non-negative and events must be coded 0, 1, 2")))]
  badEventCodesNegTime <- badEventCodesNegTime[
    badEventCodesNegTime[, is.bad := (!(event %in% 0:2) | time < 0 | is.na(time) | is.na(event))]$is.bad,]
  badEventCodesNegTime[, is.bad := NULL]

  badObservations <- rbindlist(list(endsInEvent,
                                    eventAfterCensComp,
                                    badEventCodesNegTime))
  # weights are assumed to be constant throughout follow-up
  if(!missing(weightvar)){
    varyingWeights <-cdata[
      cdata[,
            is.bad := (max(get(weightvar) > min(get(weightvar)))),
            by = id]$is.bad,
      .(id = get(idvar),
        time = NA,
        event = NA,
        comment = "Weights vary for this individual")]
    varyingWeights <- unique(varyingWeights)


    badObservations <- rbindlist(list(badObservations,unique(varyingWeights)))
  }


  if(nrow(badObservations) > 0) {
    print("Warning: the following observation(s) suggest(s) incorrect data")
    return(badObservations)
  }
  if(nrow(badObservations) == 0) {
    return(NULL)
  }
}
