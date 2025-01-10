#' Times and events across 500 bootstrap iterations
#'
#' Events and times with weights within 2 different groups. Data are simulated
#'
#' @format A data frame with 2539 rows and 5 variables:
#' \describe{
#'   \item{BootstrapSample}{Bootstrap identifier}
#'   \item{id}{ID}
#'   \item{grp}{Treatment group}
#'   \item{weight}{Weight of the ID}
#'   \item{time}{time of the event}
#'   \item{event}{event type: 0 = censored, 1 = event of interest, 2 = death}
#' }
"mcc_events_bs"
