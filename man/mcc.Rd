% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcc.R
\name{mcc}
\alias{mcc}
\title{Mean Cumulative Count for Recurrent Events}
\usage{
mcc(data, idvar, timevar, eventvar, grpvar, weightvar)
}
\arguments{
\item{data}{a data.table with study id, time, and event indicator, optionally group or weight variables. The last observed event for each id must be either censoring or a competing event while all intermediate events must be event of interest for the function to return the correct estimate.}

\item{idvar}{variable identifying the unique individuals, there should be one observation within each id for each event of interest plus one. The last observation should correspond to what happens at the end of follow-up, either the individual is censored or experiences a competing event}

\item{timevar}{Time variable, the time points from start of follow-up at which the events happen and when the individual is no longer followed}

\item{eventvar}{Event indicator, 1 = event of interest, 2 = competing event at end of follow-up, 0 = individual censored at end of follow-up}

\item{grpvar}{(optional) Variable defining groups within which the MCC should be computed. It is assumed individuals will not switch between groups during follow-up.}

\item{weightvar}{(optional) Variable specifying the weight of the individual. This is assumed to be constant throughout follow-up}
}
\value{
A data.table with one observation for each time an event has happened. The output has the following variables:
\itemize{
\item time, time of follow-up
\item atrisk, (weighted) number at risk at the current time
\item km, (weighted) Kaplan-Meier estimator of (competing risk-free) survival
\item mcc, (weighted) the mean cumulative count of the event of interest
\item censored, (weighted) number of censored at a given time
\item event_of_interest, (weighted) number of events of interest at a given time
\item competing_event (weighted) number of competing events at a given time
\item grp: (only if grp parameter is specified), the value within with the MCC is estimated
}
}
\description{
Function estimating the mean cumulative count of a recurrent event taking competing risks into account
The function allows for weighted observations and can computed counts with levels of a group variable
}
\examples{
data("mcc_events")
x <- mcc(data = mcc_events,
         idvar = "id",
         timevar = "time",
         eventvar = "event",
         grpvar = "grp",
         weightvar = "weight")

}
\references{
Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.. Estimating the burden of recurrent events in the presence of competing risks: the method of mean cumulative count. Am J Epidemiol. 2015;181(7):532-540. doi:10.1093/aje/kwu289
}
