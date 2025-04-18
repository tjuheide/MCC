% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkData.R
\name{checkData}
\alias{checkData}
\title{Generic data check for obvious errors}
\usage{
checkData(data, idvar, timevar, eventvar, weightvar)
}
\arguments{
\item{data}{name of input data with an id, time, and event variable}

\item{idvar}{name of id variable in data}

\item{timevar}{name of time variable in data}

\item{eventvar}{name of event variable in data}

\item{weightvar}{(optional) name of weight variable}
}
\value{
a tibble with observations that appear faulty (if any), otherwise NULL
}
\description{
a check for obvious errors in the data, e.g., negative time, disallowed values of the event indicator, (potentially) illogical events: 1. death or censoring before last observation, 2. an event of interest on the last day of follow-up (the latter can be seen if the individual gets an event and is censored or died on the same day)
}
\examples{
df <- data.table::data.table(id = c(1,1,1,2,2),
                             time = c(231,53,253,41,87),
                             event = c(1,1,2,1,0),
                             weight = c(.9,.9,.9,1.07,1.07))
df2 <- data.table::data.table(id = c(1,1,1,2,2),
                              time = c(-31,53,253,41,87),
                              event = c(3,2,2,3,1),
                              weight = c(.9,.9,.92,1.07,1.07))
f <- checkData(df, "id", "time", "event", "weight")
f2 <- checkData(df2, "id", "time", "event", "weight")
}
