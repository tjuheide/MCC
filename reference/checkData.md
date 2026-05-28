# Generic data check for obvious errors

a check for obvious errors in the data, e.g., negative time, disallowed
values of the event indicator, (potentially) illogical events: 1. death
or censoring before last observation, 2. an event of interest on the
last day of follow-up (the latter can be seen if the individual gets an
event and is censored or died on the same day)

## Usage

``` r
checkData(data, idvar, timevar, eventvar, weightvar)
```

## Arguments

- data:

  name of input data with an id, time, and event variable

- idvar:

  name of id variable in data

- timevar:

  name of time variable in data

- eventvar:

  name of event variable in data

- weightvar:

  (optional) name of weight variable

## Value

a tibble with observations that appear faulty (if any), otherwise NULL

## Examples

``` r
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
#> [1] "Warning: the following observation(s) suggest(s) incorrect data"
```
