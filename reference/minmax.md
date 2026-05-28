# Longest shared follow-up

Function to identify the longest follow-up in the group with shortest
follow-up

## Usage

``` r
minmax(data)
```

## Arguments

- data:

  data.frame from the MCC-function with grp-parameter specified (i.e.
  with variable grp)

## Value

numeric value of the longest time observed in all groups

## Examples

``` r
mccOutput <- mcc(data = mcc_events,
                 idvar = "id",
                 timevar = "time",
                 eventvar = "event",
                 grpvar = "grp",
                 weightvar = "weight")
minmax(mccOutput)
#>    minmax
#>     <num>
#> 1:      5
```
