# Expand MCC-estimates to all times

If MCCs are to be compared across groups it is nice to have an estimate
in both groups at the relevant time point. Likewise, if bootstrapping is
used to estimate uncertainty there should be the same number of
bootstrap estimates at all times. This function ensures that there is an
observation in all groups at all relevant times. Additional times can be
requested by specifying the times parameter. Estimates are only provided
in a group if there is at least one individual at risk at the given
time. As the input should come from the mcc-function it is assumed to
contain variables time and grp.

## Usage

``` r
expandMCC(mcc_est, times = NULL)
```

## Arguments

- mcc_est:

  data.table from the MCC-function.

- times:

  single value or vector of specific times for which an estimate of the
  MCC is needed. Default NULL only times observed in the data will be
  outputted.

## Value

data.table with an MCC-estimate at each time-point observed in either
level of grp, or specified by paramter times. E.g., if there is an
observation at time T in grp A, then in the other levels of grp there
will be created an observation at time T if it does not exist already.
If a vector of times is provided, an observation will be created for
these times as well in all levels of grp.

## Examples

``` r
data("mcc_events")
mccOutput <- mcc(data = mcc_events,
                 idvar = "id",
                 timevar = "time",
                 eventvar = "event",
                 grpvar = "grp",
                 weightvar = "weight")
mccOutputExpand <- expandMCC(mccOutput)
mccOutputExpand <- expandMCC(mccOutput, c(1, 5, 10))
#> [1] "Warning:"
#> [1] "A specified time point exceeds the follow-up in a group."
#> [1] "Estimates beyond the observed follow-up will not be made."
```
