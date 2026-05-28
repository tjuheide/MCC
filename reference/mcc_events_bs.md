# Times and events across 500 bootstrap iterations

Events and times with weights within 2 different groups. Data are
simulated

## Usage

``` r
mcc_events_bs
```

## Format

A data frame with 2539 rows and 5 variables:

- BootstrapSample:

  Bootstrap identifier

- id:

  ID

- grp:

  Treatment group

- weight:

  Weight of the ID

- time:

  time of the event

- event:

  event type: 0 = censored, 1 = event of interest, 2 = death
