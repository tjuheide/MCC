# Carry forward last non-NA

Function to let last non NA value of a variable be carried forward

## Usage

``` r
carryforward(x, na.rm = TRUE)
```

## Arguments

- x:

  name of variable for which last non NA value is to be repeated

- na.rm:

  If x starts with one or more NAs the leading NAs will be removed by
  default. Set na.rm = FALSE to retain leading NAs

## Value

a variable similar to the input but with non-leading NAs replaced by
last non-NA value. Leading non-NAs will either be removed or remain as
NAs.

## Examples

``` r
x <- c(NA, NA, NA, NA, 1, 3, 52, 5, 6, NA, 532, NA, NA, NA, 43)
y <- carryforward(x)
z <- carryforward(x, na.rm = FALSE)
```
