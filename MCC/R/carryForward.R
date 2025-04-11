#' Carry forward last non-NA
#'
#' @description
#' Function to let last non NA value of a variable be carried forward
#'
#' @param x name of variable for which last non NA value is to be repeated
#' @param na.rm If x starts with one or more NAs the leading NAs will be removed by default. Set na.rm = FALSE to retain leading NAs
#'
#' @returns a variable similar to the input but with non-leading NAs replaced by last non-NA value. Leading non-NAs will either be removed or remain as NAs.
#' @export
#'
#' @examples
#' x <- c(NA, NA, NA, NA, 1, 3, 52, 5, 6, NA, 532, NA, NA, NA, 43)
#' y <- carryforward(x)
#' z <- carryforward(x, na.rm = FALSE)
carryforward <- function(x, na.rm = TRUE) {
  nonNAs <- !is.na(x)
  nonNAVals <- x[nonNAs]
  indexPrevNonNA <- cumsum(nonNAs)
  y <- nonNAVals[indexPrevNonNA]

  if(na.rm == FALSE){
    y <- c(rep(NA,length(x) - length(y)),y)
  }

  return(y)
}





