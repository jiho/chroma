#' Insert NAs into a vector based on another one
#'
#' @param x vector without NAs
#' @param from vector from which the NAs must be inserted in x
#' @examples
#' x <- c(1, NA, 2)
#' y <- na.omit(x)
#' na_insert(y^2, from=x)
na_insert <- function(x, from) {
  if (length(x) == length(from)) {
    x[is.na(from)] <- NA
  } else if ((length(from) - sum(is.na(from))) == length(x)) {
    x <- replace(from, !is.na(from), x)
  } else {
    stop("Incompatible number of missing values")
  }
  return(x)
}
