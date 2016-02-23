# Reformat arguments into "table" form (for color parsing functions)
# - allow matrix/data.frame as first argument, in that case, only use it
# - perform value recycling for vector arguments and check that they are of compatible length
tabularise_arguments <- function(...) {
  # get the list of arguments
  args <- list(...)
  
  if ( is.matrix(args[[1]]) | is.data.frame(args[[1]]) ) {
    # if first argument is a "table", use this only
    x <- args[[1]]

  } else {
    # else, convert the arguments into a "table"
    # NB: this does argument recycling by itself
    #     we just provide a nicer error message
    tryCatch(x <- data.frame(args),
      error=function(e) {
        stop("Arguments have incompatible lengths: ", paste0(sapply(args, length), collapse=", "), call.=FALSE)
      }
    )
  }
  
  return(x)
}


# Check that values fall within a range
# (in particular for color channels in color parsing functions)
# @param x vector of values to check
# @param min,max valid range
# @param name of the argument used for the error message (by default the name of the object provided as \code{x})
is_in <- function(x, min, max, name=deparse(substitute(x))) {
  pbs <- which(x < min | x > max)
  n <- length(pbs)
  if (n > 0) {
    stop(
      name, " should be in [", min, ",", max, "]. ",
      ifelse(n == 1, 
        paste("Element", pbs, "of", name, "is not."),
        paste("Elements",
          ifelse(n <= 5,
            paste0(pbs, collapse=", "),
            paste0(paste0(pbs[1:5], collapse=", "), "... and ", n-5, " others")
          ),
          "are not."
        )
      ),
      call.=FALSE
    )
  }
  return(invisible(pbs))
}
