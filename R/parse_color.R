#' Parse colors specified in a given model
#'
#' @param x a matrix or data.frame whose columns specify the color channels.
#' @template model
#'
#' @family color specifications
#'
#' @return A vector of colors specified as hex codes
#'
#' @examples
#' parse_color(data.frame(h=c(0, 120, 240), s=0.5, v=0.7), "hsv")
#' parse_color(data.frame(r=c(255, 0, 0),
#'                        g=c(0, 255, 0),
#'                        b=c(0, 0, 255)), "rgb")
#'
#' @export
parse_color <- function(x, model) {
  # recognise color model
  model <- match.arg(model, c("rgb", "gl", "hsv", "hsl", "hcl", "lch", "lab", "cmyk", "css"))
  # TODO add temperature
  # TODO add hsi?

  # check arguments
  if (model == "css") {
    
    x <- as.character(x)
    # TODO more intense check here
    
  } else {
    
    if (!(is.matrix(x) | is.data.frame(x))) {
      stop("x should be a matrix or data.frame")
    }

    required_columns <- switch(model, cmyk = 4, 3)
    if (ncol(x) < required_columns) {
      stop("x should have at least ", required_columns, " columns")
    }
    if (ncol(x) > required_columns) {
      warning("only the first ", required_columns, " columns of x will be used")
      x <- x[1:required_columns]
    }

  }
  
  # check that the values in each channel are in the appropriate range
  # enforce all "percentages" to be in [0,1]; this is not as homogeneous in chroma.js and some values need to be modified before being passed to javascript
  if (model == "rgb") {
    is_in(x[,1], 0, 255, "r")
    is_in(x[,2], 0, 255, "g")
    is_in(x[,3], 0, 255, "b")
    
  } else if (model == "gl") {
    is_in(x[,1], 0, 1, "r")
    is_in(x[,2], 0, 1, "g")
    is_in(x[,3], 0, 1, "b")
    
  } else if ( model == "hsv" ) {
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1, "s")
    is_in(x[,3], 0, 1, "v")
    
  } else if ( model == "hsl" ) {
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1, "s")
    is_in(x[,3], 0, 1, "l")
    
  } else if ( model == "hcl" ) {
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1, "c")
    is_in(x[,3], 0, 1, "l")
    # chroma.js actually uses percentages
    x[,2:3] <- x[,2:3] * 100
    
  } else if ( model == "lch" ) {
    is_in(x[,1], 0, 1, "l")
    is_in(x[,2], 0, 1, "c")
    is_in(x[,3], 0, 360, "h")
    # chroma.js actually uses percentages
    x[,1:2] <- x[,1:2] * 100
    
  } else if ( model == "lab" ) {
    is_in(x[,1], 0, 1, "l")
    is_in(x[,2], -1, 1, "a")
    is_in(x[,3], -1, 1, "b")
    # chroma.js actually uses percentages
    x <- x * 100
    
  } else if ( model == "cmyk" ) {
    is_in(x[,1], 0, 1, "c")
    is_in(x[,2], 0, 1, "m")
    is_in(x[,3], 0, 1, "y")
    is_in(x[,4], 0, 1, "k")
    
  }
  
  # parse colors using chroma.js
  if (model == "css") {
    cmds <- apply(x, 1, function(xx) {
      paste0("chroma.", model, "('", xx, "').hex()")
    })
  } else {
    cmds <- apply(x, 1, function(xx) {
      paste0("chroma.", model, "([", paste0(xx, collapse=","), "]).hex()")
    })
  }
  res <- v8_eval(cmds)
  
  return(res)
}

# Reformat arguments for color parsing functions
# - allow matrix/data.frame as first argument
# - perform value recycling for vector arguments
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

# Utility function to check color channels ranges for color parsing functions
is_in <- function(x, min, max, name="x") {
  pbs <- which(x < min | x > max)
  n <- length(pbs)
  if (n > 0) {
    stop(name, " should be in [", min, ",", max, "]. ", n, " values are not, at positions: ", ifelse(n <= 5, paste0(pbs, collapse=", "), paste0(paste0(pbs[1:5], collapse=", "), ", ...")), call.=FALSE)
  }
  return(invisible(pbs))
}
