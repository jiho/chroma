#' Parse colors specified in a given model
#'
#' @param x a matrix or data.frame whose columns specify the color channels or a vector of color definitions for the \code{css}, \code{hex}, \code{temperature}, \code{wavelength} color models.
#' @template param_model
# TODO change model to mode, everywhere
#'
#' @template return_hex_colors
#'
#' @family color specifications
#' @seealso \code{\link{convert_color}} to convert parsed colors to another model.
#'
#' @export
#'
#' @examples
#' parse_color(data.frame(h=c(0, 120, 240), s=0.5, v=0.7), model="hsv")
#' parse_color(data.frame(r=c(255, 0, 0),
#'                        g=c(0, 255, 0),
#'                        b=c(0, 0, 255)), model="rgb")
parse_color <- function(x, model) {
  # recognise color model
  model <- match.arg(model, c("rgb", "rgba", "ryb", "gl", "hsv", "hsl", "hsi", "hcl", "lch", "lab", "cmyk", "css", "hex", "temperature", "wavelength"))

  # check arguments
  vector_color_models <- c("css", "hex", "temperature", "wavelength")
  if (model %in% vector_color_models) {

    if ( !is.vector(x) ) {
      stop("x should be a vector")
    }

    if (model %in% c("css", "hex")) {
      if ( !is.character(x) ) {
        warning("x converted to character for color parsing")
        x <- as.character(x)
      }
    }

  } else {

    if (!(is.matrix(x) | is.data.frame(x))) {
      stop("x should be a matrix or data.frame")
    }

    required_columns <- switch(model, cmyk=4, rgba=4, gl=4, 3)
    if (ncol(x) < required_columns) {
      stop("x should have at least ", required_columns, " columns")
    }
    if (ncol(x) > required_columns) {
      warning("only the first ", required_columns, " columns of x will be used")
      x <- x[,1:required_columns,drop=F]
    }

  }

  # check that the values in each channel are in the appropriate range
  # enforce all "percentages" to be in [0,1]; this is not as homogeneous in chroma.js and some values need to be modified before being passed to javascript
  if (model == "rgb") {
    is_in(x[,1], 0, 255, "red")
    is_in(x[,2], 0, 255, "green")
    is_in(x[,3], 0, 255, "blue")

  } else if (model %in% c("rgba", "gl")) {
    is_in(x[,1], 0, 1, "red")
    is_in(x[,2], 0, 1, "green")
    is_in(x[,3], 0, 1, "blue")
    is_in(x[,4], 0, 1, "alpha")
    model <- "gl"

  } else if (model == "ryb") {
    is_in(x[,1], 0, 1, "red")
    is_in(x[,2], 0, 1, "yellow")
    is_in(x[,3], 0, 1, "blue")

  } else if ( model == "hsv" ) {
    x[,1] <- hue(x[,1], model=model)
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1, "s")
    is_in(x[,3], 0, 1, "v")

  } else if ( model == "hsl" ) {
    x[,1] <- hue(x[,1], model=model)
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1, "s")
    is_in(x[,3], 0, 1, "l")

  } else if ( model == "hsi" ) {
    x[,1] <- hue(x[,1], model=model)
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1, "s")
    is_in(x[,3], 0, 2, "i")

  } else if ( model == "hcl" ) {
    x[,1] <- hue(x[,1], model=model)
    is_in(x[,1], 0, 360, "h")
    is_in(x[,2], 0, 1.5, "c")
    is_in(x[,3], 0, 1, "l")
    # chroma.js actually uses percentages
    x[,2:3] <- x[,2:3] * 100

  } else if ( model == "lch" ) {
    is_in(x[,1], 0, 1, "l")
    is_in(x[,2], 0, 1.5, "c")
    x[,3] <- hue(x[,3], model=model)
    is_in(x[,3], 0, 360, "h")
    # chroma.js actually uses percentages
    x[,1:2] <- x[,1:2] * 100

  } else if ( model == "lab" ) {
    is_in(x[,1], 0, 1, "l")
    is_in(x[,2], -1.1, 1.1, "a")
    is_in(x[,3], -1.1, 1.1, "b")
    # chroma.js actually uses percentages
    x <- x * 100

  } else if ( model == "cmyk" ) {
    is_in(x[,1], 0, 1, "c")
    is_in(x[,2], 0, 1, "m")
    is_in(x[,3], 0, 1, "y")
    is_in(x[,4], 0, 1, "k")

  } else if ( model == "temperature" ) {
    is_in(x, 1000, 40000, "temperature")

  } else if (model == "wavelength") {
    x <- as.numeric(x)
  }

  # parse colors
  if (model == "wavelength") {
    # parse using custom code
    res <- parse_wavelength(x)
  } else if (model == "ryb") {
    res <- parse_ryb(x)
  } else {
    # parse using chroma.js
    if (model %in% vector_color_models) {
      cmds <- sapply(x, function(xx) {
        stringr::str_c("chroma.", model, "('", xx, "').hex()")
      }, USE.NAMES=FALSE)
    } else {
      cmds <- apply(x, 1, function(xx) {
        stringr::str_c("chroma.", model, "([", stringr::str_c(xx, collapse=","), "]).hex()")
      })
    }

    res <- v8_eval(cmds)
  }
  return(res)
}
