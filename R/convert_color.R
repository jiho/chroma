#' Convert colors to a given color model
#'
#' Convert a vector of R colors to a given color model
#'
#' @template rcolors
#' @template model
#'
#' @return A matrix containing the color components in most cases, except for the models:
#' \describe{
#'  \item{\code{css}}{a vector of css color definition strings}
#'  \item{\code{hex}}{a vector of hexadecimal strings defining colors}
#'  \item{\code{temperature}}{a vector of numbers corresponding to the temperature of the color in Kelvin}
#' }
#'
#' @export
#'
#' @examples
#' convert_color("red", model="rgb")
#' convert_color("red", model="gl")
#' convert_color("red", model="hsl")
#' convert_color("red", model="hcl")
#' convert_color("red", model="cmyk")
#'
#' as.hsv("red")
#' as.lab("red")
#' as.lch("red")
#'
#' as.css("red")
#' as.hex("red")
#' as.temperature("red")
#'
#' as.rgb(colors()[1:5])
convert_color <- function(x, model) {
  model <- match.arg(model, c("rgb", "gl", "hsv", "hsl", "hsi", "hcl", "lch", "lab", "cmyk", "css", "hex", "temperature"))
  
  # force input R colors into hex notation
  x <- in_hex(x)
  
  # convert colors
  cmds <- paste0("chroma('", x, "').",model,"()")
  res <- v8_eval(cmds)
  
  # parse into a matrix of numbers, when appropriate
  if (! model %in% c("css", "hex", "temperature")) {
    # split the result string
    res <- strsplit(res, ",", fixed=TRUE)
    # convert to numbers
    res <- lapply(res, as.numeric)
    # stack in a matrix
    res <- do.call(rbind, res)
    # associate column names
    if (model == "gl") { model <- "rgba" } # GL is just RGBA
    colnames(res) <- strsplit(model, "", fixed=TRUE)[[1]]
  }
  
  # convert temperatures to numbers
  if (model == "temperature") {
    res <- as.numeric(res)
  }
  
  # adjust the range of some channels to match their definition (which are not homogeneous in chroma.js)
  if ( model == "hcl" ) {
    res[,2:3] <- res[,2:3] / 100
  } else if ( model == "lch" ) {
    res[,1:2] <- res[,1:2] / 100
  } else if ( model == "lab" ) {
    res <- res / 100
  }
  
  return(res)
}

#' @name convert_color
#' @export
as.rgb <- function(x) { convert_color(x, model="rgb") }

#' @name convert_color
#' @export
as.rgba <- function(x) { convert_color(x, model="gl") }

#' @name convert_color
#' @export
as.gl <- function(x) { convert_color(x, model="gl") }

#' @name convert_color
#' @export
as.hsv <- function(x) { convert_color(x, model="hsv") }

#' @name convert_color
#' @export
as.hsl <- function(x) { convert_color(x, model="hsl") }

#' @name convert_color
#' @export
as.hsi <- function(x) { convert_color(x, model="hsi") }

#' @name convert_color
#' @export
as.hcl <- function(x) { convert_color(x, model="hcl") }

#' @name convert_color
#' @export
as.lch <- function(x) { convert_color(x, model="lch") }

#' @name convert_color
#' @export
as.lab <- function(x) { convert_color(x, model="lab") }

#' @name convert_color
#' @export
as.cmyk <- function(x) { convert_color(x, model="cmyk") }

#' @name convert_color
#' @export
as.css <- function(x) { convert_color(x, model="css") }

#' @name convert_color
#' @export
as.hex <- function(x) { convert_color(x, model="hex") }

#' @name convert_color
#' @export
as.temperature <- function(x) { convert_color(x, model="temperature") }
