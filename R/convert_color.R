#' Convert colors to a given color model
#'
#' Convert a vector of colors to a given color model.
#'
#' @template param_x_rcolors
#' @template param_model
#'
#' @details All colors in chroma are represented internally as hex codes in sRGB space. So if a color exists in that space, converting it to most other models will be loossless and reversible. For example, converting from \code{rgb} to \code{lab} back to \code{rgb} should give the same \code{rgb} values. If the starting color is not representable in sRGB, it will be converted to the closest sRGB color and the reversibility will be lost. In addition, the \code{temperature} and \code{wavelength} color "models" are very different because they represent only one aspect of the color (its temperature or its corresponding wavelength) and conversion to those is almost never lossless or reversible.
#'
#' @return A matrix containing the color components in most cases, except for the models:
#' \describe{
#'  \item{\code{css}}{a vector of css color definition strings,}
#'  \item{\code{hex}}{a vector of hexadecimal strings defining colors,}
#'  \item{\code{temperature}}{a vector of numbers corresponding to the temperature of the colors in Kelvin,}
#'  \item{\code{wavelength}}{a vector of numbers corresponding to the wavelength of monochromatic light closest to the input colors.}
#' }
#'
#' @seealso \code{\link{parse_color}} for a general function to parse colors in various specifications.
#'
#' @export
#'
#' @examples
#' convert_color("red", model="rgb")
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
#' as.wavelength("red")
#'
#' # Can be vectorised
#' as.rgb(colors()[1:5])
#' as.rgb(c("#B55FFC", "blue", "purple", "#6A9F16"))
#'
#' # Starting from sRGB leads to reversibility
#' col <- rgb(150, 100, 200, maxColorValue=255)
#' as.cmyk(col)
#' as.rgb(cmyk(as.cmyk(col)))
#' # or near-reversability
#' col <- lab(0.5, 0.5, 0)
#' col
#' as.lab(col)
#'
#' # But starting outside of sRGB looses reversibility
#' col <- lab(0.5, -0.5, -1)
#' # this L*a*b* color does not exist in sRGB => it is converted to the
#' # closest sRGB equivalent
#' col
#' # and is different from the original L*a*b* specification
#' as.lab(col)
convert_color <- function(x, model) {
  model <- match.arg(model, c("rgb", "rgba", "gl", "hsv", "hsl", "hsi", "hcl", "lch", "lab", "cmyk", "css", "hex", "temperature", "wavelength"))
  # we want rgba in [0,1] = gl
  if (model == "rgba") { model <- "gl" }

  # force input R colors into hex notation
  x <- in_hex(x)

  # convert colors
  if (model == "wavelength") {
    # with custom code
    res <- convert_wavelength(x)
  } else {
    # through chroma.js
    cmds <- stringr::str_c("chroma('", x, "').",model,"()")
    res <- v8_eval(cmds)
  }

  # parse into a matrix of numbers, when appropriate
  if (! model %in% c("css", "hex", "temperature", "wavelength")) {
    if (model == "gl") { model <- "rgba" } # we need rgba as column headers

    # if all colors are NA, force the result to be a matrix of the correct dimension
    if (all(is.na(res))) {
      res <- matrix(nrow=length(x), ncol=stringr::str_length(model))
    }
    # otherwise, parse the results normally
    else {
      # split the result string
      res <- strsplit(res, ",", fixed=TRUE)
      # convert to numbers
      res <- lapply(res, as.numeric)
      # stack in a matrix
      res <- do.call(rbind, res)
    }

    # associate column names
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
  } else if ( model == "rgb" ) {
    res <- res / 255
  }

  return(res)
}

#' @rdname convert_color
#' @export
as.rgb <- function(x) { convert_color(x, model="rgb") }

#' @rdname convert_color
#' @export
as.rgba <- function(x) { convert_color(x, model="rgba") }

#' @rdname convert_color
#' @export
as.hsv <- function(x) { convert_color(x, model="hsv") }

#' @rdname convert_color
#' @export
as.hsl <- function(x) { convert_color(x, model="hsl") }

#' @rdname convert_color
#' @export
as.hsi <- function(x) { convert_color(x, model="hsi") }

#' @rdname convert_color
#' @export
as.hcl <- function(x) { convert_color(x, model="hcl") }

#' @rdname convert_color
#' @export
as.lch <- function(x) { convert_color(x, model="lch") }

#' @rdname convert_color
#' @export
as.lab <- function(x) { convert_color(x, model="lab") }

#' @rdname convert_color
#' @export
as.cmyk <- function(x) { convert_color(x, model="cmyk") }

#' @rdname convert_color
#' @export
as.css <- function(x) { convert_color(x, model="css") }

#' @rdname convert_color
#' @export
as.hex <- function(x) { convert_color(x, model="hex") }

#' @rdname convert_color
#' @export
as.temperature <- function(x) { convert_color(x, model="temperature") }

#' @rdname convert_color
#' @export
as.wavelength <- function(x) { convert_color(x, model="wavelength") }

