#' Convert colors to a given color model
#'
#' Convert a vector of R colors to a given color model
#'
#' @template colors
#' @template model
#'
#' @return A matrix containing the color components, except for the \code{css} model for which the result is a vector of css color definition strings.
#'
#' @examples
#' convert_color("red", model="rgb")
#' convert_color("red", model="hsl")
#' convert_color("red", model="hcl")
#' convert_color("red", model="cmyk")
#'
#' as.lab("red")
#' as.lch("red")
#' as.rgb(colors()[1:5])
#'
#' as.css("red")
#'
#' @export
convert_color <- function(x, model) {
  model <- match.arg(model, c("rgb", "hsv", "hsl", "hcl", "lch", "lab", "cmyk", "css"))
  
  # convert everything to a common color representation
  x <- chroma_r(x)
  
  # convert colors
  cmds <- paste0("chroma('", x, "').",model,"()")
  res <- v8_eval(cmds)
  
  # parse into a matrix of numbers
  if (model != "css") {
    # split the result string
    res <- strsplit(res, ",", fixed=TRUE)
    # convert to numbers
    res <- lapply(res, as.numeric)
    # stack in a matrix
    res <- do.call(rbind, res)
    # associate column names
    colnames(res) <- strsplit(model, "", fixed=TRUE)[[1]]
  }
  
  return(res)
}

#' @name convert_color
#' @export
as.rgb <- function(x) { convert_color(x, model="rgb") }

#' @name convert_color
#' @export
as.hsv <- function(x) { convert_color(x, model="hsv") }

#' @name convert_color
#' @export
as.hsl <- function(x) { convert_color(x, model="hsl") }

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
