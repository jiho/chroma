#' Average several colors
#'
#' @template param_x_rcolors
#' @param model color model to compute the averaging in; valid models are \code{rgb}, \code{lrgb}, \code{hsv}, \code{hsl}, \code{hcl}, \code{lab}.
#'
#' @template return_hex_colors
#'
#' @family color manipulation functions
#'
#' @export
#'
#' @examples
#' average(c("red", "blue"))
#' average(c("red", "blue", "green"))
#'
#' # show the input colors, then white, then the average
#' show_ave <- function(x, ...) {show_col(c(x, "white", average(x, ...)))}
#'
#' show_ave(c("#FEF213", "#146EFD"))
#' show_ave(hue_colors(5))
#' show_ave(brewer_colors(5, name="Set3"))
#' show_ave(cubehelix_colors(5))
#' # so averaging produces brown, most of the time, but not always:
#' show_ave(brewer_colors(5, name="Greens"))
#'
#' # beware, numerical color averaging is not the same as blending pigments
#' show_ave(c("yellow", "red"))
#' show_ave(c("blue", "red"))
#' # but
#' show_ave(c("yellow", "blue"))
#' show_ave(c("yellow", "blue"), model="rgb")
#' show_ave(c("yellow", "blue"), model="hsv")
average <- function(x, model="lab") {

  # force input R colors into hex notation
  x <- in_hex(x)

  # check arguments values
  model <- match.arg(model, c("rgb", "lrgb", "hsv", "hsl", "hcl", "lab"), several.ok=FALSE)

  # average colors
  cmds <- stringr::str_c("chroma.average(['", stringr::str_c(x, collapse="','"), "'], '", model, "').hex()")
  v8_eval(cmds)
}
