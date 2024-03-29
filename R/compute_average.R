#' Average several colors
#'
#' @template param_x_rcolors
#' @inheritParams mix
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
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
#' # beware that numerical color averaging is not the same as blending
#' # pigments, in particular in non-rgb spaces
#' show_ave(c("#FEF213", "#A0BD71", "#146EFD"))
#' show_ave(c("#FEF213", "#A0BD71", "#146EFD"), model="hcl")
#' show_ave(c("#FEF213", "#A0BD71", "#146EFD"), model="hsv")
#' show_ave(c("#FEF213", "#A0BD71", "#146EFD"), model="lab")
average <- function(x, model="lrgb", na.rm=FALSE) {
  # remove NAs if specified
  if (na.rm) {x <- na.omit(x)}

  # force input R colors into hex notation
  x <- in_hex(x)

  # check arguments values
  model <- match.arg(model, c("hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "lrgb"), several.ok=FALSE)

  # average colors
  cmds <- stringr::str_c("chroma.average(['", stringr::str_c(x, collapse="','"), "'], '", model, "').hex()")
  v8_eval(cmds)
}
