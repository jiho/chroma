#' Change color saturation
#'
#' Saturate or desaturate colors
#'
#' @template param_x_rcolors
#' @param amount numeric amount of change in saturation. Reasonable amounts are 1 to 10. Negative amounts correspond to the opposite operation (\code{saturate("red", -1) = desaturate("red", 1)}.)
#'
#' @details
#' Colors are converted into HCL space where the C (chroma) component is changed. Most colors are (very slightly) affected by the conversion and the change in saturation is therefore not exactly reversible (saturating a desaturated color will not get you back to the original one); although, perceptually, the changes should be extremely subtle and only affect very bright and saturated colors.
#'
#' @template return_hex_colors
#'
#' @family color manipulation functions
#'
#' @export
#'
#' @examples
#' saturate("#7BBBFE")
#' saturate(c("coral1", "#850E5D"))
#' desaturate("darkgreen")
#'
#' show_col(c("hotpink", saturate("hotpink"), desaturate("hotpink")),
#'          c("lightskyblue", saturate("lightskyblue"), desaturate("lightskyblue")))
#' show_col(c(desaturate("salmon3", 2),
#'            desaturate("salmon3"),
#'            "salmon3",
#'            saturate("salmon3"),
#'            saturate("salmon3", 2)))
#'
#' # saturate() and desaturate() are opposite operations, the direction of
#' # which is set by the sign of `amount`
#' saturate("red", -1)
#' desaturate("red", 1)
#'
#' # But they are not necessarily exactly reversible when they operate near
#' # extreme saturation values
#' col <- "#5EFF15"
#' (new_col <- desaturate(saturate(col)))
#' show_col(c(col, new_col))
#' # = the two greens are slightly different
saturate <- function(x, amount=1) {
  # force input R colors into hex notation
  x <- in_hex(x)

  # manipulate colors
  cmds <- stringr::str_c("chroma('", x, "').saturate(",amount,").hex()")
  v8_eval(cmds)
}

#' @rdname saturate
#' @export
desaturate <- function(x, amount=1) {
  saturate(x, amount=-amount)
  # NB: this is how desaturate is actually implemented in chroma.js
}
