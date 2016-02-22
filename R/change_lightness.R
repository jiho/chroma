#' Change color lightness
#'
#' Darken or brighten colors
#'
#' @template colors
#' @param amount numeric amount of change in lightness. Reasonable amounts are 1 to 10. Negative amounts correspond to the opposite operation (\code{darken("red", -1) = brighten("red", 1)}.)
#'
#' @details
#' Colors are converted into L* a* b* space where the L* component is changed. Most colors are (very slightly) affected by the conversion and the change in lightness is therefore not exactly reversible (brightening a darkened color will not get you back to the original one); although, perceptually, the changes should be extremely subtle and only affect very bright colors.
#' 
#' @template color_manip
#'
#' @examples
#' darken("#7BBBFE")
#' darken(c("coral1", "#850E5D"))
#' brighten("darkgreen")
#'
#' show_col(c("hotpink", darken("hotpink"), brighten("hotpink")),
#'          c("lightskyblue", darken("lightskyblue"), brighten("lightskyblue")))
#' show_col(c(brighten("salmon3", 2),
#'            brighten("salmon3"),
#'            "salmon3",
#'            darken("salmon3"),
#'            darken("salmon3", 2)))
#'
#' # darken and brighten are opposite operations
#' darken("red", -1)
#' brighten("red", 1)
#'
#' # but not necessarily exactly reversible
#' col <- "#5eff15"
#' (new_col <- brighten(darken(col)))
#' show_col(c(col, new_col))
#' 
#' @name change_lightness
#' @export
darken <- function(x, amount=1) {
  # convert everything to a common color representation
  x <- chroma_r(x)
  
  # manipulate colors
  cmds <- paste0("chroma('", x, "').darken(",amount,").hex()")
  v8_eval(cmds)
}

#' @name change_lightness
#' @export
darker <- darken

#' @name change_lightness
#' @export
brighten <- function(x, amount=1) {
  darken(x, amount=-amount)
  # NB: this is how brighten is actually implemented in chroma.js
}

#' @name change_lightness
#' @export
brighter <- brighten

#' @name change_lightness
#' @export
lighten <- brighten

#' @name change_lightness
#' @export
lighter <- brighten
