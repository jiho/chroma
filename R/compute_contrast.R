#' Quantify contrast betwen colors
#'
#' Compute the WCAG contrast ratio between two colors.
#'
#' @param x,y colors specified as hex strings or named R colors.
#'
#' @return A number giving the contrast ratio between x and y (or a vector thereof). A value of 1 means no contrast at all. A minimum contrast of 4.5 is recommended to ensure that text is still readable against a background color.
#'
#' @references
#' \url{https://www.w3.org/TR/WCAG20-TECHS/G18.html}
#'
#' @export
#'
#' @examples
#' contrast("pink", "hotpink")
#' contrast("pink", "blue")
#'
#' # The computation can be vectorised (if the arguments are compatible)
#' contrast("pink", rainbow(10))
#' contrast(c("red","pink"), c("red","blue"))
contrast <- function(x, y) {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)

  # check arguments lengths
  foo <- tabularise_arguments(x, y)
  # NB: foo is not used, we put arguments in a table just to check that their lengths are compatible

  # mix colors
  cmds <- stringr::str_c("chroma.contrast('", x, "','", y, "')")
  as.numeric(v8_eval(cmds))
}
