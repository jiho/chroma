#' Compute the contrast between two colors
#' 
#' @param x,y colors specified as hex strings or named R colors.
#' 
#' @return A number giving the contrast between x and y (or a vector thereof).
#' 
#' @export
#' 
#' @examples
#' contrast("pink", "hotpink")
#' contrast("pink", "blue")
#'
#' # can be vectorised
#' contrast("pink", rainbow(10))
contrast <- function(x, y) {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)
  
  # check arguments lengths
  foo <- tabularise_arguments(x, y)
  # NB: foo is not used, we put arguments in a table just to check that their lengths are compatible
  
  # mix colors
  cmds <- paste0("chroma.contrast('", x, "','", y, "')")
  as.numeric(v8_eval(cmds))
}
