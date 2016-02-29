#' Mix two colors
#'
#' @param x,y colors specified as hex strings or named R colors.
#' @param ratio amount of y mixed in x; \code{ratio} = 0 means pure \code{x}, \code{ratio} = 1 means pure \code{y}.
#' @param model color model to compute the mix in; valid models are \code{rgb}, \code{hsv}, \code{hsl}, \code{hcl}, \code{lab}.
#'
#' @details
#' All arguments can be vectors, as long as they are of compatible length (shorter arguments will be repeated to the length of longer arguments)
#'
#' @template return_hex_colors
#'
#' @family color manipulation functions
#'
#' @examples
#' mix("red", "blue")
#' mix("red", "blue", 0.25)
#' # Arguments can be vectors and all mixes are returned
#' mix("red", "blue", c(0.25, 0.5, 0.75))
#'
#' show_mix <- function(x, y, ...) {show_col(c(x, y, mix(x, y, ...)))}
#' show_mix("#FEF213", "#146EFD")
#' show_mix("#FEF213", "#FA000C")
#' show_mix("red", "blue", ratio=c(0.25, 0.5, 0.75))
#' show_mix("red", "blue", model=c("rgb", "hsv", "hsl", "hcl", "lab"))
#'
#' @export
mix <- function(x, y, ratio=0.5, model="rgb") {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)
  
  # check arguments values
  is_in(ratio, 0, 1, "ratio")
  model <- match.arg(model, c("rgb", "hsv", "hsl", "hcl", "lab"), several.ok = TRUE)
  
  # check arguments lengths
  foo <- tabularise_arguments(x, y, ratio, model)
  # NB: foo is not used, we put them in a table just to check that their lengths are compatible
  
  # mix colors
  cmds <- paste0("chroma.mix('", x, "','", y, "', ", ratio, ", '", model, "').hex()")
  v8_eval(cmds)
}
