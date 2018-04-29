#' Get the hue of colors
#'
#' Get the hues (angles around the color wheel, in [0,360]) of a vector of colors.
#'
#' @param x vector of colors (specified as hex strings or named R colors), or numerical hues, or a mix thereof.
#' @param model string defining the color model; valid models are the ones containing a hue component: \code{hsv}, \code{hsl}, \code{hsi}, \code{hcl}, \code{lch}.
#' @param modulo logical, whether to restrict the hues in [0,360].
#'
#' @return A vector of hue angles, in [0,360] if modulo is \code{TRUE}.
#'
#' @seealso \code{\link{channel}} used internally, to extract the hue channel.
#'
#' @export
#'
#' @examples
#' # various input formats for x
#' hue(c("#F55D5B", "#16b2b4", "#6A9F16"))
#' hue(c("red", "green", "blue"))
#' hue(c(0, 10, 365))
#' hue(c(10, "#B55FFC", "pink", NA, 365))
#'
#' # It is possible to reconstruct colors similar to the original ones
#' # based on the extracted hue, by providing the other components
#' cols <- c("firebrick", "gold", "limegreen", "dodgerblue",
#'           "navy", "mediumpurple")
#' show_col(cols,
#'   hsv(h=hue(cols, model="hsv"), s=0.9, v=1),
#'   hsl(h=hue(cols, model="hsl"), s=1, l=0.6),
#'   hcl(h=hue(cols, model="hcl"), c=1, l=0.8)
#' )
#' # = works, but is a bit less predictable with hcl().
hue <- function(x, model="hsv", modulo=TRUE) {
  # deal with the special case of all numeric/empty vectors first, for performance purposes
  if (is.numeric(x) | all(is.na(x))) {
    out <- x
  } else {
    # check inputs
    model <- match.arg(model, c("hsv", "hsl", "hsi", "hcl", "lch"))

    # detect "numeric" types within the input vector
    suppressWarnings(numx <- as.numeric(x))
    is_num <- which(!is.na(numx))
    is_not_num <- which(is.na(numx))

    # extract the already numerical hues
    num_hues <- numx[is_num]

    # extract the hue channel for the other, character-based, specifications
    char_hues <- channel(x[is_not_num], model=model, "h")

    # combine the two
    out <- c()
    out[is_num]     <- num_hues
    out[is_not_num] <- char_hues
  }

  if (modulo) {
    out <- out %% 360
  }

  return(out)
}
