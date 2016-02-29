#' Get the hue of colors
#'
#' @param h hue, either angles around the color wheel, in \code{[0, 360]} (angles outside of the range are rotated back to within \code{[0, 360]}: 380 = 20, -15 = 345, etc.), or colors (hex or named) from which the hue is extracted.
#' @param model string defining the color model; valid models are the ones containing a hue component: \code{hsv}, \code{hsl}, \code{hsi}, \code{hcl}, \code{lch}.
#'
#' @return A vector of hue angles, in \code{[0, 360]}.
#'
#' @seealso \code{\link{channel}} used to extract the hue channel.
#'
#' @examples
#' # h can be numbers, hex colors, named colors, or a mix of the above
#' hue(c(0, 10, 200))
#' hue(c("#F55D5B", "#16b2b4", "#6A9F16"))
#' hue(c("red", "green", "blue"))
#' hue(c(10, "#B55FFC", "pink", 365))
#'
#' # It should be possible to reconstruct colors similar to the original one
#  # based on the extracted hue by providing the other components
#' cols <- c("firebrick", "gold", "limegreen", "dodgerblue", "navy", "mediumpurple")
#' show_col(cols,
#'          hsv(h=hue(cols, model="hsv"), s=0.9, v=1),
#'          hsl(h=hue(cols, model="hsl"), s=1, l=0.6),
#'          hcl(h=hue(cols, model="hcl"), c=1, l=0.8))
#' # = works, but is a bit less reliably with hcl().
#'
#' @export
hue <- function(h, model="hsv") {
  if (is.numeric(h)) {
    out <- h %% 360
  } else {
    # deal with partially "numeric" vectors
    suppressWarnings(numh <- as.numeric(h))
    is_num <- which(!is.na(numh))
    is_not_num <- which(is.na(numh))
    
    # extract the character-based specifications
    charh <- channel(h[is_not_num], model=model, "h")
    
    # join the two
    out <- c()
    out[is_num] <- hue(numh[is_num])  # NB: apply the %% 360 here too
    out[is_not_num] <- charh
  }
  return(out)
}
