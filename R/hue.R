#' Get the hue of colors
#'
#' Get the hue (angle around the color wheel, in [0,360]) of a vector of colors.
#'
#' @template param_x_rcolors
#' @param model string defining the color model; valid models are the ones containing a hue component: \code{hsv}, \code{hsl}, \code{hsi}, \code{hcl}, \code{lch}.
#'
#' @return A vector of hue angles, in [0,360].
#'
#' @seealso \code{\link{channel}} used internally, to extract the hue channel.
#'
#' @export
#'
#' @examples
#' # h can be numbers, hex colors, named colors, or a mix of the above
#' hue(c("#F55D5B", "#16b2b4", "#6A9F16"))
#' hue(c("red", "green", "blue"))
#' hue(c(0, 10, 365))
#' hue(c(10, "#B55FFC", "pink", 365))
#'
#' # It should be possible to reconstruct colors similar to the original ones
#' # based on the extracted hue, by providing the other components
#' cols <- c("firebrick", "gold", "limegreen", "dodgerblue",
#'           "navy", "mediumpurple")
#' show_col(cols,
#'   hsv(h=hue(cols, model="hsv"), s=0.9, v=1),
#'   hsl(h=hue(cols, model="hsl"), s=1, l=0.6),
#'   hcl(h=hue(cols, model="hcl"), c=1, l=0.8)
#' )
#' # = works, but is a bit less reliable with hcl().
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
