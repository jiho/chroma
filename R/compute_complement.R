#' Compute the complement color
#'
#' @template param_x_rcolors
#' @param mode color mode to use, either "ryb" which yields the natural, paint-like complement, or "rgb"
#'
#' @template return_hex_colors
#'
#' @export
#' @examples
#' # paint-like complements
#' a <- c("red", "yellow", "blue")
#' b <- c("green", "purple", "orange")
#' w <- c("white", "white", "white")
#' show_col(a, complement(a), b, w, b, complement(b), a)
#'
#' # scren-like complements
#' show_col(a, complement(a, mode="ryb"))
#' show_col(a, complement(a, mode="rgb"))
#' x <- c("red", "green", "blue", "cyan", "magenta", "yellow")
#' show_col(x, complement(x, mode="rgb"))
complement <- function(x, mode="ryb") {
  mode <- match.arg(mode, c("ryb", "rgb"))
  xh <- as.hsv(x)
  if (mode == "ryb") {xh[,1] <- shift_hue(xh[,1], direction="hsv2ryb")}
  xh[,1] <- (xh[,1] + 180) %% 360
  if (mode == "ryb") {xh[,1] <- shift_hue(xh[,1], direction="ryb2hsv")}
  return(hsv(xh))
}

triad <- function(x) {
  xh <- as.hsv(x)
  xh[,1] <- shift_hue(xh[,1], direction="hsv2ryb")
  xh[,1] <- (xh[,1] + 120) %% 360
  xh[,1] <- (xh[,1] + 120*2) %% 360
  xh[,1] <- shift_hue(xh[,1], direction="ryb2hsv")
  return(hsv(xh))
}


# Piecewise linear interpolation to convert
# h <- 0:360
# plot(h, shift_hue(h), xlab="HSV h", ylab="RYB H")
# show_col(hsv(h, 1, 1), hsv(shift_hue(h, "ryb2hsv"), 1, 1))
shift_hue <- function(h, direction=c("hsv2ryb", "ryb2hsv")) {
  direction <- match.arg(direction)

  # # from https://www.sessions.edu/color-calculator/
  # breaksHSV <- c(0, 60,120,240)
  # breaksRYB <- c(0,120,180,240)
  # # -> not very nice at the end of the range

  # # from https://computergraphics.stackexchange.com/questions/1748/function-to-convert-hsv-angle-to-ryb-angle
  # # used by Adobe Kuler
  # breaksHSV <- c(0, 35, 60,120,180,240,300)
  # breaksRYB <- c(0, 60,122,165,218,275,330)

  # from https://www.mathworks.com/matlabcentral/mlc-downloads/downloads/submissions/61775/versions/22/previews/ColorGUI_v2_1/tools/Plots/generateRybWheelColors.m/index.html?access_key=
  breaksHSV <- c(0, 18, 30, 45, 59, 84,144,184,219,252,285,322)
  breaksRYB <- c(0, 30, 60, 90,120,150,180,210,240,270,300,330)

  if (direction == "hsv2ryb") {
    x <- breaksHSV
    y <- breaksRYB
  } else {
    y <- breaksHSV
    x <- breaksRYB
  }
  # ho <- stats::approx(x=c(x-360, x, x+360), y=c(y-360, y, y+360), xout=h)$y
  ho <- stats::splinefun(x=c(x-360, x, x+360), y=c(y-360, y, y+360))(h)
  return(ho)
}
