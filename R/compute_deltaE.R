#' Compute the Delta-E between two colors
#'
#' Compute the difference between two colors, using the CIE Delta-E 2000 formula. \url{https://en.wikipedia.org/wiki/Color_difference#CIEDE2000}.
#'
#' @template param_xy_rcolors
#'
#' @return A number quantifying the difference between x and y (or a vector thereof). A difference < 1 is imperceptible to the human eye.
#'
#' @references Sharma, Gaurav; Wu, Wencheng; Dalal, Edul N. (2005). "The CIEDE2000 color-difference formula: Implementation notes, supplementary test data, and mathematical observations" (PDF). Color Research & Applications (Wiley Interscience) 30 (1): 21–30. doi:10.1002/col.20070
#'
#' @export
#' @importFrom stats na.omit
#'
#' @examples
#' deltaE("pink", "hotpink")
#' deltaE("pink", "blue")
#'
#' # The computation can be vectorised
#' # For example to find the closest color in an array of possibilities
#' clrs <- rainbow(20)
#' show_col("pink", clrs)
#' d <- deltaE("pink", clrs)
#' show_col("pink", clrs[which.min(d)])
deltaE <- function(x, y) {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)

  # check arguments lengths
  if (length(x) != 1) {
    stop("Need to provide a single reference color as x argument")
  }

  # convert to L*a*b* and scale to adapt to the formula
  x <- convert_color(x, "lab") * 100
  y <- convert_color(y, "lab") * 100
  X <- data.frame(x, y)

  # apply formula to all
  deltae <- apply(na.omit(X), 1, function(x) {
    deltaE_lab(x[1:3],x[4:6])
  })
  # reinsert missing values
  # NB: rowSums is NA as soon as one element is NA
  deltae <- na_insert(deltae, from=rowSums(X))
  names(deltae) <- NULL

  return(deltae)
}


in_deg <- function(x) { ((x * 180 / pi) + 360) %% 360 }
in_rad <- function(x) { x * pi / 180 }
cosd <- function(x) {cos(in_rad(x))}
sind <- function(x) {sin(in_rad(x))}

deltaE_lab <- function(x1, x2) {
  # assumes x1 and x2 are vectors of length 3,
  # containing L*, a*, and b* in order

  C1 <- sqrt(x1[2]^2 + x1[3]^2)
  C2 <- sqrt(x2[2]^2 + x2[3]^2)
  Cbar <- (C1 + C2)/2

  G <- 0.5*(1-sqrt(Cbar^7/(Cbar^7+25^7)))

  a1prime <- (1+G)*x1[2]
  a2prime <- (1+G)*x2[2]

  C1prime <- sqrt(a1prime^2 + x1[3]^2)
  C2prime <- sqrt(a2prime^2 + x2[3]^2)

  h1prime <- in_deg(atan2(x1[3], a1prime))
  h2prime <- in_deg(atan2(x2[3], a2prime))

  deltaLprime <- x2[1] - x1[1]
  deltaCprime <- C2prime - C1prime

  diffhprime <- h2prime - h1prime
  if (C1prime*C2prime == 0) {
    deltahprime <- 0
  } else {
    if (diffhprime > 180) {
      deltahprime <- diffhprime - 360
    } else if (diffhprime < -180) {
      deltahprime <- diffhprime + 360
    } else {
      deltahprime <- diffhprime
    }
  }

  deltaHprime <- 2 * sqrt(C1prime*C2prime)*sind(deltahprime/2)

  Lbarprime <- (x1[1] + x2[1])/2
  Cbarprime <- (C1prime + C2prime)/2

  sumhprime  <- h1prime + h2prime
  diffhprime <- h1prime - h2prime
  if (C1prime*C2prime == 0) {
    hbarprime <- sumhprime
  } else {
    if (abs(diffhprime) <= 180) {
      hbarprime <- sumhprime / 2
    } else if (abs(diffhprime) > 180 & sumhprime < 360) {
      hbarprime <- (sumhprime + 360) / 2
    } else if (abs(diffhprime) > 180 & sumhprime >= 360) {
      hbarprime <- (sumhprime - 360) / 2
    } else {
      stop("Should not be possible")
    }
  }

  T_T <- 1 - 0.17 * cosd(hbarprime - 30) + 0.24 * cosd(2 * hbarprime) + 0.32 * cosd(3 * hbarprime + 6) - 0.2 * cosd(4*hbarprime - 63)
  # NB: avoid T as notation for confusion with TRUE

  deltatheta <- 30 * exp(-((hbarprime -275)/25)^2)

  R_C <- 2 * sqrt(Cbarprime^7/(Cbarprime^7+25^7))

  S_L <- 1 + (0.015 * (Lbarprime - 50)^2) / (sqrt(20 + (Lbarprime - 50)^2))

  S_C <- 1 + 0.045 * Cbarprime

  S_H <- 1 + 0.015 * Cbarprime * T_T

  R_T <- - sind(2 * deltatheta) * R_C

  k_L <- k_C <- k_H <- 1
  deltae <- sqrt(
    (deltaLprime / (k_L * S_L))^2 +
    (deltaCprime / (k_C * S_C))^2 +
    (deltaHprime / (k_H * S_H))^2 +
    R_T * (deltaCprime / (k_C * S_C)) * (deltaHprime / (k_H * S_H))
  )

  return(deltae)
}
