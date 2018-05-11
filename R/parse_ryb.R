#' RYB Color Specification
#'
#' Create a vector of colors from red, yellow, and blue.
#'
#' @param red,yellow,blue numbers in [0,1]: the color components. \code{red} can also be a matrix or data.frame containing the other components (\code{yellow} and \code{blue}.
#'
#' @template color_spec_from_matrix
#' @details Red, yellow, and blue are the primary colors in historical color theory. This is often what is taught in art schools, for paint. Nowadays, for print work, cyan, magenta, and yellow are considered to be the best set of three colorants to combine, for the widest range of high-chroma colors.
#'
#' Technically, the RYB components are interpolated to RGB channels using trilinear interpolation on a cube defined by the R,Y, and B primaries, following Gosset and Chen (2004) with a slight modification so that \code{ryb(1,1,1)} returns black. The reverse conversion, from a color to its RYB components, uses the same approach but on a cube defined by the R,G, and B "primary" channels. This is very approximate however, because many colors in the RGB cube cannot be represented in RYB; indeed many colors displayed on screens have no equivalent on paper (all the more flashy ones in particular).
#'
#' @template return_hex_colors
#'
#' @references
#' Gossett, N. and Chen, B. (2004). Paint inspired color mixing and compositing for visualization. In IEEE Symposium on Information Visualization, 2004. (pp. 113-118).
#'
#' Itten, J. (1961). Kunst der Farbe. Reinhold Pub. Corp.
#'
#' @family color specifications
#'
#' @export
#'
#' @examples
#' # Primaries
#' show_col(ryb(1,0,0), ryb(0,1,0), ryb(0,0,1))
#' # Mixing primaries gives secondaries
#' show_col(
#'   # primary 1   primary 2   secondary
#'   c(ryb(1,0,0), ryb(0,1,0), ryb(1,1,0)),
#'   c(ryb(0,0,1), ryb(1,0,0), ryb(1,0,1)),
#'   c(ryb(0,1,0), ryb(0,0,1), ryb(0,1,1))
#' )
#'
#' # Some RGB colors have RYB equivalents but some do not
#' x <- c(rgb(1,0,0), rgb(1,1,0), rgb(0,0,1), rgb(1,0,1), rgb(0,1,1))
#' show_col(
#'   x,
#'   ryb(as.ryb(x))
#' )
ryb <- function(red=0, yellow=0, blue=0) {
  # handle color channels
  x <- tabularise_arguments(red, yellow, blue)

  # parse colors using internal implementation
  colors <- parse_color(x, "ryb")

  return(colors)
}


# Cube defined by the three primaries: R, Y, and B
# Each corner defines a color and its approximate R,G,B components are specified following Itten (1961)
ryb_cube <- list(
  # check order of array(1:8, dim=c(2,2,2))
  # RYB         000 100    010    110    001    101   011    111
  #           white red yellow orange   blue purple green  black
  r = array(c(    1,  1,     1,     1, 0.163,   0.5,    0,     0), dim=c(2,2,2)),
  g = array(c(    1,  0,     1,   0.5, 0.373,     0, 0.66,     0), dim=c(2,2,2)),
  b = array(c(    1,  0,     0,     0,   0.6,   0.5,  0.2,     0), dim=c(2,2,2))
  # original values from Gosset and Chen: 111 is not pure black but some dark brown
  # r = array(c(    1,  1,     1,     1, 0.163,   0.5,    0,   0.2), dim=c(2,2,2)),
  # g = array(c(    1,  0,     1,   0.5, 0.373,     0, 0.66, 0.094), dim=c(2,2,2)),
  # b = array(c(    1,  0,     0,     0,   0.6,   0.5,  0.2,     0), dim=c(2,2,2))
)

# Cube defined by the three pure channels: R, G, and B
# We need to find the RYB values of the corners of the cube
# see https://math.stackexchange.com/questions/305395/ryb-and-rgb-color-space-conversion for the general thinking

# # define the RGB corners
# rgb_grid <- expand.grid(0:1, 0:1, 0:1)
# rgb_corners <- rgb(rgb_grid)
#
# # look for matching RYB values
# # 1/ via grid search
# x <- seq(0,1,l=30)
# ryb_grid <- expand.grid(x,x,x)
# ryb_colors <- ryb(ryb_grid)
#
# g <- lapply(rgb_corners, function(x) {
#   # compute differences
#   diff <- CMClc(x, ryb_colors, c=2)
#   # diff <- deltaE(x, ryb_colors)
#   # NB: CMClc allows to bias towards chroma, which is what we perceive the most as "color"
#   #     the results make a bit more sense than with Delta-E
#
#   # find the 5 smallest
#   index <- match(1:5, rank(diff))
#   # return them
#   list(
#     par = ryb_grid[index,],
#     value = diff[index],
#     col = ryb_colors[index]
#   )
# })
#
# # 2/ via optimization
# coldiff <- function(ryb, x_rgb) {
#   # force bounds for color components
#   if (any(ryb > 1) | any(ryb < 0)) {
#     diff <- NA
#   } else {
#     # compute color difference
#     x_ryb <- ryb(ryb[1], ryb[2], ryb[3])
#     diff <- CMClc(x_rgb, x_ryb, c=2)
#     # diff <- deltaE(x_rgb, x_ryb)
#   }
#   diff
# }
# o <- lapply(rgb_corners, function(col) {
#   # find smallest difference
#   o <- optim(c(0.5, 0.5, 0.5), coldiff, x_rgb=col)
#   # compute corresponding colour
#   o$col <- ryb(o$par[1], o$par[2], o$par[3])
#   return(o)
# })
#
# # show all results
# show_col(
#   unlist(
#     lapply(1:7, function(i) {
#       list(
#         rgb_corners[i],
#         c(g[[i]]$col, "white", o[[i]]$col))
#     }),
#     rec=F
#   )
# )
#
# # then manually inspect some matches to find the best possibility
# i=3; g[[i]]; o[[i]]

# after some fidling, this seems like the least horrible solution
rgb_cube <- list(
  # RGB         000 100    010    110   001     101       011   111
  #           black red  green yellow  blue magenta turquoise white
  r = array(c(    1,  1,     0,     0,    0,  0.309,        0,    0), dim=c(2,2,2)),
  y = array(c(    1,  0,     1,     1,    0,      0,    0.053,    0), dim=c(2,2,2)),
  b = array(c(    1,  0, 0.483,     0,    1,  0.469,    0.210,    0), dim=c(2,2,2))
)

# # compare final result
# show_col(
#   rgb_corners,
#   ryb(as.numeric(rgb_cube$r), as.numeric(rgb_cube$y), as.numeric(rgb_cube$b))
# )

# Internal implementation of RYB parsing
# @param x matrix with r,y,b columns in [0,1]
# @noRd
parse_ryb <- function(x) {
  # perform tri-linear interpolation in the RYB to RGB cube
  xi <- list(0:1, 0:1, 0:1)
  r <- interpn(xi, ryb_cube$r, x)
  g <- interpn(xi, ryb_cube$g, x)
  b <- interpn(xi, ryb_cube$b, x)

  return(grDevices::rgb(r, g, b))
}

# Internal implementation of the conversion into TYB
# @param x vector of input colors specified as hex codes.
# @noRd
convert_ryb <- function(x) {
  # convert into RGB
  x <- as.rgb(x)

  # perform tri-linear interpolation in the RGB to RYB cube
  xi <- list(0:1, 0:1, 0:1)
  r <- interpn(xi, rgb_cube$r, x)
  y <- interpn(xi, rgb_cube$y, x)
  b <- interpn(xi, rgb_cube$b, x)

  res <- cbind(r,y,b)

  return(res)
}
