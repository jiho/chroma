#' @details
#' In HCL space, the perceived color (hue) is completely separated from the perceived intensity (chromacity) and lightness of the color. This means that colors of various hues but same chromacity and lightness appear as the exact same grey when converted to greyscale. This makes the HCL space particularly suitable to create good color palettes:
#' \itemize{
#'   \item For qualitative palettes (discrete variables): varying \code{h} at constant \code{c} and \code{l} avoids drawing attention to certain hues, as would happen if the same was done in HSV or HSL space. Indeed, some hues are perceived as brighter (yellow, light green, etc.), others as duller/darker (blues, etc.).
#'   \item For sequential palettes (continuous variables): varying \code{l} (or possibly \code{c}) for a constant \code{h} gives a sense of direction and avoid the many perceptual pitfalls typical of 'rainbow'-like scales.
#' }
