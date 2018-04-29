#' Chromacity scale and palette
#'
#' Chromacity-based color scale, in HCL space.
#'
#' @param chroma chromacity, vector of two numbers in \code{[0,~1]} (0 is grey, ~1 is full color) giving the minimum and maximum chromacities along the scale.
#' @inheritParams interp_scale
#' @inheritParams hcl
#' @param ... passed to \code{\link{chroma_scale}} from other \code{chroma_*} functions; passed to \code{ggplot2::\link[ggplot2]{continuous_scale}} from the \code{scale_*} functions. NB: in all situations, passing \code{domain} is meaningless and yields an error.
#'
#' @template details_hcl
#'
#' @template return_scales
#'
#' @template seealso_hcl_scales
#' @family color scales and palettes
#'
#' @export
#'
#' @examples
#' # Define a scale towards a more intense red
#' reds <- chroma_scale(h=30)
#' # and apply it to some data
#' reds(x=c(0, 0.2, 0.6, 1))
#' show_col(reds(x=c(0, 0.2, 0.6, 1)))
#'
#' # Define a palette function
#' reds_pal <- chroma_palette(h=30)
#' # and get 10 colors from it
#' reds_pal(n=10)
#' show_col(reds_pal(n=10))
#' # or use the shortcut and get 50 colors
#' show_col(chroma_colors(n=50, h=30))
#'
#' # Determine hue from a color and then define a chroma scale
#' blues <- chroma_colors(n=50, h="dodgerblue")
#' greens <- chroma_colors(n=50, h="green")
#' golds <- chroma_colors(n=50, h="gold")
#' pinks <- chroma_colors(n=50, h="deeppink")
#' show_col(blues, greens, golds, pinks)
#'
#' # Chroma scales can be used for continuous variables
#' # such as the elevation of the Maunga Whau volcano
#' image(maunga, col=chroma_colors(100, h="orange"))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' filled.contour(maunga, color.palette=chroma_palette(h="orange"))
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=chroma_map(persp_facets(maunga$z), h="orange"))
#' # but a lightness-based scale would probably be even better
#' # (see ?light_scale)
#'
#' \dontrun{
#' # in spinning 3D
#' library("rgl")
#' persp3d(maunga, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=chroma_map(maunga$z, h="orange"))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' # and with ggplot2
#' library("ggplot2")
#' ggplot(maungaxyz) + coord_fixed() +
#'   geom_raster(aes(x=x, y=y, fill=z)) +
#'   geom_contour(aes(x=x, y=y, z=z), colour="white", alpha=0.5) +
#'   scale_fill_chroma(h="orange")
#' }
#'
#' # Or they could be used to map a third variable on a scatterplot
#' attach(airquality)
#' # define a scale encompassing the whole data
#' blue_scale <- chroma_scale(h="cornflowerblue", domain=c(0,200))
#' # use it on a plot and in the legend
#' pars <- sidemargin()
#' plot(Wind, Temp, col=blue_scale(Ozone), pch=19)
#' sidelegend(legend=c(pretty(Ozone), "NA"),
#'            col=blue_scale(c(pretty(Ozone), NA)), pch=19)
#' par(pars)
#' # note that the missing value color contrasts with the rest of the scale
#'
#' # They are not really appropriate for categorical variables though
#' attach(iris)
#' plot(Petal.Length, Petal.Width, col=chroma_map(Species), pch=19)
#' legend(1, 2, legend=levels(Species),
#'              col=chroma_colors(n=nlevels(Species)), pch=19)
#' # a hue-based scale would be much better (see ?hue_scale)
chroma_scale <- function(chroma=c(0,1), l=0.5, h=0, domain=c(0,1), reverse=FALSE, na.value=NULL) {
  # NB: argument is named `chroma` to avoid conflict with `c` (error: promise already under evaluation). But the `c` abbreviation works.

  # check arguments
  if (length(chroma) != 2) {
    stop("chroma needs to be a vector of length 2, defining the minimum and maximum chroma to use.")
  }

  # change the direction of the scale
  if (reverse) {
    chroma <- rev(chroma)
  }

  # define the function
  f <- function(x) {
    # define colors
    colors <- hcl(h=hue(h), c=scales::rescale(x, from=domain, to=chroma), l=l)

    # if the na.value is not defined, pick a good default
    na.value <- chroma_na(na.value, h=h, chroma=chroma, l=l)
    # replace NAs by na.value when necessary
    if (!is.na(na.value)) {
      na_colors <- is.na(colors)
      if (any(na_colors)) {
        colors[na_colors] <- na.value
      }
    }
    return(colors)
  }
  return(f)
}

#' @rdname chroma_scale
#' @export
chroma_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) { x <- factor(x) }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  chroma_scale(domain=range(x, na.rm=T), ...)(x)
}

#' @rdname chroma_scale
#' @export
chroma_palette <- function(...) {
  f <- function(n) {
    chroma_scale(domain=c(1,n), ...)(1:n)
  }
  return(f)
}

#' @rdname chroma_scale
#' @export
chroma_colors <- function(n, ...) {
  chroma_palette(...)(n)
}


# Pick a good missing value color for a chroma scale
# when not defined (NULL), pick a different hue in the middle of the chroma scale. Grey cannot be used here because it may be part of the scale.
chroma_na <- function(na.value, h, chroma, l) {
  if (is.null(na.value)) {
    na.value <- hcl(h=hue(h)+180, c=mean(chroma), l=l)
  }
  return(na.value)
}

## ggplot ----

#' @rdname chroma_scale
#' @export
scale_color_chroma <- function(..., chroma=c(0,1), l=0.5, h=0, reverse=FALSE, na.value=NULL, guide="colourbar") {
  ggplot2::continuous_scale("colour", "chroma",
    chroma_scale(chroma=chroma, l=l, h=h, reverse=reverse),
    na.value=chroma_na(na.value, h=h, chroma=chroma, l=l), guide=guide, ...
  )
}
#' @rdname chroma_scale
#' @export
#' @usage NULL
scale_colour_chroma <- scale_color_chroma

#' @rdname chroma_scale
#' @export
scale_fill_chroma <- function(..., chroma=c(0,1), l=0.5, h=0, reverse=FALSE, na.value=NULL, guide="colourbar") {
  ggplot2::continuous_scale("fill", "chroma",
    chroma_scale(chroma=chroma, l=l, h=h, reverse=reverse),
    na.value=chroma_na(na.value, h=h, chroma=chroma, l=l), guide=guide, ...
  )
}

# NB: discrete chroma scales do not make much sense so we do not define any.