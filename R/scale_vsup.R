#' Value suppressing color scale and palette
#'
#' Value suppressing color scale, whereby uncertain values are plotted with the
#' same colour, to prevent the reader from trying to distinguish them.
#'
#' @inheritParams interp_scale
#' @param ... passed to \code{\link{vsup_scale}} from other \code{vsup_*} functions; passed to \code{ggplot2::\link[ggplot2]{continuous_scale}} or \code{ggplot2::\link[ggplot2]{discrete_scale}} from the \code{scale_*} functions, as appropriate. NB: in all situations, passing \code{domain} is meaningless and yields an error.
#'
#' @template return_scales
#'
#' @template seealso_hcl_scales
#' @family color scales and palettes
#'
#' @export
#'
#' @examples
#' # display a range of values with increasing uncertainty
#' show_col(
#'   vsup_scale()(1:6/6, 0),
#'   vsup_scale()(1:6/6, 0.5),
#'   vsup_scale()(1:6/6, 1)
#' )
#'
#' # starting from colours with different hues but similar chroma and luminance
#' # gives more continuous looking results; for example:
#' cols <- hue_colors(2, c=0.8, l=0.5)
#' show_col(cols)
#' # display a grid of colours corresponding to various values and uncertainties
#' g <- expand.grid(x=1:10, u=1:10)
#' plot(x ~ u, data=g, col=vsup_map(g$x, g$u, cols), pch=19, asp=1, cex=6)
#' # increase the number of steps (i.e. layers) in the uncertainty mapping,
#' # to get a smoother visual aspect
#' plot(x ~ u, data=g, col=vsup_map(g$x, g$u, colors=cols, layers=10, mode="hcl"), pch=19, asp=1, cex=6)
#' # and vary to modes to check their effect
#' plot(x ~ u, data=g, col=vsup_map(g$x, g$u, colors=cols, layers=10, mode="hc" ), pch=19, asp=1, cex=6)
#' plot(x ~ u, data=g, col=vsup_map(g$x, g$u, colors=cols, layers=10, mode="hl" ), pch=19, asp=1, cex=6)
#'
#' vsup_palette(colors=cols)(10, u=0)
#' show_col(
#'   vsup_colors(10, 0   , layers=10, mode="hc"),
#'   vsup_colors(10, 0.25, layers=10, mode="hc"),
#'   vsup_colors(10, 0.5 , layers=10, mode="hc"),
#'   vsup_colors(10, 0.75, layers=10, mode="hc"),
#'   vsup_colors(10, 1   , layers=10, mode="hc")
#' )
vsup_scale <- function(colors=viridis_colors(6), branching=2, layers=4, mode="hcl", domain=c(0,1), uncertainty_domain=c(0,1), reverse=FALSE, na.value='red', extrapolate=FALSE) {
  # force input R colors into hex notation
  colors <- in_hex(na.omit(colors))
  # NB: remove NAs which don't mean anything for interpolation

  # check arguments
  mode <- match.arg(mode, c("hcl", "hl", "hc"))
  mode <- mode |> str_replace("h", "u") |> str_replace("c", "s")

  if (branching^layers > 10^6) {
    stop("The combination of branching and layers results in a number of colours that is larger than what the human eye can perceive; please lower the values of those arguments.")
  }

  # TODO pick a color as NA value, which is the same as max uncertainty

  if ( reverse ) {
    domain <- rev(domain)
  }

  js_vec <- function(x) {
    if (is.character(x)) {a <- "'"} else {a <- ""}
    str_c("[", a, str_c(x, collapse=str_c(a, ",", a)), a, "]")
  }
  colors_t <- js_vec(colors)
  v_domain_t <- js_vec(domain)
  u_domain_t <- js_vec(uncertainty_domain)

  # define the scale function which calls chroma.js internally
  f <- function(x, u) {
    # coerce everything to numbers
    x <- as.num(x)
    u <- as.num(u)
    if (length(x) != length(u)) {
      if (length(x) == 1) {
        x <- rep(x, length.out=length(u))
      } else if (length(u) == 1) {
        u <- rep(u, length.out=length(x))
      } else {
        stop("x and u must have the same length.")
      }
    }

    ct <- v8_all_context()
    ct$eval(str_c("var q = vsup.quantization().branching(", branching, ").layers(", layers, ").valueDomain(", v_domain_t, ").uncertaintyDomain(", u_domain_t, ")"))
    ct$eval(str_c("var s = vsup.scale().quantize(q).mode('", mode, "').range(chroma.scale(", colors_t, ").mode('lab'))"))
    ct$assign("v", matrix(x))
    ct$assign("u", matrix(u))
    ct$eval("
    var clrs=[];
    for (let i = 0; i < u.length; i++) {
      clrs[i]=s(v[i],u[i]);
    }")
    clrs <- ct$get("clrs")
    rm(ct)
    if (is.character(clrs)) {
      clrs <- css(clrs)
    } else {
      clrs <- hsl(clrs[1:3])
    }

    return(post_process_scale(clrs, na.value, extrapolate, x, range(domain)))
  }
}

#' @rdname vsup_scale
#' @export
vsup_map <- function(x, u, ...) {
  vsup_scale(domain=range(x, na.rm=TRUE, finite=TRUE), uncertainty_domain=range(u, na.rm=TRUE, finite=TRUE), ...)(x, u)
}

#' @rdname vsup_scale
#' @export
vsup_palette <- function(u, ...) {
  f <- function(n, u) {
    vsup_scale(domain=c(1,n), ...)(1:n, u=u)
  }
  return(f)
}

#' @rdname vsup_scale
#' @export
vsup_colors <- function(n, u, ...) { vsup_palette(...)(n, u) }

# # Pick a good missing value color for a hue scale
# # when not defined (NULL), pick a grey with the same luminance as the other colors in the scale
# hue_na <- function(na.value, l) {
#   if (is.null(na.value)) {
#     na.value <- hcl(h=0, c=0, l=l)
#   }
#   return(na.value)
# }
