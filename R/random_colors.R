#' Generate random color(s)
#'
#' @param n number of colors to generate
#'
#' @template return_hex_colors
#'
#' @examples
#' random_colors()
#' random_colors(n=10)
#' show_col(random_colors(n=10), random_colors(n=10), random_colors(n=10))
#' # Art!
#'
#' @export
random_colors <- function(n=1) {
  cmds <- rep("chroma.random()", times=n)
  v8_eval(cmds)
}

#' @rdname random_colors
#' @export
rcolors <- random_colors