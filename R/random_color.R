#' Generate random color(s)
#'
#' @return A vector of colors specified as hex codes
#'
#' @export
#'
#' @examples
#' random_color()
#' random_color(n=10)
#' show_col(random_color(n=10), random_color(n=10), random_color(n=10))
#' # art!
random_color <- function(n=1) {
  cmds <- rep("chroma.random()", times=n)
  v8_eval(cmds)
}
