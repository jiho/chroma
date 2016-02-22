#' Parse colors specified in a given model
#'
#' @param x a matrix or data.frame whose columns specify the color channels.
#' @template model
#'
#' @family color specifications
#'
#' @return A vector of colors specified as hex codes
#'
#' @examples
#' parse_color(data.frame(h=c(0, 120, 240), s=0.5, v=0.7), "hsv")
#' parse_color(data.frame(r=c(255, 0, 0),
#'                        g=c(0, 255, 0),
#'                        b=c(0, 0, 255)), "rgb")
#'
#' @export
parse_color <- function(x, model) {
  # create command string
  cmds <- apply(x, 1, function(xx) {
    paste0("chroma.", model, "([", paste0(xx, collapse=","), "]).hex()")
  })
  
  # convert colors
  res <- v8_eval(cmds)
  
  return(res)
}