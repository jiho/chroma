#' CMYK Color Specification
#'
#' Create a vector of colors from cyan, magenta, yellow, and black
#'
#' @param c,m,y,k color components, numeric vectors with values in \code{[0, 1]}.
#'
#' @template color_spec
#'
#' @examples
#' cmyk()
#' cmyk(0, 0, 0, 0.1)
#' cmyk(data.frame(c(1, 0.5), c(1, 1), c(1, 1), c(0.5, 0.5)))
#' cmyk(matrix(c(1, 0.5, 1, 1, 1, 1, 0.5, 0.5), ncol=4))
#' cmyk(0.5, 1, 1, c(0.25, 0.5, 0.75))
#'
#' # color ramp
#' show_col(cmyk(k=seq(0, 1, length.out=10)))
#'
#' @export
cmyk <- function(c=0, m=0.6, y=0.6, k=0.4) {
  if ( is.matrix(c) | is.data.frame(c) ) {
    # if first argument is a table, use this only
    d <- c[,1:4]
  } else {
    # else, convert the arguments into a table
    # NB: this does argument recycling on its own; we just provide a nicer error message
    tryCatch(d <- data.frame(c, m, y, k),
      error=function(e) {
        stop("The lengths of arguments c, m, y, and k are not compatible.", call.=F)
      }
    )
  }
  
  # check that argument values are correct
  if (any(d[,1] < 0)) {
    stop("c cannot be < 0")
  }
  if (any(d[,1] > 1)) {
    stop("c cannot be > 1")
  }
  if (any(d[,2] < 0)) {
    stop("m cannot be < 0")
  }
  if (any(d[,2] > 1)) {
    stop("m cannot be > 1")
  }
  if (any(d[,3] < 0)) {
    stop("y cannot be < 0")
  }
  if (any(d[,3] > 1)) {
    stop("y cannot be > 1")
  }
  if (any(d[,4] < 0)) {
    stop("k cannot be < 0")
  }
  if (any(d[,4] > 1)) {
    stop("k cannot be > 1")
  }
  
  # convert each row of input
  apply(d, 1, function(x) {
    chroma.cmyk(x[1], x[2], x[3], x[4])
  })
}


chroma.cmyk <- function(c, m, y, k) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.cmyk(", c, ",", m, ",", y, ",", k, ").hex()")
  ct$eval(cmd)
}
