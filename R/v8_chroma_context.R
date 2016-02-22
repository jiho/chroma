#' Create a chroma-aware javascript interpreting context
#'
#' Use the \code{\link[V8]{v8}} function of package \code{V8} to create the context and evaluate the code of chroma.js, thus making all functions available.
#'
#' @seealso \code{\link[V8]{v8}}
#'
#' @examples
#' ct <- v8_chroma_context()
#' ct$eval("chroma('pink').darken().saturate(2).hex()")
#'
#' @export
#' @importFrom V8 v8
v8_chroma_context <- function() {
  # get chroma.js's location
  file <- system.file("chroma.min.js", package="chroma")

  # read the file and make it into a single character scalar
  chromajs <- scan(file, what="character", skip=31, sep="\n", quiet=T)
  chromajs <- paste(chromajs, collapse="")

  # create the context
  ct <- v8()

  # "load" chroma
  ct$eval(chromajs)

  # return the context
  return(ct)
}
