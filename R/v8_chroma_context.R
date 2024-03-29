# Create a chroma-aware javascript interpreting context
#
# Use the \code{\link[V8]{v8}} function of package \code{V8} to create the context and evaluate the code of chroma.js, thus making all functions available.
#
# @seealso \code{\link[V8]{v8}} and \url{http://gka.github.io/chroma.js/}
#
# @examples
# ct <- v8_chroma_context()
# ct$eval("chroma('pink').darken().saturate(2).hex()")
#
# @export
#' @importFrom V8 v8
v8_chroma_context <- function() {
  # get chroma.js's location
  file <- system.file("chroma.min.js", package="chroma")

  # read the file and make it into a single character scalar
  chromajs <- scan(file, what="character", skip=57, sep="\n", quiet=T)
  chromajs <- paste(chromajs, collapse="")

  # create the context
  ct <- v8()

  # "load" chroma
  ct$eval(chromajs)

  # return the context
  return(ct)
}

# Evaluate commands in a V8 context
#
# Evaluate javascript commands in a V8 context. By default, this context is chroma-aware.
#
# @param command vector of strings to evaluate.
# @param context context to evaluate in.
#
# @examples
# v8_eval(c("chroma('pink').darken().hex()", "chroma.scale(['red','blue']).colors(5)", "chroma.mix('blue', 'red').hex()"))
#' @importFrom stats na.omit
v8_eval <- function(command, context=v8_chroma_context()) {
  # run valid (i.e. non NA) v8 commands
  # (and ensure the result is a vector)
  out <- unlist(lapply(na.omit(command), context$eval))
  # make upper case for consistency with other color-related functions in R
  out <- toupper(out)
  # re-insert NAs
  out <- na_insert(out, from=command)
  return(out)
}
