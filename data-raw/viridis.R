#
# Read viridis color palettes
#

library("stringr")
library("plyr")
library("dplyr")
library("devtools")

# scan("https://raw.githubusercontent.com/BIDS/colormap/master/colormaps.py", what="character", sep="\n")


options <- c("a", "b", "c", "d")

colors <- llply(options, function(i) {
  # read file
  x <- scan(str_c("https://raw.githubusercontent.com/BIDS/colormap/master/option_",i,".py"), what="char", sep="\n", quiet=TRUE, skip=7)
  # remove inappropriate lines
  x <- x[str_detect(x, "\\],$|\\]]$") & ! str_detect(x, ":")]
  # clean up the format to be homogenous and easily parsable
  x <- str_replace(x, fixed("cm_data = ["), "       ")
  x <- str_replace(x, fixed("]]"), "],")
  x <- str_replace_all(x, "[\\[\\]]", "")
  # parse colors
  d <- read.table(text=x, sep=",", strip.white=TRUE)
  colors <- rgb(d[,1:3])
  return(colors)
}, .progress="text", .inform=T)

magma   <- colors[[1]]
inferno <- colors[[2]]
plasma  <- colors[[3]]
viridis <- colors[[4]]

use_data(inferno, magma, plasma, viridis, internal=FALSE, overwrite=TRUE)
