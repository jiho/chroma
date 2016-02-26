#
# Read color palettes from colorbrewer2.org
#

library("plyr")
library("dplyr")
library("jsonlite")
library("stringr")
library("devtools")

# read RGB values from .json
x <- fromJSON("http://colorbrewer2.org/export/colorbrewer.json")
d <- ldply(x, function(x) {

  d <- ldply(x[-length(x)], function(y) {
    
    # get rgb components
    d <- str_split_fixed(y, pattern="[\\(\\),]", 5)
    d <- d[,c(2:4)]
    class(d) <- "numeric"

    # convert into a color
    color <- rgb(d, max=255)
    
    # add length of palette
    d <- data.frame(n=length(y), color)
    return(d)
  })
  d <- select(d, -.id)
  d$type <- x$type
  
  return(d)
})
d <- select(d, name=.id, type, n, color)

# spell out types
d$type <- str_replace(d$type, "div", "diverging")
d$type <- str_replace(d$type, "seq", "sequential")
d$type <- str_replace(d$type, "qual", "qualitative")

# summarise information about palettes
brewer_info <- as.data.frame(summarise(group_by(d, type, name), maxcolors=max(n)))
row.names(brewer_info) <- brewer_info$name

# save the colors for internal use by the package only
brewer <- d
use_data(brewer, brewer_info, internal=FALSE, overwrite=TRUE)

# # read RGB and CMYK from palette files
# # devtools::install_github("hrbrmstr/swatches")
# library("swatches")
#
# # get all possible palette names
# info <- brewer.pal.info
# info$name <- row.names(info)
#
# # read the color values from the .gpl palettes
# # TODO consider using https://github.com/hrbrmstr/swatches to read palettes in CMYK format
# d <- dlply(info, ~name, function(x) {
#   d <- llply(3:x$maxcolors, function(i) {
#     # read data
#     file <- paste0("http://colorbrewer2.org/export/ase/", x$name, "_", i, ".ase")
#     tryCatch(d <- read.table(file, skip=4), error=function(e) {message(file)})
#     # add labels
#     # d$category <- x$category
#     # d$n <- i
#
#     # read colors from .ase palette
#     file <- paste0("http://colorbrewer2.org/export/ase/", x$name, "_", i, ".ase")
#     tryCatch(d <- read_ase(file, use_names=T), error=function(e) {message(file)})
#
#     return(d)
#   })
#   return(d)
# }, .progress="none")
