#
# Parse a visible light lookup table
#

library("stringr")

# read file and keep intersting lines
x <- readLines("data-raw/Linear_visible_spectrum.svg")
x <- str_subset(x, "<stop")

# extract components of the gradient
x <- str_split_fixed(x, "[\"\\:]", 8)
wl <- x[,2]
wl <- str_replace(wl, "stop", "")
wl <- str_replace(wl, "nm", "")
wl <- as.numeric(wl)
color <- x[,7]
color <- str_replace(color, ";", "")

# add black on either side
wl <- c(370, wl, 760)
color <- c("#000000", color, "#000000")

# compute hues for

visible <- data.frame(wl, color)
use_data(visible, internal=FALSE, overwrite=TRUE)
