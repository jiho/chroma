#
# Read ScientificColorMaps
#
# http://www.fabiocrameri.ch/colourmaps.php

files <- list.files("data-raw/ScientificColourMaps/", full.names=TRUE)
scicol_names <- stringr::str_replace(basename(files), ".txt", "")

# read color scales
scicol <- lapply(files, function(f) {
  x <- read.table(f)
  tolower(rgb(x))
})

names(scicol) <- scicol_names

# register the type of color scale
scicol_info <- data.frame(name=scicol_names, type="sequential")
scicol_info$type[scicol_info$name %in% c("broc", "cork", "vik", "lisbon", "tofino", "berlin")] <- "diverging"
scicol_info$type[scicol_info$name %in% c("batlow", "roma", "oleron")] <- "special"

# reorder
scicol_info <- dplyr::arrange(scicol_info, type, name)
scicol <- scicol[scicol_info$name]

use_data(scicol, scicol_info, internal=FALSE, overwrite=TRUE)
