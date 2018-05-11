#
# Get list of CSS named colors
#

library("tidyverse")
library("XML")

# library("httr")
# library("xml2")
#
# x <- content(GET("https://www.w3schools.com/colors/colors_names.asp"), as="text")
# xml_find_first(x, "//table")


x <- readLines("https://www.w3.org/TR/css-color-4/#named-colors") %>% str_c(collapse=" ")

d <- readHTMLTable(x, header=T, which=2)
d <- d[,3:4]
names(d) <- c("name", "hex")
d$name <- str_to_lower(d$name)
d$hex <- str_to_upper(d$hex)

css_colors <- d
use_data(css_colors, internal=FALSE, overwrite=TRUE)
