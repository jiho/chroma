# make a list for image(), persp(), etc.
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
maunga <- list(x=x, y=y, z=volcano)
# make a data.frame for ggplot
maungaxyz <- broom::tidy(list(x=x, y=y, z=volcano))
use_data(maunga, maungaxyz, internal=FALSE, overwrite=TRUE)
