% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/channel.R
\name{channel}
\alias{channel}
\alias{channel<-}
\title{Get or set color channel}
\usage{
channel(x, model, channel)

channel(x, model, channel) <- value
}
\arguments{
\item{x}{vector of colors specified as hex strings or named R colors.}

\item{model}{string defining the color model; valid models are \code{rgb}, \code{hsv}, \code{hsl}, \code{hcl}, \code{lch}, \code{lab}, \code{cmyk}}

\item{channel}{string defining the channel within the color model}
}
\description{
Get or set color channel
}
\examples{
channel("pink", "rgb", "r")
col <- rainbow(5)
show_col(col)
channel(col, "hcl", "l") <- 0.3
show_col(col)
channel(colors()[1:5], "hcl", "l")

}
