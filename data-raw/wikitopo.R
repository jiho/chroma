#
# Define a color scale according to wikipedia's topographic maps conventions
#

# https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps
# NB: There is no absolute correspondance between colors and altitudes on wikipedia, except for the color of 0m altitude; i.e. the scale is relative for every map
#     It is not possible/easy to design such a scale in R so we just fix some common sense convention, based on the range in ETOPO1.

wikitopo <- read.csv(text="altitude,color
  8500,#F5F4F2
  8000,#E0DED8
  7500,#CAC3B8
  7000,#BAAE9A
  6500,#AC9A7C
  6000,#AA8753
  5500,#B9985A
  5000,#C3A76B
  4500,#CAB982
  4000,#D3CA9D
  3500,#DED6A3
  3000,#E8E1B6
  2500,#EFEBC0
  2000,#E1E4B5
  1500,#D1D7AB
  1000,#BDCC96
   500,#A8C68F
   250,#94BF8B
     0,#ACD0A5
 -0.01,#D8F2FE
  -250,#C6ECFF
  -500,#B9E3FF
 -1000,#ACDBFB
 -2000,#A1D2F7
 -3000,#96C9F0
 -4000,#8DC1EA
 -6000,#84B9E3
 -8000,#79B2DE
-11000,#71ABD8")
wikitopo <- wikitopo[order(wikitopo$altitude),]

row.names(wikitopo) <- NULL

use_data(wikitopo, internal=FALSE, overwrite=TRUE)
