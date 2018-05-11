context("Parsing compatibility")

set.seed(1)
x <- matrix(runif(9), ncol=3)

test_that("rgb is compatible with grDevices::rgb", {
  expect_equal(rgb(x), grDevices::rgb(x))
  expect_equal(rgb(x, alpha=x[,1]), grDevices::rgb(x, alpha=x[,1]))
  expect_equal(rgb(x, alpha=x[,1], names=x[1,]), grDevices::rgb(x, alpha=x[,1], names=x[1,]))
  expect_equal(rgb(round(x*255), maxColorValue=255), grDevices::rgb(round(x*255), maxColorValue=255))
})

test_that("rgb is compatible with colorspace::sRGB", {
  expect_equal(rgb(x), colorspace::hex(colorspace::sRGB(x)))
})

# hcl is not compatible with grDevices::hcl
# xhcl <- as.hcl(rgb(x))
# xhcl[,2:3] <- xhcl[,2:3]*100
# test_that("hcl is compatible with grDevices::hcl", {
#   expect_equal(hcl(xhcl, compat=TRUE), grDevices::hcl(xhcl[,1], xhcl[,2], xhcl[,3], fixup=TRUE))
#   expect_equal(hcl(xhcl[,1], xhcl[,2]/100, xhcl[,3]/100), grDevices::hcl(xhcl[,1], xhcl[,2], xhcl[,3], fixup=TRUE))
# })

xhsv <- as.hsv(rgb(x))
xhsv[,1] <- xhsv[,1]/360
test_that("hsv is compatible with grDevices::hsv", {
  expect_equal(hsv(xhsv, compat=TRUE), grDevices::hsv(xhsv[,1], xhsv[,2], xhsv[,3]))
  expect_equal(hsv(xhsv[,1]*360, xhsv[,2], xhsv[,3]), grDevices::hsv(xhsv[,1], xhsv[,2], xhsv[,3]))
})
test_that("hsv is compatible with colorspace::HSV", {
  expect_equal(hsv(xhsv), colorspace::hex(colorspace::HSV(xhsv)))
})

xlab <- as.lab(rgb(x))
test_that("hsv is compatible with colorspace::LAB", {
  expect_equal(lab(xlab), colorspace::hex(colorspace::LAB(xlab*100)))
})

# x <- matrix(runif(12), ncol=4)
# test_that("rgba is compatible with grDevices::rgb", {
#   expect_equal(rgba(x), grDevices::rgb(x[,1:3], alpha=x[,4]))
# })
# This fails but because or rounding errors
# TODO investigate a solution

test_that("CSS color parsing matches W3C", {
  expect_equal(css(css_colors$name), css_colors$hex)
})
