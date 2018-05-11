context("Parsing replicability")

# check that parsing gives the same colors as in the past
# NB: reference set on 2018-04-29

set.seed(1)
x <- matrix(runif(3*4), ncol=4)

test_that("cmyk is replicable", {
  expect_equal(cmyk(x), c("#B0160D", "#7FA245", "#5A154E"))
})

test_that("css is replicable", {
  expect_equal(css(c("FireBrick", "ForestGreen", "DodgerBlue")), c("#B22222", "#228B22", "#1E90FF"))
})

test_that("hcl is replicable", {
  expect_equal(hcl(x[,1]*360, x[,2], x[,3]), c("#FFF01B", "#91A786", "#00BADD"))
})
test_that("lch is replicable", {
  expect_equal(lch(x[,1], x[,2], x[,3]*360), c("#9B0070", "#295E73", "#00A8FB"))
})

test_that("hex is replicable", {
  expect_equal(hex(c("#76B768", "#005CCF", "#FF67FF")), c("#76B768", "#005CCF", "#FF67FF"))
})

test_that("hsi is replicable", {
  expect_equal(hsi(x[,1]*360, x[,2], x[,3]), c("#D7FF16", "#87D89B", "#10ABFF"))
})

test_that("hsl is replicable", {
  expect_equal(hsl(x[,1]*360, x[,2], x[,3]), c("#EFFEE4", "#97BA9F", "#4BABF5"))
})

test_that("hsv is replicable", {
  expect_equal(hsv(x[,1]*360, x[,2], x[,3]), c("#6FF116", "#87A98E", "#1061A0"))
})

test_that("lab is replicable", {
  expect_equal(lab(x[,1:3]), c("#B30000", "#854900", "#FF001F"))
})

test_that("rgb is replicable", {
  expect_equal(rgb(x[,1:3]), c("#44E8F1", "#5F33A9", "#92E5A0"))
})
test_that("rgba is replicable", {
  expect_equal(rgba(x), c("#44E8F110", "#5F33A935", "#92E5A02D"))
})

test_that("ryb is replicable", {
  expect_equal(ryb(x[,1:3]), c("#147C2E", "#7D677B", "#646C19"))
})


test_that("temperature is replicable", {
  expect_equal(temperature(x[,1]*10000+2000), c("#FFDEC0", "#FFEFE6", "#E6EBFF"))
})

test_that("wavelength is replicable", {
  expect_equal(wavelength(x[,1]*300+380), c("#0A366D", "#15726F", "#7EEA22"))
})
