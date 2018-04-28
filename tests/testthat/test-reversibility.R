context("Reversibility")

x <- tolower(rainbow(10))

test_that("rgb is reversible", {
  expect_that(rgb(as.rgb(x), max=255), equals(as.hex(x)))
})

test_that("rgba is reversible", {
  expect_that(rgba(as.rgba(x)), equals(x))
})

test_that("hsv is reversible", {
  expect_that(hsv(as.hsv(x)), equals(as.hex(x)))
})

test_that("hsl is reversible", {
  expect_that(hsl(as.hsl(x)), equals(as.hex(x)))
})

test_that("hsi is not reversible", {
  expect_that(hsi(as.hsi(x)), not(equals(as.hex(x))))
})

test_that("hcl is reversible", {
  expect_that(hcl(as.hcl(x)), equals(as.hex(x)))
})

test_that("lch is reversible", {
  expect_that(lch(as.lch(x)), equals(as.hex(x)))
})

test_that("lab is reversible", {
  expect_that(lab(as.lab(x)), equals(as.hex(x)))
})

test_that("cmyk is reversible", {
  expect_that(cmyk(as.cmyk(x)), equals(as.hex(x)))
})

test_that("css is reversible", {
  expect_that(css(as.css(x)), equals(as.hex(x)))
})

test_that("hex is reversible", {
  expect_that(hex(as.hex(x)), equals(as.hex(x)))
})

test_that("temperature is not reversible", {
  expect_that(temperature(as.temperature(x)), not(equals(as.hex(x))))
})
