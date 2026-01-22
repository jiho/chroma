context("Parsing reversibility")

x <- rainbow(10)

test_that("cmyk is reversible", {
  expect_equal(cmyk(as.cmyk(x)), as.hex(x))
})

test_that("css is reversible", {
  expect_equal(css(as.css(x)), as.hex(x))
})

test_that("hcl is reversible", {
  expect_equal(hcl(as.hcl(x)), as.hex(x))
})
test_that("lch is reversible", {
  expect_equal(lch(as.lch(x)), as.hex(x))
})

test_that("hex is reversible", {
  expect_equal(hex(as.hex(x)), as.hex(x))
})

test_that("hsi is somewhat reversible", {
  expect_equal(hsi(as.hsi(x[c(3,6)])), as.hex(x[c(3,6)]))
})

test_that("hsl is reversible", {
  expect_equal(hsl(as.hsl(x)), as.hex(x))
})

test_that("hsv is reversible", {
  expect_equal(hsv(as.hsv(x)), as.hex(x))
})

test_that("lab is reversible", {
  expect_equal(lab(as.lab(x)), as.hex(x))
})

test_that("rgb is reversible", {
  expect_equal(rgb(as.rgb(x)), as.hex(x))
})
test_that("rgba is NOT reversible", {
  expect_false(identical(rgba(as.rgba(x)), x))
})

test_that("ryb is not reversible", {
  expect_false(all(ryb(as.ryb(x)) == as.hex(x)))
})

# temperature is not reversible

# wavelength is not reversible
