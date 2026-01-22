context("Manipulation reversibility")

x <- rainbow(10)
test_that("manipulating alpha is NOT reversible", {
  expect_false(identical(alpha(alpha(x, 0.5), 1), x))
})

hues <- seq(0, 300, 50)

x <- hsl(hues)
test_that("manipulating hue is reversible", {
  x_modif <- x
  channel(x_modif, "hsl", "h") <- 100
  channel(x_modif, "hsl", "h") <- hues
  expect_equal(x_modif, x)
})

x <- hsl(hues, l=0.5)
test_that("manipulating lightness is reversible", {
  expect_equal(lighten(darken(x, 0.1), 0.1), x)
  expect_equal(darken(lighten(x, 0.1), 0.1), x)
})

# luminance manipulation is not reversible
# x <- hsl(hues, l=0.5)
# test_that("manipulating luminance is reversible", {
#   lums <- luminance(x)
#   x_modif <- x
#   luminance(x_modif) <- 0.2
#   luminance(x_modif) <- lums
#   expect_equal(x_modif, x)
# })

x <- hsv(hues, s=0.5)
test_that("manipulating saturation is reversible", {
  expect_equal(desaturate(saturate(x, 0.5), 0.5), x)
  expect_equal(saturate(desaturate(x, 0.5), 0.5), x)
})
