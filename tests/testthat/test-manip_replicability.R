context("Manipulation replicability")

# check that color manipulation gives the same results as in the past
# NB: reference set on 2018-04-29

set.seed(12)
x <- sample(colors(), 3)

test_that("alpha is replicable", {
  expect_equal(alpha(x), c("#53868B80", "#FFB5C580", "#4F94CD80"))
})

test_that("extracting a channel is replicable", {
  expect_equal(channel(x, "hsv", "h"), c(185.357142857143, 347.027027027027, 207.142857142857))
  expect_equal(channel(x, "hsv", "s"), c(0.402877697841727, 0.290196078431373, 0.614634146341463))
  expect_equal(channel(x, "hsv", "v"), c(0.545098039215686, 1, 0.803921568627451))

  expect_equal(channel(x, "hsl", "h"), c(185.357142857143, 347.027027027027, 207.142857142857))
  expect_equal(channel(x, "hsl", "s"), c(0.252252252252252, 1, 0.557522123893805))
  expect_equal(channel(x, "hsl", "l"), c(0.435294117647059, 0.854901960784314, 0.556862745098039))

  expect_equal(channel(x, "hsi", "h"), c(184.627259370752, 348.143208763333, 206.852718479213))
  expect_equal(channel(x, "hsi", "s"), c(0.300561797752809, 0.14218009478673, 0.451388888888889))
  expect_equal(channel(x, "hsi", "i"), c(0.465359477124183, 0.827450980392157, 0.564705882352941))

  expect_equal(channel(x, "hcl", "h"), c(207.244405195265, 5.30193518912813, 262.812948295811))
  expect_equal(channel(x, "hcl", "c"), c(0.176500706837288, 0.291747089019765, 0.361235804515684))
  expect_equal(channel(x, "hcl", "l"), c(0.526771313822724, 0.809289198650285, 0.592027005339131))

  expect_equal(channel(x, "lab", "l"), c(0.526771313822724, 0.809289198650285, 0.592027005339131))
  expect_equal(channel(x, "lab", "a"), c(-0.15692004452542, 0.290498871785959, -0.045193858204608))
  expect_equal(channel(x, "lab", "b"), c(-0.0807997471543231, 0.0269586617359212, -0.358397574830907
))

  expect_equal(channel(x, "cmyk", "c"), c(0.402877697841727, 0, 0.614634146341463))
  expect_equal(channel(x, "cmyk", "m"), c(0.0359712230215826, 0.290196078431373, 0.278048780487805))
  expect_equal(channel(x, "cmyk", "y"), c(0, 0.227450980392157, 0))
  expect_equal(channel(x, "cmyk", "k"), c(0.454901960784314, 0, 0.196078431372549))
})

test_that("manipulating lightness is replicable", {
  expect_equal(lighten(x), c("#82B6BB", "#FFE8F8", "#84C4FF"))
  expect_equal(darken(x),  c("#25595E", "#CB8595", "#00669C"))
})

test_that("manipulating luminance is replicable", {
  x_modif <- x
  luminance(x_modif) <- 0.2
  expect_equal(x_modif, c("#528489", "#9D6F79", "#4580B2"))
})

test_that("manipulating saturation is replicable", {
  expect_equal(saturate(x)  , c("#008D99", "#FFA6C3", "#0097ED"))
  expect_equal(desaturate(x), c("#7E7E7E", "#DFC2C7", "#7791AE"))
})
