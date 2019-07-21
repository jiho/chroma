context("Manipulation replicability")

# check that color manipulation gives the same results as in the past

# use a fixed set of reference colors
x <- c("cadetblue", "brown1", "darkorchid")

test_that("alpha is replicable", {
  expect_equal(alpha(x), c("#5F9EA080", "#FF404080", "#9932CC80"))
})

test_that("extracting a channel is replicable", {
  expect_equal(channel(x, "rgb", "r"), c(0.3725490196, 1, 0.6))
  expect_equal(channel(x, "rgb", "g"), c(0.6196078431, 0.2509803922, 0.1960784314))
  expect_equal(channel(x, "rgb", "b"), c(0.6274509804, 0.2509803922, 0.8))

  expect_equal(channel(x, "ryb", "r"), c(0.1691907095, 0.6191190465, 0.2800219608))
  expect_equal(channel(x, "ryb", "y"), c(0.332681964, 0.1879892349, 0.106854902))
  expect_equal(channel(x, "ryb", "b"), c(0.401563465, 0.0881669512, 0.5233003922))

  expect_equal(channel(x, "hsv", "h"), c(181.8461538462, 0, 280.1298701299))
  expect_equal(channel(x, "hsv", "s"), c(0.40625, 0.7490196078, 0.7549019608))
  expect_equal(channel(x, "hsv", "v"), c(0.6274509804, 1, 0.8))

  expect_equal(channel(x, "hsl", "h"), c(181.8461538462, 0, 280.1298701299))
  expect_equal(channel(x, "hsl", "s"), c(0.2549019608, 1, 0.6062992126))
  expect_equal(channel(x, "hsl", "l"), c(0.5, 0.6254901961, 0.4980392157))

  expect_equal(channel(x, "hsi", "h"), c(181.5502341171, 0, 281.0314183059))
  expect_equal(channel(x, "hsi", "s"), c(0.3099273608, 0.498694517, 0.6314496314))
  expect_equal(channel(x, "hsi", "i"), c(0.539869281, 0.5006535948, 0.5320261438))

  expect_equal(channel(x, "hcl", "h"), c(200.6605961951, 32.4282296757, 317.3115137675))
  expect_equal(channel(x, "hcl", "c"), c(0.2103208217, 0.8358372297, 0.8863812927))
  expect_equal(channel(x, "hcl", "l"), c(0.6115314791, 0.5737030552, 0.4338024113))

  expect_equal(channel(x, "lab", "l"), c(0.6115314791, 0.5737030552, 0.4338024113))
  expect_equal(channel(x, "lab", "a"), c(-0.1967944384, 0.7054999656, 0.6515353303))
  expect_equal(channel(x, "lab", "b"), c(-0.0742077965, 0.4482116387, -0.6009771289))

  expect_equal(channel(x, "cmyk", "c"), c(0.40625, 0, 0.25))
  expect_equal(channel(x, "cmyk", "m"), c(0.0125, 0.7490196078, 0.7549019608))
  expect_equal(channel(x, "cmyk", "y"), c(0, 0.7490196078, 0))
  expect_equal(channel(x, "cmyk", "k"), c(0.3725490196, 0, 0.2))
})

test_that("manipulating lightness is replicable", {
  expect_equal(lighten(x), c("#8FCFD1", "#FF786C", "#CD65FF"))
  expect_equal(darken(x),  c("#2F6F72", "#C40018", "#66009A"))
})

test_that("manipulating luminance is replicable", {
  x_modif <- x
  luminance(x_modif) <- 0.2
  expect_equal(x_modif, c("#508486", "#E53A3A", "#AB56D5"))
})

test_that("manipulating saturation is replicable", {
  expect_equal(saturate(x)  , c("#00A5AB", "#FF0030", "#A013E1"))
  expect_equal(desaturate(x), c("#8D9595", "#EA5950", "#9143B7"))
})
