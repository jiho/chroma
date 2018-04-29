context("NA handling")

test_that("parsing functions handle NAs", {
  expect_equal(cmyk(NA), as.character(NA))
  expect_equal(css(as.character(NA)),  as.character(NA))
  expect_equal(hcl(NA),  as.character(NA))
  expect_equal(hex(as.character(NA)),  as.character(NA))
  expect_equal(hsi(NA),  as.character(NA))
  expect_equal(hsl(NA),  as.character(NA))
  expect_equal(hsv(NA),  as.character(NA))
  expect_equal(lab(NA),  as.character(NA))
  expect_equal(rgb(NA),  as.character(NA))
  expect_equal(rgba(NA),  as.character(NA))
  expect_equal(temperature(NA), as.character(NA))
})

test_that("conversion functions handle NAs", {
  expect_equivalent(as.cmyk(NA), matrix(ncol=4))
  expect_equal(as.css(NA), as.character(NA))
  expect_equivalent(as.hcl(NA), matrix(ncol=3))
  expect_equal(in_hex(NA), as.character(NA))
  expect_equal(as.hex(NA), as.character(NA))
  expect_equivalent(as.hsi(NA), matrix(ncol=3))
  expect_equivalent(as.hsl(NA), matrix(ncol=3))
  expect_equivalent(as.hsv(NA), matrix(ncol=3))
  expect_equivalent(as.lab(NA), matrix(ncol=3))
  expect_equivalent(as.rgb(NA), matrix(ncol=3))
  expect_equivalent(as.rgba(NA), matrix(ncol=4))
  expect_equal(as.temperature(NA), as.numeric(NA))
})


test_that("channel functions handle NAs", {
  expect_equal(alpha(NA), as.character(NA))
  expect_equal(saturate(NA), as.character(NA))
  expect_equal(brighten(NA), as.character(NA))
  expect_equal(hue(NA), as.numeric(NA))

  expect_equal(luminance(NA), as.numeric(NA))
  x <- "red"
  luminance(x) <- NA
  expect_equal(x, as.character(NA))

  expect_equivalent(channel(NA, "hsv", "h"), NA)
  x <- "red"
  channel(x, "rgb", "r") <- NA
  expect_equal(x, as.character(NA))
})

test_that("binary functions handle NAs", {
  expect_equal(contrast(NA, NA), as.numeric(NA))
  expect_equal(contrast("red", NA), as.numeric(NA))
  expect_equal(delta_e(NA, NA), as.numeric(NA))
  expect_equal(delta_e("red", NA), as.numeric(NA))
  expect_equal(mix(NA, NA), as.character(NA))
  expect_equal(mix("red", NA), as.character(NA))
})

test_that("scale functions handle NAs", {
  # in mapped values
  expect_equal(interp_scale()(NA), as.character(NA))
  expect_equal(brewer_scale()(NA), as.character(NA))
  expect_equal(viridis_scale()(NA), as.character(NA))
  expect_equal(cubehelix_scale()(NA), as.character(NA))
  expect_equal(hue_scale(na.value=NA)(NA), as.character(NA))
  expect_equal(hue_scale()(NA), "#9E9E9E")
  expect_equal(light_scale()(NA), as.character(NA))
  expect_equal(chroma_scale(na.value=NA)(NA), as.character(NA))
  expect_equal(chroma_scale()(NA), "#008B76")
  # as input
  expect_equal(interp_scale(colors=c("white", NA, "black"))(0), "#FFFFFF")
})
