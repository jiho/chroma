context("NA handling")

test_that("parsing functions handle NAs", {
  expect_equal(cmyk(NA), NA_character_)
  expect_equal(css(NA_character_),  NA_character_)
  expect_equal(hcl(NA),  NA_character_)
  expect_equal(hex(NA_character_),  NA_character_)
  expect_equal(hsi(NA),  NA_character_)
  expect_equal(hsl(NA),  NA_character_)
  expect_equal(hsv(NA),  NA_character_)
  expect_equal(lab(NA),  NA_character_)
  expect_equal(rgb(NA),  NA_character_)
  expect_equal(rgba(NA),  NA_character_)
  expect_equal(temperature(NA), NA_character_)
  expect_equal(wavelength(NA), NA_character_)
})

test_that("conversion functions handle NAs", {
  expect_equivalent(as.cmyk(NA), matrix(ncol=4))
  expect_equal(as.css(NA), NA_character_)
  expect_equivalent(as.hcl(NA), matrix(ncol=3))
  expect_equal(in_hex(NA), NA_character_)
  expect_equal(as.hex(NA), NA_character_)
  expect_equivalent(as.hsi(NA), matrix(ncol=3))
  expect_equivalent(as.hsl(NA), matrix(ncol=3))
  expect_equivalent(as.hsv(NA), matrix(ncol=3))
  expect_equivalent(as.lab(NA), matrix(ncol=3))
  expect_equivalent(as.rgb(NA), matrix(ncol=3))
  expect_equivalent(as.rgba(NA), matrix(ncol=4))
  expect_equal(as.temperature(NA), NA_real_)
  expect_equal(as.wavelength(NA), NA_real_)
})


test_that("manipulation functions handle NAs", {
  expect_equal(alpha(NA), NA_character_)
  expect_equal(saturate(NA), NA_character_)
  expect_equal(brighten(NA), NA_character_)
  expect_equal(hue(NA), NA_real_)

  expect_equal(luminance(NA), NA_real_)
  x <- "red"
  luminance(x) <- NA
  expect_equal(x, NA_character_)

  expect_equivalent(channel(NA, "hsv", "h"), NA)
  x <- "red"
  channel(x, "rgb", "r") <- NA
  expect_equal(x, NA_character_)
})

test_that("computation functions handle NAs", {
  expect_equal(average(c(NA, NA)), NA_character_)
  expect_equal(average(c("red", NA)), NA_character_)
  expect_equal(average(c("#FF0000", NA), na.rm=TRUE), "#FF0000")
  expect_equal(blend(NA, NA), NA_character_)
  expect_equal(blend("red", NA), NA_character_)
  expect_equal(color_distance(NA, NA), NA_real_)
  expect_equal(color_distance("red", NA), NA_real_)
  expect_equal(contrast(NA, NA), NA_real_)
  expect_equal(contrast("red", NA), NA_real_)
  expect_equal(CMClc(NA, NA), NA_real_)
  expect_equal(CMClc("red", NA), NA_real_)
  expect_equal(deltaE(NA, NA), NA_real_)
  expect_equal(deltaE("red", NA), NA_real_)
  expect_equal(mix(NA, NA), NA_character_)
  expect_equal(mix("red", NA), NA_character_)
})

test_that("scale functions handle NAs", {
  expect_equal(interp_scale(na.value=NA)(NA), NA_character_)
  expect_equal(interp_scale()(NA), "#808080")
  expect_equal(interp_scale(colors=c("white", NA, "black"))(0), "#FFFFFF")

  # expect_equal(brewer_scale(na.value=NA)(NA), NA_character_)
  expect_equal(viridis_scale(na.value=NA)(NA), NA_character_)
  expect_equal(viridis_scale()(NA), "#828282")
  expect_equal(cubehelix_scale(na.value=NA)(NA), NA_character_)
  expect_equal(cubehelix_scale()(NA), "#808080")

  expect_equal(hue_scale(na.value=NA)(NA), NA_character_)
  expect_equal(hue_scale()(NA), "#9E9E9E")

  expect_equal(chroma_scale(na.value=NA)(NA), NA_character_)
  expect_equal(chroma_scale()(NA), "#008B76")

  expect_equal(light_scale(na.value=NA)(NA), NA_character_)
  expect_equal(light_scale()(NA), "#777777")
})
