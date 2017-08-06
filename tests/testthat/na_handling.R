context("NA handling")

test_that("parsing functions handle NAs", {
  expect_equal(cmyk(NA), as.character(NA))
  expect_equal(css(NA),  as.character(NA))
  expect_equal(hcl(NA),  as.character(NA))
  expect_equal(hex(NA),  as.character(NA))
  expect_equal(hsi(NA),  as.character(NA))
  expect_equal(hsl(NA),  as.character(NA))
  expect_equal(hsv(NA),  as.character(NA))
  expect_equal(lab(NA),  as.character(NA))
  expect_equal(rgb(NA),  as.character(NA))
  expect_equal(temperature(NA), as.character(NA))
})
