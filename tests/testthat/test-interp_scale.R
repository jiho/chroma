context("Interpolated scale")

cols <- c("#000000", "#FFFFFF")

test_that("colors are interpolated correctly", {
  bw <- interp_scale(cols, model="rgb")
  expect_equal(bw(0),   cols[1])
  expect_equal(bw(0.5), "#808080")
  expect_equal(bw(1),   cols[2])
})

test_that("various color models work", {
  expect_equal(interp_scale(cols, model="hcl")(0.5), "#777777")
  expect_equal(interp_scale(cols, model="lch")(0.5), "#777777")
  expect_equal(interp_scale(cols, model="hsi")(0.5), "#808080")
  expect_equal(interp_scale(cols, model="hsl")(0.5), "#808080")
  expect_equal(interp_scale(cols, model="hsv")(0.5), "#808080")
  expect_equal(interp_scale(cols, model="lab")(0.5), "#777777")
  expect_equal(interp_scale(cols, model="rgb")(0.5), "#808080")
  expect_equal(interp_scale(cols, model="lrgb")(0.5), "#B4B4B4")
})

test_that("reverse works", {
  expect_equal(
    interp_scale(reverse=TRUE)(c(0,1)),
    rev(interp_scale(reverse=FALSE)(c(0,1)))
  )
})

test_that("data outside domain returns NA or extreme color", {
  bw <- interp_scale(cols, domain=c(0,1))
  expect_equal(bw(-1), NA_character_)
  expect_equal(bw(2),  NA_character_)
  bw <- interp_scale(cols, domain=c(0,1), extrapolate=TRUE)
  expect_equal(bw(-1), cols[1])
  expect_equal(bw(2),  cols[2])
})

test_that("values overrides domain", {
  bw <- interp_scale(cols, domain=c(0,1), values=c(3,4))
  expect_equal(bw(0.5), NA_character_)
  expect_equal(bw(3),   cols[1])
  expect_equal(bw(4),   cols[2])
})

test_that("values overrides reverse", {
  bw <- interp_scale(cols, reverse=TRUE, values=c(3,4))
  expect_equal(bw(3), cols[1])
  expect_equal(bw(4), cols[2])
})

test_that("unsorted values returns error", {
  expect_error(interp_scale(c("red", "green", "blue"), values=c(1,3,2)))
})

test_that("different number of values and colors returns error", {
  expect_error(interp_scale(c("red", "green"), values=c(1,2,3)))
  expect_error(interp_scale(c("red", "green", "blue"), values=c(1,2)))
})
