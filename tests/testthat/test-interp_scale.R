context("Interpolated scale")

test_that("colors are interpolated correctly", {
  cols <- c("#000000", "#FFFFFF")
  bw <- interp_scale(cols, model="rgb")

  expect_equal(bw(0),   "#000000")
  expect_equal(bw(0.5), "#7F7F7F")
  expect_equal(bw(1),   "#FFFFFF")
})

test_that("reverse works", {
  expect_equal(
    interp_scale(reverse=TRUE)(c(0,1)),
    rev(interp_scale(reverse=FALSE)(c(0,1)))
  )
})

test_that("data outside domain returns extreme color", {
  cols <- c("#000000", "#FFFFFF")
  bw <- interp_scale(cols, domain=c(0,1))

  expect_equal(bw(-1), "#000000")
  expect_equal(bw(2),  "#FFFFFF")
})

test_that("values overrides domain", {
  cols <- c("#000000", "#FFFFFF")
  bw <- interp_scale(cols, domain=c(0,1), values=c(3,4))

  expect_equal(bw(0.5), cols[1])
  expect_equal(bw(3),   cols[1])
  expect_equal(bw(4),   cols[2])
})

test_that("values overrides reverse", {
  cols <- c("#000000", "#FFFFFF")
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
