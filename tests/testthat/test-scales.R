context("Scale building")

test_that("colors are interpolated correctly", {
  cols <- c("#000000", "#ffffff")
  bw <- interp_scale(cols, model="rgb")

  expect_that(bw(0), equals("#000000"))
  expect_that(bw(0.5), equals("#7f7f7f"))
  expect_that(bw(1), equals("#ffffff"))
})

test_that("reverse works", {
  expect_that(
    interp_scale(reverse=TRUE)(c(0,1)),
    equals( rev(interp_scale(reverse=FALSE)(c(0,1))) )
  )
})

test_that("data outside domain returns extreme color", {
  cols <- c("#000000", "#ffffff")
  bw <- interp_scale(cols, domain=c(0,1))

  expect_that(bw(-1), equals("#000000"))
  expect_that(bw(2), equals("#ffffff"))
})

test_that("values overrides domain", {
  cols <- c("#000000", "#ffffff")
  bw <- interp_scale(cols, domain=c(0,1), values=c(3,4))

  expect_that(bw(0.5), equals(cols[1]))
  expect_that(bw(3), equals(cols[1]))
  expect_that(bw(4), equals(cols[2]))
})

test_that("values overrides reverse", {
  cols <- c("#000000", "#ffffff")
  bw <- interp_scale(cols, domain=c(0,1), values=c(3,4))

  expect_that(bw(3), equals(cols[1]))
  expect_that(bw(4), equals(cols[2]))
})

test_that("unsorted values returns error", {
  expect_error(interp_scale(c("red", "green", "blue"), values=c(1,3,2)))
})

test_that("different number of values and colors returns error", {
  expect_error(interp_scale(c("red", "green"), values=c(1,2,3)))
  expect_error(interp_scale(c("red", "green", "blue"), values=c(1,2)))
})
