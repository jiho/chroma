context("Computation with colors")

# check that color computation gives the same results as in the past
# NB: reference set on 2018-05-01

set.seed(12)
x <- sample(colors(), 3)

models <- c("hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "lrgb")

test_that("color mixing is replicable", {
  expect_equal(
    mix(x[1], x[2], model=models),
    c("#99A0CB", "#99A0CB", "#A280CC", "#9D6CDD", "#9F81C5", "#AD9FA7", "#A99EA8", "#BE9FAA")
  )
  expect_equal(
    unlist(lapply(models, function(m) {average(x[1:2], model=m)})),
    c("#65AEB7", "#65AEB7", "#80B8B6", "#6CDDDB", "#81C5C4", "#AD9FA7", "#A99EA8", "#FFB5C5")
  )
  expect_equal(
    unlist(lapply(models, function(m) {average(x, model=m)})),
    c("#5AA99D", "#5AA99D", "#6FC4A7", "#62D8B1", "#71C8AB", "#959BB4", "#8B9AB4", "#BDA5C9")
  )
  expect_equal(
    blend(x[1], x[2], mode=c("multiply", "darken", "lighten", "screen", "overlay", "burn", "dodge")),
    c("#535F6B", "#53868B", "#FFB5C5", "#FFDCE5", "#A6B9CA", "#535569", "#FFFFFF")
  )
})

# test_that("mixing and averaging are equivalent", {
#   expect_equal(
#     mix(x[1], x[2], model=models),
#     unlist(lapply(models, function(m) {average(x[1:2], model=m)}))
#   )
# })

test_that("distance measures are replicable", {
  expect_equal(
    color_distance(x[1], x[2], model=models),
    c(204.23453, 204.23453, 163.51643, 161.67216, 161.67056, 54.0011, 187.502),
    tol=10^-5
  )
  expect_equal(contrast(x[1], x[2]), 2.459453, tol=10^-5)
  expect_equal(CMClc(x[1], x[2]), 46.22181, tol=10^-5)
  expect_equal(deltaE(x[1], x[2]), 46.96253, tol=10^-5)
})

