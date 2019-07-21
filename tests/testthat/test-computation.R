context("Computation with colors")

# check that color computation gives the same results as in the past

# use a fixed set of reference colors
x <- c("cadetblue", "brown1", "darkorchid")

models <- c("hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "lrgb")

test_that("color mixing is replicable", {
  expect_equal(
    unlist(lapply(models, function(m) {average(x, model=m)})),
    c("#009AB4", "#009AB4", "#46BD8F", "#41D39C", "#4BCE9D", "#BA668F", "#A85B8F", "#D23997")
  )

  expect_equal(
    blend(x[1], x[2], mode=c("multiply", "darken", "lighten", "screen", "overlay", "burn", "dodge")),
    c("#5F2828", "#5F4040", "#FF9EA0", "#FFB6B8", "#BE6E71", "#5F0000", "#FFD3D6")
  )

  expect_equal(
    mix(x[1], x[2], model=models),
    c("#7F9837", "#7F9837", "#854FB9", "#924AD5", "#9558D0", "#C27D6F", "#AF6F70", "#C0797A")
  )
})

test_that("mixing and averaging are equivalent in lab and RGB", {
  expect_equal(
    mix(x[1], x[2], model="lab"),
    average(x[1:2], model="lab")
  )
  expect_equal(
    mix(x[1], x[2], model="rgb"),
    average(x[1:2], model="rgb")
  )
})

test_that("distance measures are replicable", {
  expect_equal(
    color_distance(x[1], x[2], model=models),
    c(179.52478, 179.52478, 181.550336, 181.847724, 181.846859, 104.33064, 208.93061),
    tol=10^-5
  )
  expect_equal(contrast(x[1], x[2]), 1.13624, tol=10^-5)
  expect_equal(CMClc(x[1], x[2]), 74.89912, tol=10^-5)
  expect_equal(deltaE(x[1], x[2]), 50.12273, tol=10^-5)
})

