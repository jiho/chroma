context("Delta E computation")

# read dataset from Sharma et al 2005
d <- read.table("ciede2000testdata.txt")

test_that("deltaE implementation matches reference",{
  expect_equal(
    apply(d, 1, function(x) {deltaE_lab(x[1:3], x[4:6])}),
    d[,7],
    tolerance=0.01
  )
})

