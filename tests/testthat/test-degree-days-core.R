test_that("average degree-days use the daily mean above base", {
  expect_equal(degree_days(10, 20, 5, method = "average"), 10)
  expect_equal(degree_days(1, 5, 7, method = "average"), 0)
})

test_that("upper-threshold methods require a valid upper threshold", {
  expect_error(
    degree_days(10, 20, 5, method = "average_cut"),
    "Tupper greater than Tbase"
  )
})

test_that("average_cut applies a vertical upper cutoff to the daily mean", {
  expect_equal(degree_days(10, 20, 5, Tupper = 25, method = "average_cut"), 10)
  expect_equal(degree_days(20, 40, 5, Tupper = 25, method = "average_cut"), 0)
})

test_that("triangle and sine methods reduce to the mean when the full curve is above base", {
  expect_equal(degree_days(10, 20, 5, method = "triangle"), 10, tolerance = 0.01)
  expect_equal(degree_days(10, 20, 5, method = "sine"), 10, tolerance = 0.01)
})

test_that("upper-threshold triangle and sine methods reduce accumulation", {
  tri <- degree_days(10, 35, 5, method = "triangle")
  tri_upper <- degree_days(10, 35, 5, Tupper = 30, method = "triangle_upper")
  sine <- degree_days(10, 35, 5, method = "sine")
  sine_upper <- degree_days(10, 35, 5, Tupper = 30, method = "sine_upper")
  
  expect_lt(tri_upper, tri)
  expect_lt(sine_upper, sine)
})