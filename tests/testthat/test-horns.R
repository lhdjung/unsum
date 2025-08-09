
test_that("`horns()` works correctly in the 1-5 case", {
  c(300, 0, 0, 0, 0)      |> horns(1, 5) |> expect_equal(0)
  c(150, 0, 0, 0, 150)    |> horns(1, 5) |> expect_equal(1)
  c(100, 0, 0, 0, 200)    |> horns(1, 5) |> expect_equal(0.8888889)
  c(60, 60, 60, 60, 60)   |> horns(1, 5) |> expect_equal(0.5)
  c(200, 50, 30, 20, 0)   |> horns(1, 5) |> expect_equal(0.2113889, tolerance = 1e-6)
  c(150, 100, 50, 0, 0)   |> horns(1, 5) |> expect_equal(0.1388889, tolerance = 1e-6)
  c(100, 40, 20, 40, 100) |> horns(1, 5) |> expect_equal(0.7333333, tolerance = 1e-6)
})


test_that("`horns()` works correctly in the 1-2 case", {
  c(10, 10) |> horns(1, 2) |> expect_equal(1)
  c(10, 0)  |> horns(1, 2) |> expect_equal(0)
  c(10, 5)  |> horns(1, 2) |> expect_equal(0.8888889)
})


test_that("`horns_uniform()` works correctly with scales starting at 1", {
  1 |> horns_uniform(2) |> expect_equal(1)
  1 |> horns_uniform(5) |> expect_equal(0.5)
  1 |> horns_uniform(6) |> expect_equal(0.4666667, tolerance = 1e-6)
  1 |> horns_uniform(7) |> expect_equal(0.4444444, tolerance = 1e-6)
  1 |> horns_uniform(8) |> expect_equal(0.4285714, tolerance = 1e-6)
})


test_that("`horns_uniform()` works correctly with scales *NOT* starting at 1", {
  5 |> horns_uniform(10) |> expect_equal(0.4666667, tolerance = 1e-6)
  8 |> horns_uniform(12) |> expect_equal(0.5)
  27 |> horns_uniform(34) |> expect_equal(0.4285714, tolerance = 1e-6)
  50 |> horns_uniform(55) |> expect_equal(0.4666667, tolerance = 1e-6)
  -3 |> horns_uniform(3) |> expect_equal(0.4444444, tolerance = 1e-6)
})
