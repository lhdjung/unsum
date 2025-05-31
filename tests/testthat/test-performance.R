test_that("complexity is correctly calculated", {
  closure_gauge_complexity(mean = 4.26, sd = 1.3, n = 118, scale_min = 1, scale_max = 7) |> expect_equal(2.69897000433602)
  closure_gauge_complexity(mean = 1.55, sd = 0.2, n = 74,  scale_min = 1, scale_max = 7) |> expect_equal(1.65321251377534)
})
