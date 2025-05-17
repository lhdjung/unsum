
test_that("`horns()` works correctly in the 1-5 case", {
  c(300, 0, 0, 0, 0)    %>% horns(1, 5) %>% expect_equal(0)
  c(150, 0, 0, 0, 150)  %>% horns(1, 5) %>% expect_equal(1)
  c(60, 60, 60, 60, 60) %>% horns(1, 5) %>% expect_equal(0.5)
})


test_that("`horns()` works correctly in the 1-2 case", {
  c(10, 10) %>% horns(1, 2) %>% expect_equal(1)
  c(10, 0)  %>% horns(1, 2) %>% expect_equal(0)
  c(10, 5)  %>% horns(1, 2) %>% expect_equal(0.8888889)
})

