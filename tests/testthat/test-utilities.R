
data <- data.frame(x = rnorm(100),
                   y = rnorm(100))

data2 <- data.frame(x = rnorm(100),
                    y = rnorm(100))

context("minimizing/preserve functions")
test_that("delayed_with works",
          {
            delay <- delayed_with("Mean x" = mean(x),
                                                 Y = mean(y),
                                                 sd(y))

            expect_equal(class(delay), "function")

            expect_length(delay_result <- delay(data), 3)
            expect_named(delay_result)
            expect_equal(names(delay_result), c("Mean x", "Y", ""))
          }
)

test_that("mean_dist_to works", {

  expect_equal(class(mean_dist_to(data)), "function")

  expect_equal(mean_dist_to(data)(data), 0)
  expect_gt(mean_dist_to(data)(data2), 0)

})

test_that("mean_self_proximity works", {
  expect_gt(mean_self_proximity(data), 0)

})


test_that("moments_n works", {
  moment <- moments_n(1:3)
  expect_equal(class(moment), "function")

  moment <- moments_n(1:3, cols = "x")
  expect_equal(class(moment), "function")

  expect_length(moment <- moment(data), 3)
  expect_length(names(moment), 3)

})


context("draw_data")
test_that("densify works", {
  N_groups <- vapply(split(test_drawing, test_drawing[[".group"]]),
                     nrow, 1)
  nrow_expected <- sum(ifelse(N_groups > 1, N_groups*2, N_groups))

  expect_equal(nrow(densify(test_drawing, res = 2)), nrow_expected)

  nrow_expected <- sum(ifelse(N_groups > 1, N_groups*3, N_groups))
  expect_equal(nrow(densify(test_drawing, res = 3)), nrow_expected)

  expect_equivalent(densify(test_drawing, res = 1), test_drawing)
})


test_that("draw_data works", {
  expect_error(draw_data())
})



