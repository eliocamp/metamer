set.seed(42)
data <- data.frame(x = rnorm(300),
                   y = rnorm(300))



test_that("metamerize runs", {

  # with defaults
  expect_s3_class(metamerize(data,
                             preserve = moments_n(1:2)),
                  "metamer_list")

  expect_s3_class(metamers <- metamerize(data,
                                          preserve = moments_n(1:2),
                                          perturbation = 0.02,
                                          signif = 2,
                                          stop_if = n_tries(1000)),
                  "metamer_list")
  expect_identical(data, metamers$metamers[[1]])

})

metamers <- metamerize(data,
                        preserve = moments_n(1:2),
                        perturbation = 0.02,
                        signif = 2,
                        stop_if = n_tries(1000))

test_that("metamerize can be chained", {
  skip_if_not(exists("metamers"))
  expect_s3_class(metamers <<- metamerize(metamers, stop_if = n_tries(1000)), "metamer_list")
})


test_that("metamers are metamers", {
  skip_if_not(exists("metamers"))
  values <- lapply(metamers$metamers, function(data) signif(moments_n(1:2)(data), 2))

  value1 <- values[[1]]

  n_different <- sum(Reduce("+", lapply(values, function(v) v != value1)))
  expect_equal(n_different, 0)
})



target <- data.frame(x = rnorm(100),
                     y = rnorm(100))
test_that("minimization works", {
  expect_s3_class(metamerize(data,
                             preserve = moments_n(1:2),
                             perturbation = 0.01,
                             minimize = mean_self_proximity,
                             verbose = TRUE,
                             signif = 2,
                             stop_if = n_tries(1000)),
                  "metamer_list")

  expect_s3_class(metamerize(data,
                             preserve = moments_n(1:2),
                             perturbation = 0.01,
                             minimize = c(mean_self_proximity,
                                          mean_dist_to(target)),
                             signif = 2,
                             stop_if = n_tries(1000)),
                  "metamer_list")
})


test_that("metamerize errors well" ,{
  expect_error(metamerize(data,
                          preserve = moments_n(1:2),
                          perturbation = c(1, 2, 3))
  )
})


test_that("methods work" ,{
  m <- as.data.frame(metamers)
  expect_s3_class(m, "data.frame")
  expect_equal(length(unique(m[[".metamer"]])), length(metamers$metamers))

  skip_if_not_installed("ggplot2")
  expect_equal(as.data.frame(metamers),
               fortify.metamer_list(metamers))

  skip_if_not_installed("data.table")
  mdt <- as.data.table.metamer_list(metamers)
  expect_s3_class(mdt, "data.table")
  mdf <- as.data.frame(m)

  expect_equivalent(mdf, m)
})


context(".onLoad")
expect_null(.onLoad())
