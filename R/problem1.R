# blblm.old <- function(formula, data, m = 10, B = 5000) {
#   data_list <- split_data(data, m)
#   estimates <- map(
#     data_list,
#     ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
#   res <- list(estimates = estimates, formula = formula)
#   class(res) <- "blblm"
#   invisible(res)
# }

# why my parallel version is consistently slower than the non-parallel
res0 <-
bench::mark(blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100),
            blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE ),
            blblm.old(mpg ~ wt * hp, data = mtcars, m = 3, B = 100),
            check = FALSE,
            filter_gc = FALSE,
            memory = FALSE )

Rprof()
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
Rprof(NULL)
result <- summaryRprof()

Rprof()
fit <- blblm.old(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
Rprof(NULL)
result2 <- summaryRprof()

mtcars2 <- mtcars[sample.int(32, size = 320, replace = TRUE), ]

bench::mark(blblm(mpg ~ wt * hp, data = mtcars2, B = 100 ),
            blblm(mpg ~ wt * hp, data = mtcars2, B = 100, parallel = FALSE),
            blblm.old(mpg ~ wt * hp, data = mtcars2, B = 100),
            check = FALSE )
