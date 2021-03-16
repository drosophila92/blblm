# this cause problems during checking examples
blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE )
# this doesn't
blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE )

# add this solves the problem
#' closeAllConnections()

# why some functions are not exported, but can be accessed by users