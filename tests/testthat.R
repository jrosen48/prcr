library(testthat)
library(prcr)

context('create_functions() test')

test_check("prcr")

mtcars_df <- as.data.frame(mtcars[, c("disp", "hp", "wt")])
two_profile_solution <- create_profiles(mtcars_df, 2, to_scale = T)

test_that("output from analysis is prcr object",{
    expect_s3_class(two_profile_solution, "prcr")
})

# larger data set

flights_df <- nycflights13::flights
flights_df <- dplyr::select(flights_df, dep_delay, air_time)
flights_df <- dplyr::sample_n(flights_df, 1000)
flights_df

# two_profile_solution <- prcr::create_profiles(flights_df, 3, to_scale = T)

check_for_class <- function(x){
    if (!is.list(x)){
        return(NA)
    } else {
        return(x[[4]])
    }
}

y <- purrr::map_dbl(2:10, flights_df,to_scale = T, prcr::create_profiles)

iterate_through_solutions <- function(n_start = 2, n_stop = 10, to_scale = F, to_center = F, df){
    the_range <- n_start:n_stop
    y <- purrr::map(the_range, to_scale, to_center, df, prcr::create_profiles)
    out <- check_for_class(y)
    return(out)
}

iterate_through_solutions(2, 10, df = flights_df)

x <- c(2:10)
y <- purrr::map(x, to_scale = T, to_center = T, df = flights_df, prcr::create_profiles)


sapply(y, the_func)