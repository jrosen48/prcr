
## Background

`prcr` is an `R` package for person-centered analysis. Person-centered analyses focus on clusters, or profiles, of observations, and their change over time or differences across factors. See [Bergman and El-Khouri (1999)](http://onlinelibrary.wiley.com/doi/10.1002/(SICI)1521-4036(199910)41:6%3C753::AID-BIMJ753%3E3.0.CO;2-K/abstract) for a description of the analytic approach. See [Corpus and Wormington (2014)](http://www.tandfonline.com/doi/abs/10.1080/00220973.2013.876225) for an example of person-centered analysis in psychology and education.

## Example using mtcars

In this example using the built-in to R `mtcars` data for fuel consumption and other information for 32 automobiles, the variables `disp` (for engine displacement, in cu. in.), `qsec` (for the 1/4 mile time, in seconds), and `wt` for weight (in 1000 lbs.) are clustered with a `2` cluster solution specified. Because the variables are in very different units, the `to_scale` argument is set to `TRUE`.

  library(prcr)
  mtcars_df <- as.data.frame(mtcars[, c("disp", "hp", "wt")])
  two_profile_solution <- create_profiles(mtcars_df, 2, to_scale = T)
  summary(two_profile_solution)
  print(two_profile_solution)
  plot(two_profile_solution)

The output has the class `prcr` and has slots for additional information that can be extracted from it, such as the r-squared (for comparing the relative fit of different cluster solutions) raw clustered data (i.e., for conducting statistical tests to determine whether the cluster centroids are different from one another and for use in additional analyses) and the processed data (i.e., for creating different plots of the cluster centroids).

  two_profile_solution$r_squared
  two_profile_solution$clustered_raw_data
  two_profile_solution$clustered_processed_data

Functions for easily comparing the r-squared value for a range of cluster solutions, and for carrying out cross-validation of the clustering solution, will be added in future updates to the package. 
