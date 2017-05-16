# prcr 0.1.3

* Added new interface for main (`create_profiles()`) function so that variables to create profiles must be specified, rather than every variable in the data frame being used to create profiles being used. This change means that the `data.frame` does not need to be subset before using this package, and also that it is easier to use cluster assignments in subsequent analyses because the original data frame with either a variable for the cluster assignments or dummy-coded variables for each cluster are returned.

* Changed `n_clusters` argument in `create_profiles()` to `n_profiles`

# prcr 0.1.2

* Changed axes for plot associated with plot method, so clusters are on the x-axis and variables are on the y-axis

* Used R version 3.4

* Add URL and BugReports fields to DESCRIPTION

# prcr 0.1.1

* Added a `NEWS.md` file to track changes to the package.

* Fix r-squared, which was calculated using the sum of the within-cluster sum of squares divided by the total sum of squares, rather than the between-cluster sum of squares divided by the total sum of squares

* Fix plot, which was reversed

# prcr 0.1.0

* Initial release