# prcr 0.2.1

* address issue in vignette - warning from R CMD check

# prcr 0.2.0

* major updates

# prcr 0.1.5

* add a function, `detect_outliers()` to detect multivariate outliers based on Hadi's (1994) procedure (thanks to Rebecca Steingut for this contribution) 

    - Note that this will (in this version) be carried out separate from the `create_profiles()` function

* add `plot_raw_data` and `plot_centered_data` as arguments (that can be specified as `TRUE` but default to `FALSE`) `create_profiles()` to change plot of profile centroids (thanks again to Rebecca for input that led to making this addition)

* add of a new function, `cross_validate()`, to perform double split-half cross-validation

* make minor changes to how centering and scaling of data is carried out

* change `create_profiles()`, to now return a `.data` slot, so the original data can be used for subsequent analyses

# prcr 0.1.4

* Fixed error in create_profiles() that returned only the cluster assignment for `.data`, rather than the original `data.frame` with the addition of the cluster assignment

* Added function `plot_r_squared()` to plot R^2 (r-squared) values for a range of number of profiles and updated vignette to include use of `plot_r_squared()` function

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