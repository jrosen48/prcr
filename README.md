# prcr
`R` package for person-centered analysis.

To install it, you can use the `devtools` package and the function `devtools::install_github("jrosen48/prcr")`.

This package is organized around four functions:

1. `prepare_data()`
2. `cluster_data()`
3. `calculate_stats()`
4. `explore_factors()`

Call each function prefaced with `?` to review documentation.

Three additional functions are in the works:

1. `compare_clusters()` to compare fit indices across cluster solutions specified with varying parameters, such as different methods of centering or numbers of clusters.
2. `create_profiles()` to create profiles from data using Latent Profile Analysis (LPA) (to compare to or be used in replace of `cluster_data()`.
2. `cross_validate()` to perform split-half (or other types) of cross validation.

