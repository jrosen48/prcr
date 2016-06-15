# prcr
`R` package for person-centered analysis.

Because this package is in development and is not yet available on CRAN, to install it, first install the `devtools` package using `install.packages("devtools")`, followed by the function `devtools::install_github("jrosen48/prcr")`. After installing the package, use `library(prcr)` to load it each session.

This package is organized around four functions:

1. `prepare_data()`
2. `create_profiles()`
3. `calculate_stats()`
4. `explore_factors()`

The basic workflow is to first prepare the data (by removing incomplete cases and centering and / or scaling the data) with `prepare data()`, to create profiles with the `create_profiles()` function, calculate statistics about the profiles using the `calculate_stats()` function, and explore the distribution of profiles across select factors with the `explore_factors()` function.

To review basic documentation, call each function prefaced with `?` .