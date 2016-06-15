# prcr
`prcr` is an `R` package for person-centered analysis. Person-centered analyses focus on clusters, or profiles, of observations, and their change over time or differences across factors. See [Bergman and El-Khouri (1999)](http://onlinelibrary.wiley.com/doi/10.1002/(SICI)1521-4036(199910)41:6%3C753::AID-BIMJ753%3E3.0.CO;2-K/abstract) for a description of the analytic approach and [Corpus and Wormington (2014)](http://www.tandfonline.com/doi/abs/10.1080/00220973.2013.876225) for an example of person-centered analysis in psychology and education, and sc

Because this package is in development and is not yet available on CRAN, to install it, first install the `devtools` package using `install.packages("devtools")`, followed by the function `devtools::install_github("jrosen48/prcr")`. After installing the package, use `library(prcr)` to load it each session.

This package is organized around four functions:

1. `prepare_data()`
2. `create_profiles()`
3. `calculate_stats()`
4. `explore_factors()`

The basic workflow is to first prepare the data (by removing incomplete cases and centering and / or scaling the data) with `prepare data()`, to create profiles with the `create_profiles()` function, calculate statistics about the profiles using the `calculate_stats()` function, and explore the distribution of profiles across select factors with the `explore_factors()` function.

To review basic documentation, call each function prefaced with `?` .