
<!-- README.md is generated from README.Rmd. Please edit that file -->
prcr
====

`prcr` is an `R` package for person-centered analysis. Person-centered analyses focus on clusters, or profiles, of observations, and their change over time or differences across factors. See [Bergman and El-Khouri (1999)](http://onlinelibrary.wiley.com/doi/10.1002/(SICI)1521-4036(199910)41:6%3C753::AID-BIMJ753%3E3.0.CO;2-K/abstract) for a description of the analytic approach. See [Corpus and Wormington (2014)](http://www.tandfonline.com/doi/abs/10.1080/00220973.2013.876225) for an example of person-centered analysis in psychology and education.

Installation
------------

You can install the development version of `prcr` (v. `0.2.0`) from Github with:

``` r
# install.packages("devtools")
devtools::install_github("jrosen48/prcr")
```

You can install `prcr` from CRAN (v. `0.1.5`) with:

``` r
install.packages("prcr")
```

Example
-------

This is a basic example using the built-in dataset `pisaUSA15`:

``` r
library(prcr)
```

``` r
df <- pisaUSA15
m3 <- create_profiles_cluster(df, broad_interest, enjoyment, instrumental_mot, self_efficacy, n_profiles = 3)
#> Prepared data: Removed 354 incomplete cases
#> Hierarchical clustering carried out on: 5358 cases
#> K-means algorithm converged: 5 iterations
#> Clustered data: Using a 3 cluster solution
#> Calculated statistics: R-squared = 0.424
summary(m3)
#> # A tibble: 3 x 5
#>                 Cluster broad_interest enjoyment instrumental_mot
#>                   <chr>          <dbl>     <dbl>            <dbl>
#> 1 Profile 1 (2458 obs.)       2.829468  2.841131         2.322010
#> 2 Profile 2 (1598 obs.)       3.177023  3.407541         1.395599
#> 3 Profile 3 (1302 obs.)       1.683218  1.903418         2.430748
#> # ... with 1 more variables: self_efficacy <dbl>
```

Vignettes
---------

See examples of use of `prcr` in the [vignettes](https://jrosen48.github.io/prcr/articles/index.html).

Code of Conduct
---------------

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms below. This Code of Conduct is adapted from the Contributor Covenant (<http:contributor-covenant.org>), version 1.0.0, available at <http://contributor-covenant.org/version/1/0/0/>

> As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities. We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, or religion. Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct. Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. Project maintainers who do not follow the Code of Conduct may be removed from the project team. Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting one or more of the project maintainers.
