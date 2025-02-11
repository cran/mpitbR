
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpitbR: A Toolbox for Calculating Multidimensional Poverty Indices in R

<img src="man/figures/hex_sticker_mpitbR2.png" align="right" height="139"/>

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/mpitbR?color=orange)](https://cran.r-project.org/package=mpitbR)
[![R build
status](https://github.com/girelaignacio/mpitbR/workflows/R-CMD-check/badge.svg)](https://github.com/girelaignacio/mpitbR/actions)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/mpitbR)](https://cran.r-project.org/package=mpitbR)

<!-- badges: end -->

Here it is provided a package for estimating multidimensional poverty
measures based on the Alkire-Foster method which mirrors the estimation
procedures of the original
<tt>[mpitb](https://doi.org/10.1177/1536867X231195286)</tt> Stata
package.

### Installation

You can install the development version of mpitbR from
[GitHub](https://github.com/girelaignacio/mpitbR) or installing it from
CRAN.

``` r
# Install the package from CRAN
install.packages("mpitbR")

# Install the latest version of the package from Github
devtools::install_github("girelaignacio/mpitbR")
```

### Usage

A short example:

``` r
# Here we use the same synthetic household survey-like dataset from the Stata package example
data <- subset(syn_cdta)
data <- na.omit(data)

# Define the survey structure
svydata <- survey::svydesign(id=~psu, weights = ~weight, strata = ~stratum, data = data)

# mpitb set command
  # First we define the indicators with their dimensions names (d1,d2,d3) as a list 
indicators <- list(d1 = c("d_nutr","d_cm"),
                   d2 = c("d_satt","d_educ"),
                   d3 = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))
  # Set the multidimensional poverty measurement project
set <- mpitb.set(svydata, indicators = indicators, name = "myname", desc = "pref. desc")

# mpitb est command
  # Estimate! 
est <- mpitb.est(set, c(20, 33), over = c("area","region"), 
                 tvar = "t", cotyear = "year")
```

### Citation

Please cite this package if used in publications:

Girela, Ignacio (2024). *mpitbR: Calculate Alkire-Foster
Multidimensional Poverty Measures*. R package version 1.0.0,
<https://CRAN.R-project.org/package=mpitbR>.

A BibTeX entry for LaTeX users is

``` r
@Manual{mpitbRpkg,
    title = {mpitbR: Calculate Alkire-Foster Multidimensional Poverty Measures},
    author = {Ignacio Girela},
    year = {2024},
    note = {R package version 1.0.0},
    url = {https://CRAN.R-project.org/package=mpitbR},
  }
```

### License

This project is licensed under the [GPL (\>=
3)](https://www.fsf.org/news/gplv3_launched)

### Documentation

For more detailed information, please refer to the documentation

**IMPORTANT**: A more detailed vignette on the usage of the package is
under construction. Meanwhile, for exploring the package’s basic
functionalities, you can refer to this submitted [draft
document](https://www.researchgate.net/publication/380394221_mpitbR_a_Toolbox_for_Measuring_Multidimensional_Poverty_Indices_in_R),
which has not undergone peer review

### Contributing

Contributions are welcome! Please open an issue or submit a pull request

### Contact

Please, do not hesitate to contact me!

Ignacio Girela (📧 <ignacio.girela@unc.edu.ar>)
