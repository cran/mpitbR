
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpitbR: A Toolbox for Calculating Multidimensional Poverty Indices in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/girelaignacio/mpitbR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/girelaignacio/mpitbR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Here it is provided a package for estimating multidimensional poverty
measures based on the Alkire-Foster method which mirrors the estimation
procedures of the original mpitb Stata package.

## Installation

You can install the development version of mpitbR from
[GitHub](https://github.com/girelaignacio/mpitbR) or installing it from
CRAN.

``` r
# If the package is not available in CRAN you can install it from GitHub!
# From CRAN
if (!require("mpitbR", character.only = TRUE)) {
  install.packages("mpitbR")
}
#> Loading required package: mpitbR
# From Github
if (!require("mpitbR", character.only = TRUE)) {
  devtools::install_github("girelaignacio/mpitbR")
}
```

This process may take time the first time.

## A short example

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
system.time(
est <- mpitb.est(set, c(20, 33), over = c("area","region"), 
                 tvar = "t", cotyear = "year")
)             
#>         ****** SPECIFICATION ******
#> Call:
#> mpitb.est.mpitb_set(set = set, klist = c(20, 33), over = c("area", 
#>     "region"), cotyear = "year", tvar = "t")
#> Name:  myname 
#> Weighting scheme:  equal 
#> Description:  pref. desc 
#> ___________________
#>                                                                      
#> Dimension 1:  d1 0.333                                 (d_nutr, d_cm)
#> Dimension 2:  d2 0.333                               (d_satt, d_educ)
#> Dimension 3:  d3 0.333 (d_elct, d_sani, d_wtr, d_hsg, d_ckfl, d_asst)
#> ___________________
#>                             
#> Indicator 1:   d_nutr 0.1667
#> Indicator 2:     d_cm 0.1667
#> Indicator 3:   d_satt 0.1667
#> Indicator 4:   d_educ 0.1667
#> Indicator 5:   d_elct 0.0556
#> Indicator 6:   d_sani 0.0556
#> Indicator 7:    d_wtr 0.0556
#> Indicator 8:    d_hsg 0.0556
#> Indicator 9:   d_ckfl 0.0556
#> Indicator 10:  d_asst 0.0556
#> 
#>         ****** ESTIMATION ******
#> ___________________
#> Partial AF measures: ' M0 H A ' under estimation... DONE
#> 
#> ___________________
#> Indicator-specific measures: ' hd hdk actb pctb ' under estimation... DONE
#> 
#> ___________________
#> Estimate changes over time over ' M0 H A hd hdk ' measures... DONE
#> 
#>         ****** RESULTS ******
#> ___________________
#> Parameters
#> Number of time periods:  2 
#> Subgroups:  3 
#> Poverty cut-offs (k):  2 
#> 
#> *Notes: 
#>   Confidence level: 95 %
#>   Parallel estimations:  FALSE
#>    user  system elapsed 
#>   45.06    1.00   97.00
```

## Some comparisons with the original Stata package

The equivalent command of the previous example in the <tt>mpitb</tt>
Stata package is the following:

> mpitb est, name(trial01) measures(all) /// indmeasures(all) aux(all)
> /// klist(20 33) weight(equal) /// lframe(myresults, replace) /// svy
> over(area region) /// cotmeasures(all) cotframe(mycot, replace)
> tvar(t) cotyear(year)

Here below, we compare the results from the Stata package and this
version in R. We merged both data frames with the results (point
estimate, standard errors, and confidence intervals). We calculate the
L1 distance between the estimates ($|\theta_{R}-\theta_{Stata}|$) by
measure in the case of the “lframe” results and by ctype and annualized
measure in the case of the “cotframe” results.

The following output shows the summary of the distribution of this
comparisons (suffix “.x” refers to the estimates of the R package and
“.y” to the Stata version).

``` r
# "lframe" COMPARISONS
aggregate(abs(b.x - b.y)~ measure, merged, summary)
#>   measure abs(b.x - b.y).Min. abs(b.x - b.y).1st Qu. abs(b.x - b.y).Median
#> 1       A        2.537193e-10           2.496580e-09          6.505966e-09
#> 2    actb        2.573115e-14           6.975318e-11          1.892713e-10
#> 3       H        1.972034e-13           3.192835e-09          9.451868e-09
#> 4      hd        5.355799e-12           1.776457e-09          4.280438e-09
#> 5     hdk        2.442491e-15           1.071102e-09          2.483521e-09
#> 6      M0        1.436365e-11           1.280760e-09          2.683751e-09
#> 7    pctb        2.979159e-12           7.875736e-10          1.820007e-09
#>   abs(b.x - b.y).Mean abs(b.x - b.y).3rd Qu. abs(b.x - b.y).Max.
#> 1        8.427761e-09           1.304837e-08        2.210272e-08
#> 2        4.151699e-10           4.673255e-10        3.131624e-09
#> 3        1.100592e-08           1.678608e-08        2.963083e-08
#> 4        5.174864e-09           7.316088e-09        1.486417e-08
#> 5        3.248355e-09           4.859661e-09        1.471275e-08
#> 6        3.275439e-09           4.787983e-09        1.169818e-08
#> 7        3.267782e-09           3.926226e-09        2.626389e-08
aggregate(abs(se.x - se.y)~ measure, merged, summary)
#>   measure abs(se.x - se.y).Min. abs(se.x - se.y).1st Qu.
#> 1       A          5.416848e-13             5.584464e-11
#> 2       H          1.160892e-11             1.849522e-10
#> 3      hd          6.822338e-13             1.575026e-10
#> 4     hdk          5.614433e-15             1.316371e-10
#> 5      M0          1.465078e-13             6.681607e-11
#>   abs(se.x - se.y).Median abs(se.x - se.y).Mean abs(se.x - se.y).3rd Qu.
#> 1            1.480838e-10          1.902079e-10             2.567154e-10
#> 2            4.395547e-10          4.371655e-10             6.625546e-10
#> 3            3.627557e-10          4.065640e-10             6.459523e-10
#> 4            2.777703e-10          3.432593e-10             4.983902e-10
#> 5            1.752028e-10          2.275738e-10             3.695835e-10
#>   abs(se.x - se.y).Max.
#> 1          6.804142e-10
#> 2          1.160708e-09
#> 3          1.563327e-09
#> 4          1.832390e-09
#> 5          6.184998e-10
aggregate(abs(ll.x - ll.y)~ measure, merged, summary)
#>   measure abs(ll.x - ll.y).Min. abs(ll.x - ll.y).1st Qu.
#> 1       A          2.891913e-10             4.516447e-09
#> 2       H          3.786096e-11             3.067251e-09
#> 3      hd          2.317469e-12             1.400088e-09
#> 4     hdk          2.370888e-12             8.662990e-10
#> 5      M0          3.895412e-12             1.577318e-09
#>   abs(ll.x - ll.y).Median abs(ll.x - ll.y).Mean abs(ll.x - ll.y).3rd Qu.
#> 1            7.651098e-09          9.340626e-09             1.471136e-08
#> 2            7.146682e-09          9.442070e-09             1.297974e-08
#> 3            3.383367e-09          4.334191e-09             6.363083e-09
#> 4            1.933710e-09          2.616787e-09             3.715519e-09
#> 5            2.832272e-09          3.768499e-09             5.334923e-09
#>   abs(ll.x - ll.y).Max.
#> 1          2.192765e-08
#> 2          2.958044e-08
#> 3          1.482086e-08
#> 4          1.457998e-08
#> 5          1.119702e-08
aggregate(abs(ul.x - ul.y)~ measure, merged, summary)
#>   measure abs(ul.x - ul.y).Min. abs(ul.x - ul.y).1st Qu.
#> 1       A          1.068123e-10             3.338948e-09
#> 2       H          2.997895e-10             3.667333e-09
#> 3      hd          1.238520e-11             2.432907e-09
#> 4     hdk          2.556844e-12             1.451214e-09
#> 5      M0          4.180370e-11             1.734692e-09
#>   abs(ul.x - ul.y).Median abs(ul.x - ul.y).Mean abs(ul.x - ul.y).3rd Qu.
#> 1            9.316736e-09          1.024661e-08             1.643490e-08
#> 2            7.834429e-09          9.890734e-09             1.367126e-08
#> 3            5.388588e-09          6.273871e-09             9.840444e-09
#> 4            3.231405e-09          4.022657e-09             5.815440e-09
#> 5            2.776975e-09          3.904529e-09             5.206159e-09
#>   abs(ul.x - ul.y).Max.
#> 1          2.196234e-08
#> 2          2.897404e-08
#> 3          2.735358e-08
#> 4          1.487718e-08
#> 5          1.141741e-08
# "cotframe" COMPARISONS
aggregate(abs(b.x - b.y)~ ctype+ann, cot.merged, summary)
#>   ctype ann abs(b.x - b.y).Min. abs(b.x - b.y).1st Qu. abs(b.x - b.y).Median
#> 1   abs   0        1.870067e-12           3.408458e-10          8.934434e-10
#> 2   rel   0        0.000000e+00           1.760384e-07          4.433030e-07
#> 3   abs   1        1.213274e-13           3.914198e-11          1.023916e-10
#> 4   rel   1        6.494183e-11           2.521504e-08          5.924641e-08
#>   abs(b.x - b.y).Mean abs(b.x - b.y).3rd Qu. abs(b.x - b.y).Max.
#> 1        1.223757e-09           1.741895e-09        7.372153e-09
#> 2        5.706635e-07           8.288004e-07        3.124419e-06
#> 3        1.409919e-10           1.976471e-10        9.089043e-10
#> 4        8.606367e-08           1.148786e-07        4.512245e-07
aggregate(abs(se.x - se.y)~ ctype+ann, cot.merged, summary)
#>   ctype ann abs(se.x - se.y).Min. abs(se.x - se.y).1st Qu.
#> 1   abs   0          1.005595e-12             1.705410e-10
#> 2   rel   0          1.336629e-10             7.556717e-08
#> 3   abs   1          5.540273e-14             1.943348e-11
#> 4   rel   1          6.217767e-03             2.685340e-01
#>   abs(se.x - se.y).Median abs(se.x - se.y).Mean abs(se.x - se.y).3rd Qu.
#> 1            3.957626e-10          4.869718e-10             7.045401e-10
#> 2            1.794390e-07          2.308181e-07             3.476520e-07
#> 3            4.806957e-11          5.777346e-11             8.246320e-11
#> 4            5.255512e-01          6.358943e-01             8.650374e-01
#>   abs(se.x - se.y).Max.
#> 1          1.854281e-09
#> 2          1.575077e-06
#> 3          2.303284e-10
#> 4          2.765468e+00
aggregate(abs(ll.x - ll.y)~ ctype+ann, cot.merged, summary)
#>   ctype ann abs(ll.x - ll.y).Min. abs(ll.x - ll.y).1st Qu.
#> 1   abs   0          8.942084e-06             5.865915e-05
#> 2   rel   0          9.729035e-10             4.001384e-07
#> 3   abs   1          1.052010e-06             6.901110e-06
#> 4   rel   1          1.218661e-02             5.263171e-01
#>   abs(ll.x - ll.y).Median abs(ll.x - ll.y).Mean abs(ll.x - ll.y).3rd Qu.
#> 1            9.840598e-05          9.207851e-05             1.216658e-04
#> 2            8.386673e-07          1.015672e-06             1.476080e-06
#> 3            1.157778e-05          1.083278e-05             1.431329e-05
#> 4            1.030061e+00          1.246330e+00             1.695442e+00
#>   abs(ll.x - ll.y).Max.
#> 1          2.013519e-04
#> 2          3.728481e-06
#> 3          2.368856e-05
#> 4          5.420217e+00
aggregate(abs(ul.x - ul.y)~ ctype+ann, cot.merged, summary)
#>   ctype ann abs(ul.x - ul.y).Min. abs(ul.x - ul.y).1st Qu.
#> 1   abs   0          8.941498e-06             5.865930e-05
#> 2   rel   0          3.731699e-10             8.543615e-08
#> 3   abs   1          1.051955e-06             6.901133e-06
#> 4   rel   1          1.218664e-02             5.263170e-01
#>   abs(ul.x - ul.y).Median abs(ul.x - ul.y).Mean abs(ul.x - ul.y).3rd Qu.
#> 1            9.840926e-05          9.207839e-05             1.216645e-04
#> 2            2.286018e-07          3.617387e-07             4.645883e-07
#> 3            1.157759e-05          1.083275e-05             1.431344e-05
#> 4            1.030061e+00          1.246330e+00             1.695442e+00
#>   abs(ul.x - ul.y).Max.
#> 1          2.013484e-04
#> 2          3.679716e-06
#> 3          2.368807e-05
#> 4          5.420217e+00
```

All the results are very close with some exceptions found in few
estimates of the standard errors of the relative changes over time
measures. Since the point estimates (“b”) are close and Delta method is
used for calculating the standard errors, here there may be some
differences between the “survey” library in R and Stata

------------------------------------------------------------------------
