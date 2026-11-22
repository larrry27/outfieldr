# outfieldr
outfieldr is an R package that finds optimal outfielder positioning given a set of fielders and a batter. It is currently in development, but is functional. The package is built off of [Bill Petti's baseballr package](https://github.com/BillPetti/baseballr), which pulls data from [Baseball Savant](https://baseballsavant.mlb.com/) to analyze.

## Get Started
outfieldr can currently be installed in any R instance via devtools:

``` r
install.packages('devtools')
devtools::install_github(repo = "larrry27/outfieldr")
```

Once the package is installed, any of its functions can be used. Examples and other details can be found in Getting Started.

## Planned Features
outfieldr is currently in development, with the following features to be added (not an exhaustive list):
- Ability to input a projected lineup for a team, with a list of optimal positions returned (similar to an outfielder's lineup card).
- Ability to toggle experimental Arizona Fall League outfield positioning rules.

## Authors
outfieldr is maintained by [Larry Donahue](https://www.lbdfour.com) and was originally a submission for Northwestern's MSDS 457 - Sports Management Analytics course. I can be contacted via the links on my website.