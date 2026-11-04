# outfieldr
outfieldr is an R package that finds optimal outfielder positioning given a set of fielders and a batter. It is currently in development and is not yet ready for production use.

## Get Started
outfieldr can be installed in any R script via devtools:
``` r (devtools)
install.packages('devtools')
devtools::install_github(repo = "larrry27/outfieldr")
```

Once the package is installed, any of its functions can be used. Get a feel for batted ball data with `getBattedBallData` or outfielder statistics with `getFielderStats` before using `optimizeOutfield` to generate optimal outfielder positions.

## Optimal Positioning
The algorithm to calculate optimal outfielder positioning takes the last *n* batted balls by a hitter per field region (left, center, right). Each ball has their own catchable radius for a given fielder, determined by their sprint speed and outfielder jump statistics from [Baseball Savant](https://baseballsavant.mlb.com/). The center of the area with the most overlap of the circles created by these balls and their catch radii constitutes optimal positioning for an outfielder, as it allows them to catch the most balls. There are a few assumptions used:

1. **The left and right fielders cannot pass the spray angle which is covered by center field.** This is currently π/9 in either direction.
2. **A catch is made if an outfielder reaches the batted ball location within .4 seconds of it hitting the ground.**

## Examples

Find the optimal outfielder positioning versus Pete Crow-Armstrong given the White Sox infield.
``` r
optimizeOutfield(691718,803011,671976,695731,200,2026)
```

## Planned Features
outfieldr is currently in development, with the following features to be added:
- Per-park adjustment for outfielder positioning
- Ability to input a projected lineup for a team, with a list of optimal positions returned
- Ability to toggle experimental Arizona Fall League outfield positioning rules

## Authors
outfieldr is maintained by [Larry Donahue](https://www.lbdfour.com) and was originally a submission for Northwestern's MSDS 457 - Sports Management Analytics course. I can be contacted via the links on my website.