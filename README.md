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

1. **One outfielder should catch a ball**, so we should prevent overlapping catch radii when possible.
2. **It’s better for the batter to be between the ball and the outfield wall if a catch is not possible**, so they have momentum to run forward with their throw to the infield or don’t have to chase it backwards.
3. **The left and right fielders cannot pass the spray angle which is covered by center field.** This is currently π/12, but is subject to change.

## Examples

Example 1: Find the optimal outfielder positioning versus Kyle Schwarber.
``` r
Once available, code to be populated here.
```

Example 2: Given Tristan Peters in center field, field optimal positioning versus Seiya Suzuki.
``` r
Once available, code to be populated here.
```

## Planned Features
outfieldr is currently in development, with the following features to be added:
- Per-park adjustment for outfielder positioning
- Ability to input a projected lineup for a team, with a list of optimal positions returned
- Ability to toggle experimental Arizona Fall League outfield positioning rules

## Authors
outfieldr is maintained by [Larry Donahue](https://www.lbdfour.com) and was originally a submission for Northwestern's MSDS 457 - Sports Management Analytics course. I can be contacted via the links on my website.