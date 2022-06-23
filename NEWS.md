# metamer (development version)

# metamer 0.3.0

## Breaking chances

This version overhauls the code and syntax. 
It makes it pipe friendly and includes new arguments and changes some of the existing ones. 

## New features

- The new `stop_if` argument replaces the old `N` argument. 
This allows for more flexible and complex stopping criteria than just the number of iterations.
See `?stop_conditions` for a list of implemented functions. 

- The new `keep` argument replaces the old `trim` argument.


(see `?stop_conditions`).

# metamer 0.2.0

## New features 

* Added `fortify()` method to plot metamers directly with 'ggplot2'.

* The `perturbation` argument in `metamerize()` can now be a vector of equal in 
length to `change`

* Multiple minimization functions can be passed to `minimize`.

* Added function `mean_self_proximity()` that computes the inverse of the mean 
minimum distance between each datapoint. Pass it to `minimize` if you don't want 
it to clump.

* Small speed improvement.

* Tweaked annealing algorithm.

# metamer 0.1.0

* First release
* Added a `NEWS.md` file to track changes to the package.
