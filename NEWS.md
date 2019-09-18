# metamer 0.2.0.9000

## New features 

* Added `fortify()` method to plot metamers directly with 'ggplot2'.

* The `perturbation` argument in `metamerize()` can now be a vector of equal in 
length to `change`

* Multiple minimization functions can be passed to `minimize`.

* Added function `mean_self_proximity()` that computes the inverse of the mean 
minimum distance between each datapoint. Pass it to `minimize` if you don't want 
it to clump.

* Small speed improvement.

# metamer 0.1.0

* First release
* Added a `NEWS.md` file to track changes to the package.
