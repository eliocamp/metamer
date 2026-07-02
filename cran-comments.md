
This submission is to update my email address as I not longer have access to the previous one. 

## R CMD check results

0 errors | 0 warnings | 1 notes

There is one note

  Namespace in Imports field not imported from: ‘progress’
    All declared Imports should be used.

However, this seems to be a false positive, since progress is used in line 203 in R/metamer-class.R
