# tstests 1.0.1

* Added a correction to the conditional value at risk test (CC and CCI) for the zero
failures case which resulted in Inf when taking the logs. 
* Also a correction/warning for the conditional duration test when number of failures
is less than of equal to 1. In this case, the test will return NA.

# tstests 1.0.0

* Initial CRAN submission.
* Bumped version to 1.0.0 from github version 0.3.0.
* Added Hong Li test.
* Added html vignette.
