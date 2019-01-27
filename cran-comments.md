## Test environments
* local OS X install, R 3.5.1
* ubuntu 16.04, GCC
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Note
To my knowledge, this is the first R adaptation of NSGA III algorithm.
Since NSGA III is a wrapper feature selection algorithm, 
it requires retraining the model n times on each iteration.
Due to this running example might slightly exceed 5s limit. 
