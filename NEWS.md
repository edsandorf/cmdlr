## cmdlR v0.0.2
* Starting value search options are moved from model options to estimation options.
* The user must now specify the log of the likelihood value and whether to return a positive or negative value (depends on the optimizer). It is now possible to pass data from inside the log-likelihood function along as `attributes()` of the ll value. This change should make it easier to extend functionality in the future. Breaks code prior to development version v.0.0.1.9007
* `attach()` data and draws is no longer necessary inside of the log-likelihood function.
* Took dependence on 'rlang' to make work with environments and expression evaluation easier.
* Fixed a bug where `ids` in `split_data()` was not always a vector, which caused `sort()` to fail.
* Various minor bug fixes


## cmdlR v0.0.1
* Added a simple search for starting values function
* Added functions to calculate the standard errors of variances, covariances, correlations and standard deviations of variance-covariance matrix of random parameters in a MIXL model
* Can specify paramters to stay fixed at the starting values using model_opt$fixed
* Added convenient function store() governed by the list save_opt() to save results and model objects
* Added working LC example
* Added working MIXL example
* Added working MNL example
* Added functions to create random draws and a convenient wrapper function
make_random_draws()
* Added the test datasets data_coral.rds and data_petr_test.rds
