## cmdlr v0.0.5
* Added a function to standardize variables 
* General bug fixes


## cmdlr v0.0.4
* Major structural changes to the code with multiple changes breaking existing code.
* Code refactoring
* No longer needs a named list of validated options but makes on-the-fly checks prior to estimation at a very slight pre-estimation overhead, but at reduced risk of passing incorrect objects through to the functions.
* Standardized the model object which now has the class 'cmdlr'. This means that the model object has fewer elements and rely on S3 generics for many operations. For example, neither the standard nor robust vcov is available, but can be obtained with the S3 generic for vcov
* Added S3 Generic for vcov
* Added S3 generics for glance() and tidy(), and a placeholder for augment() that are consistent with the 'broom' package. The augment() function currently only returns the model_matrix, but is ready for extensions.
* save_opt() is no longer part of the code. Instead a new function save with arguments is provided. 
* prepare() now returns the estimation environment and not a named list. Furthermore, it can take additional named objects in the ... which are added to the estimation environment. Also works for parallel where the objects are exported in their entirety (no splits on cores). 
* All examples are updated to reflect changes to the overall structure


## cmdlR v0.0.3
* The code has undergone major refactoring and linting, while giving the documentation a much needed update. Previously, some functions relied on scoping rather than explicit passing of arguments. This is now fixed. 
* Added an example with a MDCEV model with an outside good. Tested on Apollo data. 
* The 'check_data' option passed to 'estim_opt' allows the user to bypass 
data checks. Useful for Monte Carlo analysis.
* Added the functions 'rep_row()' and 'rep_col'
* 'N', 'S' and 'nobs' are no longer specified in 'model_opt' but inferred from the data using the data, id- and choice task variables.
* Bug fixes

## cmdlR v0.0.2
* Added hybrid choice model example and a new mixed logit example. 
* Added function 'ordered_logit()'
* Added function 'inspect_list()' to aid with development and exploratory work
* Added option 'calcualte_hessian' to 'estim_opt'. 
* Removed NLOPTR as an optimizer since it cannot use referencing by name 
* Added functionality to analyze choices prior to estimation. 
* alt_availability is no longer specified in the log-likelihood function, but
as the list entry `alt_avail` in `model_opt`. `alt_avail` is also used to work
out `J`, which means that it is a required entry. The change was necessary to allow development of `analyze_choices()`. This change breaks earlier code. 
* The code no longer uses `attach()/detach()` for parameters and data. Breaks all earlier code.
* Removed the need for `summary_opt`. This list of options duplicated information found in other options and complicated maintainance. 
* Options for name and description are moved from `model_opt` to `save_opt` to reduce number of objects passed between functions. 
* Starting value search options are moved from model options to estimation options, and starting value search is no longer called explicitly by the user, but from within `estimate`. 
* The user must now specify the log of the likelihood value and whether to return a positive or negative value (depends on the optimizer). 
* It is now possible to pass data from inside the log-likelihood function along as `attributes()` of the ll value. This change should make it easier to extend functionality in the future. Breaks code prior to development version v.0.0.1.9007
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
