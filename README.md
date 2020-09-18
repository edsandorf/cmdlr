<!-- badges: start -->
[![Travis build status](https://travis-ci.com/edsandorf/cmdlR.svg?branch=master)](https://travis-ci.com/edsandorf/cmdlR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/cmdlR)](https://cran.r-project.org/package=cmdlR)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/cmdlR)](http://www.r-pkg.org/pkg/cmdlR)
<!-- badges: end -->

# cmdlR: Choice Modeling in R

The problem of choice is fundamental to economics. Choice models are used to understand how people make choices in markets. cmdlR is a set of wrapper functions around a user written log-likelihood function. The package also contain several functions to check for local-optima, calculate welfare measures, compare results and make predictions. To get started, the package contains several examples with pre-programmed log-likelihood functions that can be easily tweaked by the user. 

WARNING: The package is currently under development and while it works, backwards incompatible changes can happen at any point. This warning will be removed when development of the package has stabilized.

# Release notes cmdlR v0.0.2
This release includes the ability to estimate models using the 'ucminf' package along with other code improvements and minor bug fixes. Under the hood, the code now creates an estimation environment and evaluates the log-likelihood function in that context, which avoids the need to make multiple calls to 'attach()' and 'detach()'.

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
 

# How to install cmdlR?

The latest version of the package is available from Github. 

`devtools::install_github("edsandorf/cmdlR")`

# How to contribute?
1. Go to Github and create an account if you don't have one.
2. Fork the project and clone it locally on your computer. Make sure that the repository is synced remotely before you move on to the next step.
3. Create a new branch for each bug fix or new feature you want to add.
4. Do the work and write a descriptive commit message. If you have added a new feature, please contribute documentation and tests. 
5. Push the changes to your remote repository.
6. Create a new pull request for each bug fix or new feature added.
7. Respond to any code review feedback.


This list is based on a great post on [how to contribute](https://akrabat.com/the-beginners-guide-to-contributing-to-a-github-project/) to a github project. 

In order keep everything readable and maintainable, please adhere to the code style. For details, please see the [R chapter](http://r-pkgs.had.co.nz/r.html) of 'R packages' by Hadley Wickham.

# How to cite?

Sandorf, E. D., 2019, cmdlR: Choice Modeling in R, https://github.com/edsandorf/cmdlR


# Acknowledgments
This package is based on years of experience developing and estimating choice
models. A lot of the inspiration for this has come from existing R packages
such as [gmnl](https://CRAN.R-project.org/package=gmnl) and [mlogit](https://CRAN.R-project.org/package=mlogit). I have also benefited from the extensive choice modeling software [Apollo](http://www.apollochoicemodelling.com/) and the excellent [Matlab codes](https://github.com/czaj/DCE) written by Mikolaj Czajkowski. I have benefited greatly from discussions with Danny Campbell, Mikolaj Czajkowski, Stephane Hess and David Palma. The package comes with absolutely no warranty and limited support. 


