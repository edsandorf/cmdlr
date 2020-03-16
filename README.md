<!-- badges: start -->
[![Travis build status](https://travis-ci.com/edsandorf/cmdlR.svg?branch=master)](https://travis-ci.com/edsandorf/cmdlR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/cmdlR)](https://cran.r-project.org/package=cmdlR)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/cmdlR)](http://www.r-pkg.org/pkg/cmdlR)
<!-- badges: end -->

# cmdlR: Choice Modeling in R

The problem of choice is fundamental to economics. Choice models are used to understand how people make choices in markets. cmdlR is a set of wrapper functions around a user written log-likelihood function. The package also contain several functions to check for local-optima, calculate welfare measures, compare results and make predictions. To get started, the package contains several examples with pre-programmed log-likelihood functions that can be easily tweaked by the user. 

# Release notes v. 0.0.0.9000
The package is currently not working, but is under development (16.03.2020). 

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


