<!-- badges: start -->
[![R build status](https://github.com/edsandorf/cmdlr/workflows/R-CMD-check/badge.svg)](https://github.com/edsandorf/cmdlr/actions)
[![Coverage Status](https://codecov.io/github/edsandorf/cmdlr/coverage.svg?branch=master)](https://codecov.io/github/edsandorf/cmdlr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/cmdlR)](https://cran.r-project.org/package=cmdlr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/cmdlR)](http://www.r-pkg.org/pkg/cmdlr)
<!-- badges: end -->

# cmdlR: Choice Modeling in R

The problem of choice is fundamental to economics. Choice models are used to understand how people make choices in markets. `cmdlr` is a set of wrapper functions around a user written log-likelihood function. 

To make it easier for new choice modelers to get started with writing their own log-likelihood functions, the package comes with several applied examples that can be easily tweaked by the user.

WARNING: The package is currently under development and while it works, backwards incompatible changes can happen at any point. This warning will be removed when development of the package has stabilized.

[See the NEWS for a current list of changes (breaking or not)](NEWS.md)

# How to install cmdlR?

The latest version of the package is available from Github. 

`devtools::install_github("edsandorf/cmdlr")`

# How to cite?

Run `citation("cmdlr")` to see citation information.

# Getting help
The package does not have its own help forum nor do I provide much in terms of e-mail support. I do try to document each function sufficiently, provide relevant examples and publish articles and vignettes detailing functionality and use to the package website: ([https://cmdlR.edsandorf.me](https://cmdlR.edsandorf.me)).

# Reporting a bug 
All software contain bugs and this R package is no different. I do my best to root them out, but sometimes they can be persistent and difficult to spot. If you do come across one, please report it through the package's GitHub repository. Label the issue as a "bug". Please be detailed and include a reproducible example. 

# Requesting a feature
The normal development cycle of the package depends on the needs of my current projects. I will usually develop new functions and add them to the package as needed. However, if the package is missing an essential feature, please feel free to let me know. To help me track feature requests and allow others to comment on them, please make suggestions using the package's Github repository and label the issue as a "feature request". The more detail you add with the request, the easier it will be to understand and implement.

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


# Acknowledgments
This package is based on years of experience developing and estimating choice
models. The package was developed under the Marie Sklodowska-Curie Action INSPiRE under the European Union's Horizon 2020 Research and INnovation Program Grant Agreement No 793163. A lot of the inspiration for this has come from existing R packages
such as [gmnl](https://CRAN.R-project.org/package=gmnl) and [mlogit](https://CRAN.R-project.org/package=mlogit). I have also benefited from the extensive choice modeling software [Apollo](http://www.apollochoicemodelling.com/) and the excellent [Matlab codes](https://github.com/czaj/DCE) written by Mikolaj Czajkowski. I have benefited greatly from discussions with Danny Campbell, Mikolaj Czajkowski, Stephane Hess and David Palma. The package comes with absolutely no warranty and limited support. 
