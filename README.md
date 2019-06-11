# cmdlR: Choice modeling in R

This package contains supplementary functions to handle data input, setup
for multicore estimation and several pre-programmed likelihood functions that 
will quickly get you analyzing your data.

This package is written to work together with my other package (gizmo)[https://github.com/edsandorf/gizmo], which contains a collection of useful functions for statistical analysis and choice modeling. 

# How to install cmdlR?

The latest version of the package is available from Github. 

`devtools::install_github("edsandorf/cmdlR")`

If you want the
bleeding edge, you can download the development branch of the package. However,
the development branch may be more susceptible to bugs. 

# How to contribute to cmdlR?

## The process of contributing
1. Go to Github and create an account if you don't have one.
2. Fork the project and clone it locally on your computer. Make sure that it is synced to the remote repository before you move on to step 3.
3. Create a new branch for each separate piece of work, e.g. a bug fix or new feature (see below).
4. Do the work and write a descriptive commit message. If you have added a new feature, please also write documentation and tests.
5. Push your changes to your original Github repository.
6. Create a new pull request in Github.
7. Respond to any code review feedback.

This list is based on a great post on [how to contribute](https://akrabat.com/the-beginners-guide-to-contributing-to-a-github-project/) to a github project. 

##  Generally about pull requests
While I greatly appreciate input and bug fixes to the code, in order keep everything readable, please adhere to the code style.

If you have more than one bug fix or want to add more than one feature, please submit multiple pull requests. A rule of thumb is one pull request per fix or addition. 

If you are adding a new feature or function, please also submit documentation and preferably a test file. This will make it much easier to include with the code. 

# How to cite the use of cmdlR?


# Acknowledgements
This package is based on years of experience developing and estimating choice
models. A lot of the inspiration for this has come from existing R packages
such as [gmnl](https://CRAN.R-project.org/package=gmnl) and [mlogit](https://CRAN.R-project.org/package=mlogit). I have also benefited from the extensive choice modeling software [Apollo](http://www.apollochoicemodelling.com/) and the excellent [Matlab codes](https://github.com/czaj/DCE) written by Mikolaj Czajkowski. I have benefited greatly from discussions with Danny Campbell, Mikolaj Czajkowski, Stephane Hess and David Palma.


