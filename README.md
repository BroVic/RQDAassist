
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RQDAassist

<!-- badges: start -->

[![R-CMD-check](https://github.com/BroVic/RQDAassist/workflows/R-CMD-check/badge.svg)](https://github.com/BroVic/RQDAassist/actions)
<!-- badges: end -->

The powerful computer-aided qualitative data analysis (CAQDAS) package
[RQDA](http://rqda.r-forge.r-project.org/) can, even on a good day, be
slightly challenging to install. This is particularly so for researchers
who are not that much into the programming side of things.

Additionally, since the advent of R-4.0, RQDA was archived due to the
upgrade of some of its dependencies. It still works with the new version
of R, but installing it through the regular pathway,`install.packages()`
is currently not available.

The goal of this package is to help users overcome this challenge.

## Installation

This package is currently under development and can be installed in an R
session from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BroVic/RQDAassist")
```

## To install RQDA

To install RQDA, after installing this package simply run this line of
code:

``` r
RQDAassist::install()
```

This will start the process of fetching the packages that RQDA needs to
be installed and work properly. If the package **RGtk2** needs to be
installed, **its** dependencies will first be downloaded and the package
will be compiled from source. This process takes a while, so be patient!

## System Requirements

Ultimately, this package needs to work on Window, Linux and Mac. But at
the time of writing, it has only been tested on Windows 10 x64. You are
welcome to test it on other platforms and let me know what does not work
so we can improve it.

## Contributions/Feedback

This is an experimental package and thus feedback is welcome. If you run
into problems, kindly post an issue. Contributions are also welcome.
