
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RQDAassist

<!-- badges: start -->

[![R-CMD-check](https://github.com/BroVic/RQDAassist/workflows/R-CMD-check/badge.svg)](https://github.com/BroVic/RQDAassist/actions)

<!-- badges: end -->

[RQDA](http://rqda.r-forge.r-project.org/), a powerful computer-aided
qualitative data analysis (CAQDAS) package, can be slightly challenging
to install. This is particularly so for researchers who are particularly
skilled at R programming.

Since the advent of R-4.0, RQDA was archived due to various changes in
the dependency ecosystem. The package remains useful to many but
installing it through the regular pathway – `install.packages` – is not
currently available.

The main purpose for this package is to aid RQDA users who still depend
on it for their work.

## Installation

This package is strictly *off-CRAN* and can be installed from
[GitHub](https://github.com/BroVic/RQDAassist) with the following code:

``` r
# install.packages("remotes")
remotes::install_github("BroVic/RQDAassist")
```

## Usage

### Installing RQDA

To install RQDA, simply run this line of code:

``` r
RQDAassist::install()
```

#### Binary install

For the default installation, two (2) of the RQDA dependencies will be
installed as binaries, while the rest will be build from their
respective source tarballs. Note that this default option is most
suitable **for interactive sessions**. This is because, after the
installation of RGtk2 binaries, installation will stop to allow for the
installation of Gtk+. To activate the prompt for Gtk+, the user has to
attach RGtk2 as follows:

``` r
library(RGtk2)
```

The user will then be prompted with a dialog to install Gtk+. Once that
is accomplished, run `RQDAassist::install()` again to complete the rest
of the installation.

#### Source install

The source installation should be the choice if the `install` function
is to be used non-interactively. If the package **RGtk2** needs to be
installed, its dependencies i.e. the GTK distribution will first be
downloaded, its static libraries and header files placed on `PATH` and
then RGtk2 will be compiled from source. The user should note that this
will take a while; when the installation gets to this point for RGtk2,

    * byte-compile and prepare package for lazy loading

it may look as if the process is frozen. Kindly wait and **do not
interrupt the program at this point!**

### Additional functionality

Although this package is useful for just installing RQDA, there are
other functions that exist to make life easier when working with RQDA
projects. For more details, kindly consult the documentation via
`help(package = "RQDAassist")` for an overview of the available
functions.

## System Requirements

To build source packages in R,
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is required.
Ultimately, *RQDAassist* needs to work on the major platforms
i.e. Windows, Linux and Mac, but for now has only been tested on Windows
10 x64. Users of other OSes are welcome to try it out and let us know
what does not work so it can be improved upon.

## Contributions/Feedback

If you run into problems, kindly post an issue and expect a quick
response. Feature requests and ontributions are also welcome.
