
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RQDAassist

<!-- badges: start -->

[![R-CMD-check](https://github.com/BroVic/RQDAassist/workflows/R-CMD-check/badge.svg)](https://github.com/BroVic/RQDAassist/actions)

<!-- badges: end -->

{[RQDA](http://rqda.r-forge.r-project.org/)}, a powerful computer-aided
qualitative data analysis (CAQDAS) package, can be slightly challenging
to install. This is particularly so for researchers who are particularly
skilled at R programming.

Since the release of R 4.0, {RQDA} was archived due to various changes
in its dependency ecosystem. The package remains useful to many but
installing it through the regular pathway i.e. `install.packages` is
currently not available.

The main purpose of this package is to aid RQDA users who still depend
on it for their work.

***Note: Since the introduction of
[UCRT](https://blog.r-project.org/2021/12/07/upcoming-changes-in-r-4.2-on-windows/)-based
R builds (starting from R 4.2) the RQDA dependencies cannot be built
with the current toolset. Please use R 4.0 to 4.1***.

## Installation

For obvious reasons, this package cannot be published on CRAN. It can be
installed from [GitHub](https://github.com/BroVic/RQDAassist) with the
following code:

``` r
# install.packages("remotes")
remotes::install_github("BroVic/RQDAassist")
```

## Usage

### Installing RQDA

To install {RQDA}, simply run this line of code:

``` r
RQDAassist::install()
```

#### Binary install

For the default installation, two (2) of the {RQDA} dependencies will be
installed as binaries, while the rest will be built from their
respective source tarballs. The 2 packages are {igraph} and {RGtk2}; the
former is obtained from CRAN while the latter comes from
[Togaware](https://rattle.togaware.com/), the creators of the {Rattle}
package.

#### Source install

The source installation could be the choice if the user cannot (or does
not want to) install the binary from Togaware. The user should note that
for {RGtk2}, this will take a while; when the installation gets to this
point for {RGtk2},

    * byte-compile and prepare package for lazy loading

it may look as if the process is frozen. Kindly wait and **do not
interrupt the program at this point!**

### Additional functionality

Although this package is useful for installing {RQDA}, there are other
functions that exist to make life easier when working with {RQDA}
projects. For more details, kindly consult the documentation via
`help(package = "RQDAassist")` for an overview of the available
functions.

## System Requirements

On Windows, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is
required to install {RQDA} and its dependencies. Ultimately,
{RQDAassist} needs to work on the other major platforms i.e. Linux and
Mac, but for now has only been tested on Windows 10 x64. Users of these
other operating systems are welcome to try it out and let us know what
does not work so it can be improved upon.

## Contributions/Feedback

If you run into problems, kindly post an issue and expect a quick
response. Feature requests and contributions are also welcome.
