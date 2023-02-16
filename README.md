
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RQDAassist

<!-- badges: start -->

[![R-CMD-check](https://github.com/BroVic/RQDAassist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BroVic/RQDAassist/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

{[RQDA](http://rqda.r-forge.r-project.org/)}, a powerful computer-aided
qualitative data analysis (CAQDAS) package, can be slightly challenging
to install. This is particularly so for researchers who are not
well-versed in managing R’s tool sets and software extensions.

Upon the release of R 4.0, {RQDA} was archived due to various changes in
the dependency ecosystem. The package remains useful to many but
installing it through the regular pathway i.e. `install.packages` will
not be available until the issues that led to its archiving are
resolved.

This package was inspired by an [initial
script](https://gist.github.com/BroVic/7771e1e86df35f6410a3f586ea1ef6c6)
that was written to provide installation help to RQDA users who still
depend on it for their work.

## Installation

For obvious reasons, this package cannot be published on CRAN. It can be
installed from [GitHub](https://github.com/BroVic/RQDAassist) with:

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

For the default installation, two of the {RQDA} dependencies, {igraph}
and {RGtk2}, will be installed as binaries; the former is obtained from
CRAN while the latter comes from
[Togaware](https://rattle.togaware.com/), creators of the {Rattle}
package. The rest will be built from their respective source packages.

#### Source install

A source installation could be the choice if the user cannot (or does
not want to) install the binary from Togaware. Note that for {RGtk2},
this will take quite a while; when the installation gets to this point,

    * byte-compile and prepare package for lazy loading

it may appear as if the process is frozen. Kindly wait and **do not
interrupt the program at this point**.

#### R and Rtools requirement

Since the introduction of
[UCRT](https://blog.r-project.org/2021/12/07/upcoming-changes-in-r-4.2-on-windows/)-based
R builds (starting from R 4.2) the RQDA dependencies cannot be built
with the current R and Rtools42 (on Windows). Only R versions 4.0 to 4.1
are compatible; when these are not found on the system,
`RQDAassist::install` will attempt to download R 4.1.3 from CRAN as well
as Rtools40. This, however, will only work properly in interactive R
sessions because of the need for the user to confirm installation
permissions via the Windows SmartScreen technology.

### Additional functionality

Although this package is useful for installing {RQDA}, there are other
functions that exist to make life easier when working with {RQDA}
projects. For more details, kindly consult the documentation via
`help(package = "RQDAassist")` for an overview of the available
functions.

## Troubleshooting

For the RQDA installation, several steps are involved and it’s not
unusual for it to fail for a number of reasons e.g. when there is poor
internet access. To better identify failure points, the user can apply
additional verbosity to the installation

``` r
RQDAassist::install(verbose = TRUE)
```

A lot of effort has been put into providing informative output when
things do go wrong.

## Contributions/Feedback

If you run into problems, kindly post an issue and expect a quick
response. Feature requests and contributions are also welcome.
