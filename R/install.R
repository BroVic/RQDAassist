#' Install RQDA Archive
#'
#' Install RQDA from CRAN archives and also while performing an installation of
#' its core dependencies namely, RGtk2, gWidgets, and gWidgetsRGtk2.
#'
#' @details The \code{install} function carries out wholesale installation of
#' all the packages required by RQDA, before actually installing it.
#'
#' \code{install_rgtk2_and_deps()} installs RGtk2. Before doing so, it fetches
#' the Gtk+ distribution, making it available for RGtk2 compilation and
#' subsequent usage as a package.
#'
#' @importFrom devtools has_devel
#' @importFrom devtools install_version
#' @importFrom purrr iwalk
#' @importFrom utils install.packages
#'
#' @param verbose logical.
#'
#' @export
install <- function(verbose = TRUE)
{
  # Message prints only once after package is loaded (see zzz.R)
  if (!as.logical(Sys.getenv("RQDA_ASST_HAS_RUN_INSTALL"))) {
    Sys.setenv(RQDA_ASST_HAS_RUN_INSTALL = TRUE)
    cat("You are about to install archived versions of RQDA and its dependencies.\n")
    ans <-
      readline("Hit <ENTER> to continue or 'q' to quit: ")
    if (tolower(ans) == 'q')
      return(invisible())
  }

  .install_init(c('cairoDevice', "igraph"), verbose)

  .check_buildtools()

  if (!.rgtk2IsInstalled() && .Platform$OS.type != 'windows') {
    warning(
      "Automatic compilation/installation of RGtk2 is only available on Windows"
    )
    return()
  }

  install_rgtk2_and_deps()

  iwalk(
    c(
      gWidgets = '0.0-54.2',
      gWidgetsRGtk2 = '0.0-86',
      RQDA = '0.3-1'
    ),
    installEachPackageByVersion,
    verbose = verbose
  )
}






# Internal functions ----

## Provides the index to CRAN directory
cran.index <- function() {
  c("https://cran.r-project.org")
}

## Returns the address to RStudio's CRAN mirror
rstudio <- function() {
  c('https://cran.rstudio.com')
}




.localGtkPath <- function() {
  gtkdir <- file.path(.libPaths()[1], 'RGtk2', 'gtk')
  file.path(gtkdir, .Platform$r_arch)
}



## Installs initial packages required by the script.
## What makes these ones special is that they are
## current package versions from CRAN and they are
## downloaded as binaries.
## @param cranry A character vector of packages.
.install_init <- function(cranbry, verbose) {
  stopifnot(is.character(cranbry))
  tryCatch({
    notInstalled <-
      cranbry[!cranbry %in% .packages(all.available = TRUE)]
    install.packages(notInstalled,
                     repos = rstudio(),
                     quiet = !verbose)
  }, error = function(e) {
    stop(sprintf(
      "Initialization failed. Install %s",
      paste(cranbry, collapse = ', ')
    ))
  })
}



## Checks the availability of Rtools on Windows
.check_buildtools <- function() {
  if (!devtools::has_devel()) {
    if (.Platform$OS.type == 'windows') {
      toolsUrl <-
        file.path(cran.index(),
                  "bin",
                  .Platform$OS.type,
                  "Rtools/history.html")
      errBuildtools <-
        sprintf("Build tools were not found. Please visit %s to install.",
                toolsUrl)
      stop(errBuildtools, call. = TRUE)
    }
  }
}




# Custom error condition for RGtk2
.abortRgtk2 <- function() {
  msg <- sprintf("Could not install RGtk2. Try doing so in R console")
  stop(msg, call. = FALSE)
}






## Controls how each package is installed
## @param name Name of the package
## @param ver The package version
installEachPackageByVersion <- function(ver, name, verbose) {
  stopifnot({
    is.character(ver)
    is.character(name) && length(name) == 1L
    is.logical(verbose)
  })

  pkgExists <- quote(name %in% .packages(all.available = TRUE))

  ## Avoid repeat installations via an early return
  ## If we're dealing with RGtk2, just stop the script
  ## and install Gtk+ interactively, if it is required.
  if (eval(pkgExists)) {
    message(sQuote(name), " is already installed")
    return()
  }

  ## Grab the package archives as desired.
  ## When installing them, the following are considered:
  ##   => asking for upgrade interrupts installation
  ##   => install only 64-bit compatible builds
  ## But first, if RGtk2 is not present, there's no
  ## point trying to install packages that depend on it.
  dependsOnRgtk2 <- (name == 'gWidgetsRGtk2' || name == 'RQDA')
  rgtk2NotReady <-
    !(.rgtk2IsInstalled() && dir.exists(.localGtkPath()))
  if (dependsOnRgtk2 && rgtk2NotReady) {
    message(sQuote(name), " was not installed because RGtk2 is not ready")
    return()
  }

  tryCatch({
    msg <- sprintf("Installing '%s' ... ", name)
    message(msg, appendLF = verbose)

    devtools::install_version(
      name,
      ver,
      repos = cran.index(),
      quiet = !verbose,
      upgrade = "never",
      INSTALL_opts = "--no-multiarch"
    )
    message("Done")
  },
  error = function(e) {
    message("Failed")
  })
}



.rgtk2IsInstalled <- function()
{
  "RGtk2" %in% .packages(all.available = TRUE)
}




#' @importFrom purrr map_lgl
#' @importFrom purrr walk
#' @importFrom utils download.file
#' @importFrom utils untar
#' @importFrom utils unzip
#'
#' @rdname install
#'
#' @export
install_rgtk2_and_deps <- function()
{
  tmpdir <- tempdir()

  # download GTK+
  gtkroot <- "C:/GTK"
  if (!dir.exists(gtkroot)) {
    gurl <-
      "http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip"
    gzp <- file.path(tmpdir, basename(gurl))
    download.file(gurl, gzp)

    # Extract to Root directory
    # TODO: Establish an option for re-downloadking GTK+ in the event of
    # installation failure caused by missing dependencies for the compilation
    #        unlink(gtkroot, recursive = TRUE, force = TRUE)
    files <- unzip(gzp, exdir = gtkroot)
    if (!length(files))
      stop("Failed to extract GTK+ from ZIP archive")
    file.remove(gzp)
  }

  # set environment variable for GTK_PATH (Windows only)
  # This enables the compiler to find include search path
  # per instructions in 'RGtk/INSTALL'
  Sys.setenv(GTK_PATH = gtkroot)

  # Download the RGtk2 archive tarball from CRAN and extract package
  if (!.rgtk2IsInstalled()) {
    rurl <-
      "https://cran.r-project.org/src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz"
    rtar <- file.path(tmpdir, basename(rurl))

    tryCatch({
      download.file(rurl, rtar)
      if (!untar(rtar, exdir = tmpdir, verbose = TRUE))
        stop("Extraction of RGtk2 tarball failed")
      file.remove(rtar)
    }, error = function(e) stop(e))

    # install RGtk2 - The package will be built from source.
    # We use the option --no-test-load to prevent attempts
    # at loading the package, which could fail due to the
    # absence of Gtk+ binaries within the package at the
    # time of installation.
    try(devtools::install(
      pkg = file.path(tmpdir, "RGtk2"),
      reload = FALSE,
      build = TRUE,
      args = c("--no-multiarch", "--no-test-load"),
      upgrade = 'never'
    ))
  }

  # Get a copy of the GTK+ distribution for RGtk2's internal use
  gtk.int.path <- .localGtkPath()
  if (dir.exists(gtk.int.path)) {
    message("Directory ", gtk.int.path, " already exists")
    return()
  }
  tryCatch({
    dir.create(gtk.int.path, recursive = TRUE)
  },
  warning = function(w)
    warning("Run `library(RGtk2)` to install Gtk+ when prompted"))

  message("Copying Gtk+ distribution to RGtk2 package...", appendLF = FALSE)
  tryCatch({
    successes <- purrr::map_lgl(
      list.files(gtkroot, full.names = TRUE),
      file.copy,
      to = gtk.int.path,
      recursive = TRUE
    )
    if (!all(successes))
      stop(call. = FALSE)
    message("Done")
  }, error = function(e) message("Failed"))
}
