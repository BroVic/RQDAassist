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

  ## Checks the availability of Rtools on Windows
  ## and if absent, tell user where to obtain it.
  if (!devtools::has_devel()) {
    if (.Platform$OS.type == 'windows') {
      toolsUrl <-
        file.path(.cranIndex(), "bin", .Platform$OS.type, "Rtools/history.html")
      stop(sprintf(
        "Build tools were not found. Please visit %s to install.",
        sQuote(toolsUrl)
      ),
      call. = FALSE)
    }
  }

  ## Install initial packages required by RQDA.
  ## What makes these ones special is that they are
  ## current package versions from CRAN and they are
  ## downloaded as binaries.
  pkgs <- c("igraph")   # old code had more than 1 package!
  # notInst <- pkgs[!pkgs %in% .packages(all.available = TRUE)]
  instTyp <- if (.onWindows()) "binary" else "source"
  if (!(pkgs %in% .packages(all.available = TRUE))) {
    tryCatch({
      install.packages(pkgs,
                       repos = .rstudioMirror(),
                       quiet = !verbose,
                       type = instTyp)
    }, error = function(e)
      stop(conditionMessage(e), call. = FALSE)
    )
  }

  install_rgtk2_and_deps()

  iwalk(
    c(
      cairoDevice = "2.28.2",
      gWidgets = '0.0-54.2',
      gWidgetsRGtk2 = '0.0-86',
      RQDA = '0.3-1'
    ),
    .installEachPackageByVersion,
    verbose = verbose
  )
}






# Internal functions ----



## Reports end result of a given operation
.report <- function()
{
  list(success = "Done\n", failure = "Failed\n")
}






.onWindows <- function() {
  .Platform$OS.type == "windows"
}





## Provides the index to CRAN directory
.cranIndex <- function() {
  c("https://cran.r-project.org")
}





## Returns the address to RStudio's CRAN mirror
.rstudioMirror <- function() {
  c('https://cran.rstudio.com')
}







.localGtkPath <- function() {
  gtkdir <- file.path(.libPaths()[1], 'RGtk2', 'gtk')
  file.path(gtkdir, .Platform$r_arch)
}







# Custom error condition for RGtk2
.abortRgtk2 <- function() {
  msg <- sprintf("Could not install RGtk2. Try doing so in R console")
  stop(msg, call. = FALSE)
}






## Controls how each package is installed
## @param name Name of the package
## @param ver The package version
#' @importFrom devtools install_version
.installEachPackageByVersion <- function(ver, name, verbose) {
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
    cat(sQuote(name), "is already installed\n")
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
    warning(sQuote(name), " was not installed because RGtk2 is not ready")
    return()
  }

  NL <- if (verbose) "\n" else ""
  cat(sprintf("Installing '%s' ... %s", name, NL))
  tryCatch({
    devtools::install_version(
      name,
      ver,
      repos = .cranIndex(),
      quiet = !verbose,
      upgrade = "never",
      INSTALL_opts = "--no-multiarch"
    )
    if (!verbose)
      cat(.report()$success)
  },
  error = function(e) {
    if (!verbose)
      cat(.report()$failure)
  })
}






.rgtk2IsInstalled <- function()
{
  "RGtk2" %in% .packages(all.available = TRUE)
}






#' @importFrom devtools install
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
  if (.Platform$OS.type == 'windows') {
    gtkroot <- "C:/GTK"
    cat("Check if Gtk distribution is in place... ")
    if (!dir.exists(gtkroot)) {
      cat("no\nInstalling... ")
      tryCatch({
        gzp <- .downloadArchive(
          "http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip",
          tmpdir
        )

        # Extract to Root directory
        # TODO: Establish an option for re-downloadking GTK+ in the event of
        # installation failure caused by missing dependencies for the compilation
        #        unlink(gtkroot, recursive = TRUE, force = TRUE)
        if (!length(unzip(gzp, exdir = gtkroot)))
          stop("Extraction of 'GTK+' archive failed")
        cat(.report()$success)
      },
      error = function(e) cat(.report()$failure),
      finally = file.remove(gzp))
    }
    else
      cat("yes\n")

    # set environment variable for GTK_PATH (Windows only)
    # This enables the compiler to find include search path
    # per instructions in 'RGtk/INSTALL'
    cat("Set environment variable 'GTK_PATH'... ")
    Sys.setenv(GTK_PATH = gtkroot)
    cat(.report()$success)
  }
  else
    warning("Automatic Gtk distribution is not (yet) supported for this platform")

  # Download the RGtk2 tarball from CRAN and extract package
  if (!.rgtk2IsInstalled()) {
    tryCatch({
      rtar <- .downloadArchive(
        "https://cran.r-project.org/src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz",
        tmpdir
      )
      cat("Extract RGtk2 archive... ")
      if (!untar(rtar, exdir = tmpdir, verbose = TRUE))
        cat(.report()$success)
    },
    error = function(e) cat(.report()$failure),
    finally = file.remove(rtar))

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
  if (dir.exists(gtk.int.path))
    return()
  withCallingHandlers(
    dir.create(gtk.int.path, recursive = TRUE),
    warning = function(w) {
      warning("Run `library(RGtk2)` to install Gtk+ when prompted")
      return()
    }
  )

  cat("Copy Gtk+ distribution to RGtk2 package... ")
  tryCatch({
    successes <- purrr::map_lgl(
      list.files(gtkroot, full.names = TRUE),
      file.copy,
      to = gtk.int.path,
      recursive = TRUE
    )
    if (!all(successes))
      stop(call. = FALSE)
    cat(.report()$success)
  }, error = function(e) {
    cat(.report()$failure)
    warning("Gtk+ was not properly situated in RGtk2")
  })
}






.downloadArchive <- function(url, destdir)
{
  stopifnot(dir.exists(destdir))
  if(!grepl("^https?\\:\\/\\/", url))
    stop("Invalid URL scheme")
  archname <- basename(url)
  archpath <- file.path(destdir, archname)
  dwn.meth <- "auto"
  if (.onWindows() && capabilities(curl <- 'libcurl'))
    dwn.meth <- curl
  if(download.file(url, archpath, method = dwn.meth))
    stop("Could not download ", archname)
  archpath
}
