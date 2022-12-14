globalVariables(".")

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
#' @param type A string, one of \code{binary} or \code{source}.
#' @param verbose logical.
#'
#' @note The binary option will download the package from a non-CRAN repository.
#' The packages involved are RGtk2 and cairoDevice. All other packages,
#' including RQDA, are downloaded as source packages by default.
#'
#' @export
install <- function(type = c("binary", "source"), verbose = TRUE)
{
  type <- match.arg(type)
  .crosscheckArgs(type, verbose)
  .startupPrompt(type)

  ## Check for the availability of Rtools on Windows
  ## and if absent, tell the user where to get it.
  if (.onWindows() && !devtools::has_devel(quiet = TRUE)) {
    toolsUrl <-
      file.path(.cranIndex(),
                "bin",
                .Platform$OS.type,
                "Rtools/history.html")
    stop(
      sprintf(
        paste("Your system is not ready to build packages.",
              "Please visit %s to install Rtools."),
        sQuote(toolsUrl)
      ),
      call. = FALSE
    )
  }

  ## Install initial packages required by RQDA. What makes these ones special
  ## is that they are current package versions from CRAN and they are
  ## downloaded as binaries.
  pkg <- "igraph"

  if (!(pkg %in% .packages(all.available = TRUE))) {
    install.packages(pkg,
                     repos = 'https://cran.rstudio.com',
                     quiet = !verbose,
                     type = "binary")
  }

  install_rgtk2_and_deps(type, verbose)

  ## These are last known compatible versions of packages
  ## and they are all source tarballs, and thus have to be
  ## built.
  cran.arch <- c(
      cairoDevice = "2.28.2.1",
      gWidgets = '0.0-54.2',
      gWidgetsRGtk2 = '0.0-86',
      RQDA = '0.3-1'
    )

  iwalk(cran.arch, .installEachPackageByVersion, verbose = verbose)
}






# Prompts the user - only once after package is loaded (see zzz.R)
#' @importFrom assertthat is.string
.startupPrompt <- function(inst.type)
{
  stopifnot(assertthat::is.string(inst.type))

  if (!interactive() || inst.type == "binary")
    return(invisible())

  if (!as.logical(Sys.getenv("RQDA_ASST_HAS_RUN_INSTALL"))) {
    readline_lowcase <- function() tolower(readline(prompt))

    Sys.setenv(RQDA_ASST_HAS_RUN_INSTALL = TRUE)
    cont <- "Continue (Y/N)? "
    prompt <- paste("Source installation will take some time.", cont)
    ans <- readline_lowcase()

    repeat {
      pos <- regexpr("^(y(es)?|no?)$", ans)

      if (pos > 0)
        break

      prompt <- paste("Invalid input.", cont)
      ans <- readline_lowcase()
    }

    ans <- substr(ans, pos - 1, pos)

    if (ans == 'n')
      return(invisible())
  }
}






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







.localGtkPath <- function() {
  pkgpath <- system.file(package = "RGtk2")
  file.path(pkgpath, 'gtk', .Platform$r_arch)
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

  ## Avoid repeat installations via an early return.
  if (.pkgExists(name)) {
    cat("Package already installed:", name, "\n")
    return()
  }

  ## Grab the package archives as desired.
  ## When installing them, the following are considered:
  ## => asking for upgrade interrupts installation
  ## => install only 64-bit compatible builds
  ## But first, if RGtk2 is not present, there's no
  ## point trying to install packages that depend on it.
  dependsOnRgtk2 <- (name == 'gWidgetsRGtk2' || name == 'RQDA')

  rgtk2NotReady <- if (.onWindows())
    !(.pkgExists("RGtk2") && dir.exists(.localGtkPath()))
  else
    isFALSE(.pkgExists("RGtk2"))

  if (dependsOnRgtk2 && rgtk2NotReady) {
    warning(name, " was not installed because RGtk2 is not ready")
    return()
  }

  newLine <- if (verbose)
    "\n"
  else
    ""

  cat(sprintf("Installing '%s' ... %s", name, newLine))

  tryCatch({
    devtools::install_version(
      name,
      ver,
      repos = .cranIndex(),
      quiet = !verbose,
      upgrade = "never",
      INSTALL_opts = "--no-multiarch"
    )
    cat(.report()$success)
  },
  error = function(e) {
    cat(.report()$failure)
    warning(conditionMessage(e))
  })
}








#' @importFrom devtools install
#' @importFrom purrr map_lgl
#' @importFrom purrr walk
#' @importFrom stringr %>%
#' @importFrom stringr str_replace_all
#' @importFrom utils download.file
#' @importFrom utils untar
#' @importFrom utils unzip
#'
#' @rdname install
#'
#' @export
install_rgtk2_and_deps <-
  function(type = c("binary", "source"), verbose = TRUE)
{
  type <- match.arg(type)
  .crosscheckArgs(type, verbose)

  tmpdir <- tempdir()

  if (.onWindows()) {

    if (type == "binary") {

      if (!.pkgExists("RGtk2")) {
        install.packages(
          "https://access.togaware.com/RGtk2_2.20.36.2.zip",
          repos = NULL,
          verbose = verbose,
          quiet = !verbose
        )
        warning(
          "RGtk2 has been installed but still needs Gtk+ to run correctly.",
          "Run 'library(RGtk2)' and follow the prompt to install Gtk+.",
          "Then, run 'RQDAassist::install' again to complete the installation.",
          call. = FALSE
        )
      }

      return(invisible())
    }

    ## The rest of the function is for handling source installs
    gtkroot <- "C:/GTK"
    cat("Check for Gtk distribution... ")

    if (dir.exists(gtkroot)) {
      cat("present\n")
    }
    else {
      cat("absent\nInstalling... ")

      tryCatch({
        if (.Platform$r_arch == "x64") {
          gtkarch <- "gtk+-bundle_2.22.1-20101229_win64.zip"
          arch <- "win64"
        }
        else {
          gtkarch <- "gtk+-bundle_2.22.1-20101227_win32.zip"
          arch <- "win32"
        }

        gtkarch.dir <-
          sprintf("http://ftp.gnome.org/pub/gnome/binaries/%s/gtk+/2.22",
                  arch)
        gzp <- .downloadArchive(file.path(gtkarch.dir, gtkarch), tmpdir)

        # Extract to Root directory
        # TODO: Establish an option for re-downloading GTK+ in the event of
        # installation failure caused by missing dependencies for compilation
        #        unlink(gtkroot, recursive = TRUE, force = TRUE)
        if (!length(unzip(gzp, exdir = gtkroot)))
          stop("Extraction of 'GTK+' archive failed")

        cat(.report()$success)
        file.remove(gzp)
      },
      error = function(e)
        cat(.report()$failure))
    }

    # set environment variable for GTK_PATH (Windows only)
    # This enables the compiler to find include search path
    # per instructions in 'RGtk2/INSTALL'
    cat("Set environment variable 'GTK_PATH'... ")
    Sys.setenv(GTK_PATH = gtkroot)
    cat(.report()$success)
  }
  else if (R.version$platform == "x86_64-pc-linux-gnu") {
    sudoInstall <- "sudo apt-get install -y libgtk2.0-dev"

    if (!interactive())
      stop("The system dependencies cannot be installed non-interactively",
           "Please do this in an active R session, or at a Terminal using ",
           sQuote(sudoInstall))

    system(paste("sudo apt-get update;", sudoInstall))

    # Update PATH if necessary
    asPath <- paste(
      c(
        "/usr/lib/x86_64-linux-gnu/gtk-2.0",
        "/etc/gtk-2.0",
        "/usr/include/gtk-2.0"
      ),
      collapse = ":"
    )
    rgx <- asPath %>%
      str_replace_all("\\:", "\\\\:") %>%
      str_replace_all("\\.", "\\\\.")

    op <- Sys.getenv("PATH")

    if (!grepl(rgx, op))
      Sys.setenv(PATH = paste(op, asPath, sep = ":"))
  }
  else
    warning("Automatic Gtk distribution is not supported for this platform")

  # Download the RGtk2 tarball from CRAN and extract package (all platforms)
  if (!.pkgExists("RGtk2")) {
    tryCatch({
      url <- file.path(.cranIndex(),
                       "src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz")
      rtar <- .downloadArchive(url, tmpdir)
      cat("Extract RGtk2 archive... ")

      if (!untar(rtar, exdir = tmpdir, verbose = TRUE))
        cat(.report()$success)

      file.remove(rtar)
    },
    error = function(e) cat(.report()$failure))

    # RGtk2 will be built from source. We use the option --no-test-load to
    # prevent attempts at loading the package, which could fail due to the
    # absence of Gtk+ binaries within the package at the time of installation.
    devtools::install(
      pkg = file.path(tmpdir, "RGtk2"),
      reload = FALSE,
      build = TRUE,
      args = c("--no-multiarch", "--no-test-load"),
      upgrade = 'never',
      quiet = !verbose
    )
  }

  # Make a copy of GTK+ for RGtk2's internal use (Windows)
  if (!.onWindows())
    return(invisible())

  gtk.int.path <- .localGtkPath()

  if (dir.exists(gtk.int.path))   # TODO: Perhaps check contents
    return(invisible())

  withCallingHandlers(
    dir.create(gtk.int.path, recursive = TRUE),
    warning = function(w) {
      warning("Run `library(RGtk2)` to install Gtk+ when prompted")
      return(invisible())
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




# Checks the two arguments passed onto the installation functions.
# If the checks are passed, returns the value of 'type' (does partial matching)
.crosscheckArgs <- function(type, verbose)
{
  if (!is.logical(verbose))
    stop("'verbose' must be logical vector")

  if (length(verbose) > 1) {
    verbose <- verbose[1]
    warning("First element of verbose was taken and the rest ignored")
  }

  if (!interactive() && type == "binary")
    stop("The binary version can only be installed in interactive mode")
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






.pkgExists <- function(pkg)
{
  stopifnot(is.character(pkg))
  all(pkg %in% .packages(all.available = TRUE))
}
