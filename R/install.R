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
#' @note The binary option will download the package from MRAN. The packages
#' involved are RGtk2 and cairoDevice. All other packages, including RQDA, are
#' downloaded as source packages by default.
#'
#' @export
install <- function(type = c("binary", "source"),
                    verbose = TRUE)
{
  type <- match.arg(type)
  .crosscheckArgs(type, verbose)

  .startupPrompt()

  ## Check for the availability of Rtools on Windows
  ## and if absent, tell the user where to get it.
  if (isFALSE(devtools::has_devel(quiet = TRUE)) &&
      .onPlatform("windows")) {
    rtoolsurl <-
      file.path(.cranIndex(), "bin/windows/Rtools/history.html")
    stop(sprintf(
      paste(
        "Your system is not ready to build packages.",
        "Please visit %s to install Rtools."
      ),
      rtoolsurl
    ), call. = FALSE)
  }

  ## Install initial packages required by RQDA. What makes
  ## these ones special is that they are current package versions
  ## from CRAN and they are downloaded as binaries.
  pkg <- "igraph"
  if (.onPlatform("linux") && type == "binary")
    type <- "source"

  if (!.pkgExists(pkg))
    install.packages(pkg,
                     repos = 'https://cran.rstudio.com',
                     quiet = !verbose,
                     type = type)

  install_rgtk2_and_deps(type, verbose)

  # These are archived source packages in
  # their last known compatible version
  cran.arch <- c(gWidgets = '0.0-54.2',
                 gWidgetsRGtk2 = '0.0-86',
                 RQDA = '0.3-1')

  if (type == "source")
    cran.arch <- c(cairoDevice = "2.28.2", cran.arch)
  else {
    if (!.pkgExists("cairoDevice")) {
      url <- .mranUrls("cairoDevice")
      install.packages(
        url,
        repos = NULL,
        verbose = verbose,
        quiet = !verbose
      )
    }
  }

  iwalk(cran.arch, .installEachPackageByVersion, verbose = verbose)
}






# Prompts the user - only once after package is loaded (see zzz.R)
.startupPrompt <- function()
{
  if (interactive()) {
    if (!as.logical(Sys.getenv("RQDA_ASST_HAS_RUN_INSTALL"))) {
      Sys.setenv(RQDA_ASST_HAS_RUN_INSTALL = TRUE)
      cont <- "Continue (yes/no)? "

      prompt <- paste("This installation may take some time.", cont)
      ans <- tolower(readline(prompt))
      repeat {
        pos <- regexpr("^(y(es)?|no?)$", ans)
        if (pos > 0)
          break
        ans <- readline(paste("Invalid input", cont))
      }
      ans <- substr(ans, pos - 1, pos)
      if (tolower(ans) == 'n')
        return(invisible())
    }
  }

}






## Reports end result of a given operation
.report <- function()
{
  list(success = "Done\n", failure = "Failed\n")
}





# The default platform tested for by this function is Windows
.onPlatform <- function(x = c("windows", "macosx", "linux")) {
  x <- match.arg(x)
  if (x == "windows")
    return(.Platform$OS.type == x)
  if (x == "macosx")
    return(grepl("^x86.*darwin", R.version$platform))
  if (x == "linux")
    return(R.version$platform == "x86_64-pc-linux-gnu")
  stop("Platform", sQuote(R.version$platform), "is not supported")
}





## Provides the index to CRAN directory
.cranIndex <- function() {
  c("https://cran.r-project.org")
}






.mranUrls <- function(pkg)
{
  pkg <- match.arg(pkg, c("RGtk2", "cairoDevice"))
  stopifnot(is.character(pkg))
  if (.onPlatform("windows")) {
    os <- "windows"
    ext <- "zip"
  }
  else if (.onPlatform("macosx")) {
    os <- "macosx"
    ext <- "tgz"
  }
  else
    stop("Unsupported platform for binary installation")
  urlpath <-
    sprintf("https://cran.microsoft.com/snapshot/2021-12-15/bin/%s/contrib/4.1",
            os)
  allpkgs <- paste0(c(rgtk2 = "RGtk2_2.20.36.2.",
                      cairoDevice = "cairoDevice_2.28.2.1."),
                    ext)
  obj <- list(path = urlpath, package = allpkgs)
  pkg.rgx <- sprintf("^%s_", pkg)
  pkg.choice <- grep(pkg.rgx, obj$package, value = TRUE)
  paste(obj$path, pkg.choice, sep = '/')
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
    cat(name, "is already installed\n")
    return()
  }

  ## Grab the package archives as desired.
  ## When installing them, the following are considered:
  ##   => asking for upgrade interrupts installation
  ##   => install only 64-bit compatible builds
  ## But first, if RGtk2 is not present, there's no
  ## point trying to install packages that depend on it.
  dependsOnRgtk2 <- (name == 'gWidgetsRGtk2' || name == 'RQDA')

  rgtk2NotReady <- if (.onPlatform("windows"))
    ! (.pkgExists("RGtk2") && dir.exists(.localGtkPath()))
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
#' @importFrom magrittr %>%
#' @importFrom purrr map_lgl
#' @importFrom purrr walk
#' @importFrom utils download.file
#' @importFrom utils untar
#' @importFrom utils unzip
#'
#' @rdname install
#'
#' @export
install_rgtk2_and_deps <-
  function(type = c("binary", "source"), verbose)
  {
    type <- match.arg(type)
    .crosscheckArgs(type, verbose)

    if (.pkgExists("RGtk2"))  # TODO: Add check for GTK+?
      return()

    if (type == "binary") {
      url <- .mranUrls("RGtk2")
      install.packages(url,
                       repos = NULL,
                       verbose = verbose,
                       quiet = !verbose)
      warning(
        "RGtk2 has been installed but still needs Gtk+ to run correctly.",
        "Run 'library(RGtk2)' and follow the prompt to install Gtk+.",
        "Then, run 'RQDAassist::install' again to complete the installation.",
        call. = FALSE
      )
      return(invisible())
    }

    tmpdir <- tempdir()
    .install_gtk_manual(tmpdir)

    # install RGtk2 - The package will be built from source. We use the option
    # --no-test-load to prevent attempts at loading the package, which could fail
    # due to the absence of Gtk+ binaries within the package at the time of
    # installation. If not on Windows, we won't go beyond this step.
    rgtk.dir <- .extract_rgtk2_tarball(tmpdir)
    devtools::install(
      pkg = rgtk.dir,
      reload = FALSE,
      build = TRUE,
      args = c("--no-multiarch", "--no-test-load"),
      upgrade = 'never',
      quiet = !verbose
    )
    if (!.onPlatform("windows"))
      return(invisible())

    # Check whether there is a local copy of GTK+ in RGtk2 (as should be the
    # case whenever it is fully installed). If it is not so, mMake a copy of
    # from the one existing at C:/GTK for RGtk2's internal use (Windows only)
    gtk.pkg.path <- .localGtkPath()
    if (dir.exists(gtk.pkg.path)) # TODO: Perhaps check contents
      return(invisible())

    withCallingHandlers(
      dir.create(gtk.pkg.path, recursive = TRUE),
      warning = function(w) {
        warning("Run `library(RGtk2)` to install Gtk+ when prompted")
        return(invisible())
      }
    )

    .copy_gtk_root_to_pkg(gtk.pkg.path)
  }





## We install GTK+ manually
.install_gtk_manual <- function(dir)
{
  stopifnot(dir.exists(dir))
  if (.onPlatform("windows"))
    .gtk_win(dir)
  else if (.onPlatform("linux"))
    .gtk_ubuntu()
  else if (.onPlatform("macosx"))
    .gtk_macosx(dir)
  else
    warning("Automatic Gtk distribution is not (yet) supported for this platform")
}






.gtkroot <- function() {
  "C:/GTK"
}






.gtk_win <- function(download.dir)
{
  cat("Check for Gtk distribution... ")
  if (dir.exists(.gtkroot()))
    cat("present\n")
  else {
    cat("absent\nInstalling... ")
    tryCatch({
      gtk.arch.path <-
        "http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22"

      gtkarch <- if (.Platform$r_arch == "x64")
        "gtk+-bundle_2.22.1-20101229_win64.zip"
      else if (.Platform$r_arch == "i386")
        "gtk+-bundle_2.22.1-20101227_win32.zip"

      src.url <- paste(gtk.arch.path, gtkarch, sep = '/')
      gtk.zip <- .downloadArchive(src.url, download.dir)

      # Extract to Root directory
      # TODO: Establish an option for re-downloadking GTK+ in the event of
      # installation failure caused by missing dependencies for the compilation
      #        unlink(.gtkroot(), recursive = TRUE, force = TRUE)
      if (!length(unzip(gtk.zip, exdir = .gtkroot())))
        stop("Extraction of 'GTK+' archive failed")
      cat(.report()$success)
      file.remove(gtk.zip)
    },
    error = function(e)
      cat(.report()$failure))
  }

  # set environment variable for GTK_PATH (Windows only)
  # This enables the compiler to find include search path
  # per instructions in 'RGtk/INSTALL'
  cat("Set environment variable 'GTK_PATH'... ")
  Sys.setenv(GTK_PATH = .gtkroot())
  cat(.report()$success)
}







.gtk_ubuntu <- function()
{
  sudoInstall <- "sudo apt-get install -y libgtk2.0-dev"
  if (!interactive()) {
    stop(
      "The system dependencies cannot be installed non-interactively",
      "Please do this in an active R session, or at a Terminal using ",
      sQuote(sudoInstall)
    )
  }
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
    gsub("\\:", "\\\\:", .) %>%
    gsub("\\.", "\\\\.", .)

  op <- Sys.getenv("PATH")
  if (!grepl(rgx, op))
    Sys.setenv(PATH = paste(op, asPath, sep = ":"))
}







.gtk_macosx <- function(download.dir)
{
  url <- "http://r.research.att.com/libs/GTK_2.24.17-X11.pkg"
  gtk.pkg <- .downloadArchive(url, download.dir)
  system(paste("open", gtk.pkg))
}








# Downloads the RGtk2 tarball from CRAN and extracts package (all platforms)
.extract_rgtk2_tarball <- function(download.dir)
{
  tryCatch({
    url <- file.path(.cranIndex(),
                     "src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz")
    rgtk2.tar <- .downloadArchive(url, download.dir)
    cat("Extract RGtk2 archive... ")
    if (!untar(rgtk2.tar, exdir = download.dir, verbose = TRUE))
      cat(.report()$success)
    file.remove(rgtk2.tar)
  },
  error = function(e)
    { cat(.report()$failure) })

  file.path(download.dir, "RGtk2")
}








#' @importFrom purrr map_lgl
.copy_gtk_root_to_pkg <- function(rootpath, pkgpath) {
  tryCatch({
    cat("Copy Gtk+ distribution to RGtk2 package... ")
    gtkfiles <- list.files(rootpath, full.names = TRUE)
    successes <-
      purrr::map_lgl(gtkfiles, file.copy, to = pkgpath, recursive = TRUE)
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
  if (!is.logical(verbose)) {
    stop("'verbose' must be logical vector")
    if (length(verbose) > 1) {
      verbose <- verbose[1]
      warning("First element of verbose was taken and the rest ignored")
    }
  }
  if (!interactive() && type == "binary")
    stop("The binary version can only be installed in interactive mode")
}







.downloadArchive <- function(url, destdir)
{
  stopifnot(dir.exists(destdir))
  if (!grepl("^https?\\:\\/\\/", url))
    stop("Invalid URL scheme")
  archname <- basename(url)
  archpath <- file.path(destdir, archname)
  dwn.meth <- "auto"
  if (.onPlatform("windows") && capabilities(curl <- 'libcurl'))
    dwn.meth <- curl
  if (download.file(url, archpath, method = dwn.meth))
    stop("Could not download ", archname)
  archpath
}






.pkgExists <- function(pkg)
{
  stopifnot(is.character(pkg))
  all(pkg %in% .packages(all.available = TRUE))
}
