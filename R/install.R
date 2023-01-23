globalVariables(".")

#' Install Archived RQDA
#'
#' Install RQDA from CRAN archives and at the same time installing its core
#' dependencies namely, RGtk2, cairoDevice, gWidgets, and gWidgetsRGtk2.
#'
#' @details The \code{install} function carries out wholesale installation of
#' all the packages required by RQDA, before actually installing it.
#'
#' \code{install_rgtk2_and_deps()} installs RGtk2. Before doing so, it fetches
#' the Gtk+ distribution, making it available for the compilations of RGtk2
#' when a source installation is used; for the binary installation, Gtk+ is
#' installed interactively when an attempt is made to load RGtk2 for the first
#' time.
#'
#' @param type A string, one of \code{binary} or \code{source}.
#' @param verbose logical.
#'
#' @note The binary option will download RGtk2 from non-CRAN repository.
#' RQDA, cairoDevice, gWidgets, and gWidgetsRGtk2 are downloaded as source
#' packages from the CRAN archive. Windows users will need an appropriate
#' version of Rtools to build the packages.
#'
#' @importFrom devtools has_devel
#' @importFrom devtools install_version
#' @importFrom purrr iwalk
#' @importFrom utils install.packages
#'
#' @export
install <- function(type = c("binary", "source"), verbose = FALSE)
{
  .validateArgs(type, verbose)
  .startupPrompt(type)
  .checkBuildReadiness()
  try(.installCranBinaries("igraph"))
  try(install_rgtk2_and_deps(type, verbose))

  installArchivedCranPackage <- function(ver, name) {
    if (.pkgExists(name)) {
      cat("Package already installed:", name, "\n")
      return()
    }

    if (identical(name, "cairoDevice")) {
      if (!dir.exists(gtkroot())) {
        warning("'cairoDevice' was skipped due to absence of GTK distribution")
        return()
      }

      .setGtkEnvar()
    }

    rgtk2ReadyForUse <- .pkgExists("RGtk2")

    if (.onWindows() && rgtk2ReadyForUse)
      rgtk2ReadyForUse <- dir.exists(.pkgLocalGtkPath())

    if (name %in% c('gWidgetsRGtk2', 'RQDA') &&
        isFALSE(rgtk2ReadyForUse)) {
      warning(sQuote(name),
              " requires the proper installation of ",
              sQuote("RGTk2"))
      return()
    }

    .jobMessage(sprintf("Installing '%s'", name), verbose)
    tryCatch({
      devtools::install_version(
        name,
        ver,
        repos = .cranIndex(),
        quiet = !verbose,
        upgrade = "never",
        INSTALL_opts = "--no-multiarch"
      )

      .reportSuccess()
    },
    error = function(e) {
      .terminateOnError(e)
    })
  }

  iwalk(
    c(
      # maintain this order, so that RQDA is attempted last
      gWidgets = '0.0-54.2',
      cairoDevice = "2.28.2.1",
      gWidgetsRGtk2 = '0.0-86',
      RQDA = '0.3-1'
    ),
    installArchivedCranPackage
  )
}







#' @importFrom devtools install
#' @importFrom utils install.packages
#' @importFrom utils untar
#' @importFrom utils unzip
#'
#' @rdname install
#'
#' @export
install_rgtk2_and_deps <-
  function(type = c("binary", "source"), verbose = FALSE)
  {
    .validateArgs(type, verbose)
    .checkBuildReadiness(type, verbose)
    tmpdir <- tempdir()
    rgtk2 <- "RGtk2"

    if (.onWindows()) {
      .jobMessage("Check for the Gtk distribution", verbose)

      if (dir.exists(gtkroot())) {
        cat("found\n")
      }
      else {
        cat("missing\n")
        prefix <- "gtk+-bundle_2.22.1-"

        if (.Platform$r_arch == "x64") {
          arch <- "win64"
          gtkarch <- sprintf("%s20101229_%s.zip", prefix, arch)
        }
        else {
          arch <- "win32"
          gtkarch <- sprintf("%s20101227_%s.zip", prefix, arch)
        }

        gtkarch.dir <-
          sprintf("https://download.gnome.org/binaries/%s/gtk+/2.22", arch)
        gtk.path <- paste(gtkarch.dir, gtkarch, sep = "/")

        .jobMessage("Download the GTK distribution", verbose)
        tryCatch({
          gtkzip <- .downloadArchive(gtk.path, tmpdir, quiet = !verbose)
          .reportSuccess()
        }, error = function(e) .terminateOnError(e))

        .jobMessage("Install the GTK distribution to 'C:\\'", verbose)
        tryCatch({
          if (!length(unzip(gtkzip, exdir = gtkroot())))
            stop(.extractionFailMessage("GTK distribution"))

          file.remove(gtkzip)
          .reportSuccess()
        }, error = function(e) .terminateOnError(e))

        cat(sprintf("Set environment variable %s\n", sQuote(names(gtkroot()))))
        .setGtkEnvar()
      }

      if (!.pkgExists(rgtk2)) {
        if (type == "binary") {

          .jobMessage("Install binary build of RGtk2", verbose)
          tryCatch({
            if (!verbose)
              sink(tempfile())

            install.packages("https://access.togaware.com/RGtk2_2.20.36.2.zip",
                             repos = NULL,
                             verbose = verbose,
                             quiet = !verbose)

            if (!verbose)
              sink()

            .reportSuccess()
          },
          error = function(e)
            .terminateOnError(e))
        }
        else if (type == "source") {
          rgtk2.cran <-
            file.path(.cranIndex(),
                      "src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz")

          .jobMessage("Download RGtk2 archive", verbose)
          tryCatch({
            rtar <- .downloadArchive(rgtk2.cran, tmpdir, quiet = !verbose)
            .reportSuccess()
          },
          error = function(e)
            .terminateOnError(e))

          .jobMessage("Extract RGtk2 archive", verbose)
          tryCatch({
            if (untar(rtar, exdir = tmpdir, verbose = verbose) != 0L)
              stop(.extractionFailMessage(rgtk2))

            file.remove(rtar)
            .reportSuccess()
          },
          error = function(e)
            .terminateOnError(e))

          # RGtk2 will be built from source. We use the option --no-test-load to
          # prevent attempts at loading the package, which could fail due to the
          # absence of Gtk+ binaries within the package at the time of
          # installation.
          .jobMessage("Install RGtk2", verbose)
          tryCatch({
            devtools::install(
              pkg = file.path(tmpdir, rgtk2),
              reload = FALSE,
              build = TRUE,
              args = c("--no-multiarch", "--no-test-load"),
              upgrade = 'never',
              quiet = !verbose
            )

            .reportSuccess()
          },
          error = function(e)
            .terminateOnError(e))
        }
      }

      if (.pkgExists(rgtk2) && dir.exists(.pkgLocalGtkPath()))
        return(invisible())    # TODO: Perhaps check contents

      dir.create(.pkgLocalGtkPath(), recursive = TRUE)
      .jobMessage("Copy Gtk+ to RGtk2", verbose)
      tryCatch({
        successes <- vapply(
          list.files(gtkroot(), full.names = TRUE),
          file.copy,
          logical(1),
          to = .pkgLocalGtkPath(),
          recursive = TRUE
        )

        if (!all(successes))
          stop(call. = FALSE)

        .reportSuccess()
      },
      error = function(e) {
        warning("Gtk+ was not properly copied to RGtk2", call. = FALSE)
        .terminateOnError(e)
      })

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
    else {
      warning("Automatic Gtk distribution is not supported for this platform")
    }
  }






# Prompts the user - only once after package is loaded (see zzz.R)
#' @importFrom assertthat is.string
.startupPrompt <- function(inst.type)
{
  if (!interactive() || inst.type == 'binary')
    return(invisible())

  if (!as.logical(Sys.getenv("RQDA_ASST_HAS_RUN_INSTALL"))) {
    Sys.setenv(RQDA_ASST_HAS_RUN_INSTALL = TRUE)
    cont <- "Continue (Y/n)? "
    prompt <- paste("Source installation will take some time.", cont)
    readline_lowcase <- function() { tolower(readline(prompt)) }
    ans <- readline_lowcase()

    repeat {
      if (ans == "")
        ans <- "y"

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





.checkBuildReadiness <- function()
{
  .checkBuildTools()
  .checkRversion()
}





.checkRversion <- function()
{
  if (!.onWindows())
    return()

  if (getRversion() >= 4.2)
    stop("Dependencies currently not installable in R For Windows >= 4.2 ")
}





.checkBuildTools <- function()
{
  if (!.onWindows())
    return(TRUE)

  if (!devtools::has_devel(quiet = !verbose))
    warning(sprintf(
      paste(
        "Your system is not ready to build packages.",
        "Please visit %s to install Rtools."
      ),
      sQuote(file.path(.cranIndex(), "bin/Rtools/history.html"))
    ),
    call. = FALSE)
}





## Reports end result of a given operation
.report <- function()
{
  p <- list(success = "done", failure = "failed")
  paste0(p, '\n')
}






.installCranBinaries <- function(pkgs)
{
  amiss <- !.pkgExists(pkgs)
  if (any(amiss))
    install.packages(pkgs[amiss],
                     repos = 'https://cran.rstudio.com',
                     quiet = !verbose,
                     type = "binary")
}





.onWindows <- function() {
  .Platform$OS.type == "windows"
}





## Provides the index to CRAN directory
.cranIndex <- function() {
  "https://cran.r-project.org"
}






# Where Gtk+ is saved locally within the RGtk2 library
.pkgLocalGtkPath <- function() {
  pkgpath <- system.file(package = "RGtk2")
  file.path(pkgpath, 'gtk', .Platform$r_arch)
}







# Custom error conditions for RGtk2
.abortRgtk2 <- function() {
  msg <- sprintf("Could not install RGtk2. Try doing so in the R console")
  stop(msg, call. = FALSE)
}





.validateInstallType <- function(type)
{
  tryCatch(match.arg(type, c("binary", "source")),
           error = function(e)
             stop(conditionMessage(e), call. = FALSE))
}






# Checks the two arguments passed onto the installation functions.
# If the checks are passed, returns the value of 'type' (does partial matching)
#' @importFrom assertthat is.string
.validateArgs <- function(type, verbose)
{
  type <- .validateInstallType(type)

  if (!is.logical(verbose))
    stop("'verbose' must be logical vector")

  if (length(verbose) > 1L) {
    verbose <- verbose[1]
    warning("First element of verbose was taken and the rest ignored")
  }
}






# Wrapper for `download.file` essentally for building download paths and
# various settings
#
# @param url The URL of the file to be downloaded.
# @param destdir The directory where the file is to be saved.
# @param ... Additional arguments passed to `download.file`.
#
#' @importFrom utils download.file
.downloadArchive <- function(url, destdir, ...)
{
  stopifnot(dir.exists(destdir))
  fOpts <- options(internet.info = 100)

  if(!grepl("^https?\\:\\/\\/", url))
    stop("Invalid URL scheme")

  archname <- basename(url)
  destfile <- file.path(destdir, archname)
  dwn.meth <- "auto"

  if (.onWindows() && capabilities(curl <- 'libcurl'))
    dwn.meth <- curl

  tryCatch({
    res <- download.file(url, destfile, method = dwn.meth, ...)
  }, error = function(e) { # do nothing
  }, finally = options(fOpts))

  if(res != 0L)
    stop("Could not download ", sQuote(archname))

  destfile
}






.pkgExists <- function(pkg = character())
{
  stopifnot(is.character(pkg))
  all(pkg %in% .packages(all.available = TRUE))
}





#' @importFrom assertthat is.string
.extractionFailMessage <- function(str)
{
  stopifnot(assertthat::is.string(str))
  sprintf("Extraction of %s failed")
}





.terminateOnError <- function(err)
{
  stopifnot(inherits(err, "simpleError"))
  cat(.report()$failure)
  stop(conditionMessage(err), call. = FALSE)
}





.reportSuccess <- function()
{
  cat(.report()$success)
}





#' @importFrom assertthat is.string
.jobMessage <- function(str, ...)
{
  stopifnot(assertthat::is.string(str))
  cat(str, " ... ")
}





gtkroot <- function() {
  c(GTK_PATH = "C:/GTK")
}





.setGtkEnvar <- function() {
  Sys.setenv(GTK_PATH = gtkroot())
}






.installRRtools <- function(verbose)
{
  stopifnot(is.logical(verbose))
  download.dir <- file.path(Sys.getenv("HOME"), "Downloads")
  rexe <- file.path(.cranIndex(), "bin/windows/base/old/4.1.3/R-4.1.3-win.exe")
  rtools <- paste(
    "https://github.com",
    "r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe",
    sep = "/"
    )

  if (!dir.exists(download.dir))
    download.dir <- tempdir()  ## TODO: Add a message

  installRRtoolsSoftware <- function(rurl) {
    if (basename(download.dir) == "Downloads") {
      rsoft <- basename(rurl)

      download <- if (rsoft %in% list.files(download.dir))
        file.path(download.dir, rsoft)
      else
        .downloadArchive(rurl, download.dir, quiet = !verbose)
    }

    shell.exec(download)
  }

  if (getRversion() < "4.0" || getRversion() >= "4.2")
    installRRtoolsSoftware(rexe)

  if (!dir.exists("C:/rtools40"))
    installRRtoolsSoftware(rtools)
}
