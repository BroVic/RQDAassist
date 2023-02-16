#
# Source file: install.R
#
# ---- Exported functions ----
#' Install Archived RQDA
#'
#' Install RQDA from CRAN archives and at the same time installing its core
#' dependencies mainly, RGtk2, cairoDevice, gWidgets, and gWidgetsRGtk2. Where
#' necessary, compatible versions of R and Rtools are also installed.
#'
#' @details The \code{install} function carries out wholesale installation of
#' all the packages required by RQDA, before actually installing it.
#'
#' \code{install_rgtk2_and_deps} installs RGtk2. Before doing so, it fetches
#' the Gtk+ distribution, making it available for RGtk2. When a source
#' installation is used, GTK is required for compiling RGtk2 (and cairoDevice);
#' for the binary installation, Gtk+ is required for proper usage of RGtk2.
#' Additionally, if compatible versions of R (i.e. 4.0 to 4.1) or Rtools
#' (4.0) are not available on the machine, these will also be installed, but
#' only in an interactive session (Windows only).
#'
#'
#' @param type A string, one of \code{binary} or \code{source}.
#' @param verbose logical.
#'
#' @note The binary option will download RGtk2 from non-CRAN repository, while
#' RQDA, cairoDevice, gWidgets, and gWidgetsRGtk2 are downloaded as source
#' packages from the CRAN archive.
#'
#' @examples
#' # install RQDA (and dependencies) using defaults
#' \dontrun{install()}
#'
#' # install all from source, verbosely
#' \dontrun{install("source", TRUE)}
#'
#' @importFrom devtools install_version
#' @importFrom purrr iwalk
#' @importFrom utils install.packages
#'
#' @export
install <- function(type = c("binary", "source"), verbose = FALSE)
{
  type <- .validateArgs(type, verbose)

  if (!.sourceInstallPrompt(type))
    return(invisible())

  tryCatch(
    install_rgtk2_and_deps(type, verbose),
    error = function(e)
      .terminateOnCondition(e, verbose = TRUE)
  )

  cranBinaries <- "igraph"
  amiss <- !.pkgExists(cranBinaries)

  if (any(amiss))
    install.packages(cranBinaries[amiss],
                     repos = 'https://cran.rstudio.com',
                     quiet = !verbose,
                     type = "binary")

  installArchivedCranPackage <- function(ver, name) {
    if (.pkgExists(name)) {
      cat("Package already installed:", name, "\n")
      return()
    }

    .catJobMessage(sprintf("Installing '%s'", name), verbose)
    install.opts <- "--no-multiarch"

    if (identical(name, "cairoDevice")) {
      nl <- if (verbose) "" else "\n"

      if (!dir.exists(gtkroot())) {
        cat(nl)
        stop("The GTK distribution, a 'cairoDevice' dependency, was not found")
      }

      if (!.setGtkEnvironmentVariable(silent = TRUE)) {
        cat(nl)
        stop("'GTK_PATH' environment variable could not be set")
      }

      install.opts <- append(install.opts, "--no-test-load")
    }

    rgtk2ReadyForUse <- .pkgExists("RGtk2")

    if (.onWindows() && rgtk2ReadyForUse)
      rgtk2ReadyForUse <- dir.exists(.pkgLocalGtkPath())

    if (name %in% c('gWidgetsRGtk2', 'RQDA') &&
        isFALSE(rgtk2ReadyForUse)) {
      warning(sQuote(name),
              " requires the proper installation of ",
              sQuote("RGTk2"),
              call. = FALSE)
      return()
    }

    tryCatch({
      devtools::install_version(
        name,
        ver,
        repos = .cranIndex(),
        quiet = !verbose,
        upgrade = "never",
        INSTALL_opts = install.opts
      )

      .catReportSuccess(verbose)
    },
    error = function(e) {
      .terminateOnCondition(e, verbose)
    },
    warning = function(w) {
      warnmsg <- conditionMessage(w)

      if (endsWith(warnmsg, "had non-zero exit status"))
        .terminateOnCondition(w, verbose)
      else
        warning(warnmsg, call. = FALSE)
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
    type <- .validateArgs(type, verbose)
    tmpdir <- tempdir()

    if (.onWindows()) {
      .catJobMessage("Checking for the Gtk distribution", verbose)

      if (dir.exists(gtkroot())) {
        if (verbose)
          message("Root directory for GTK distribution is ", sQuote(gtkroot()))
        else
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

        tryCatch({
          .catJobMessage("Downloading the GTK distribution", verbose = TRUE)
          gtkzip <-
            .downloadArchives(gtk.path, tmpdir, quiet = !verbose)
          .catReportSuccess(verbose)
        }, error = function(e) .terminateOnCondition(e, verbose))

        tryCatch({
          .catJobMessage("Installing the GTK distribution", verbose)

          if (!length(unzip(gtkzip, exdir = gtkroot())))
            .extractionFailedError("GTK distribution")

          file.remove(gtkzip)
          .catReportSuccess(verbose)
        }, error = function(e) .terminateOnCondition(e, verbose))

      }

      if (!.ensureBuildReadiness(verbose))
        stop(.report(FALSE)$incompat)

      rgtk2 <- "RGtk2"

      if (!.pkgExists(rgtk2)) {
        if (type == "binary") {

          tryCatch({
            .catJobMessage("Installing binary build of RGtk2", verbose)
            install.packages(
              "https://access.togaware.com/RGtk2_2.20.36.2.zip",
              repos = NULL,
              verbose = verbose,
              quiet = !verbose
            )

            .catReportSuccess(verbose)
          },
          error = function(e) {
            .terminateOnCondition(e, verbose)
          },
          warning = function(w) {
            warning(conditionMessage(w), call. = FALSE)
          })

        }
        else if (type == "source") {
          rgtk2.cran <-
            file.path(.cranIndex(),
                      "src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz")

          tryCatch({
            .catJobMessage("Downloading RGtk2 archive", verbose)
            rtar <- .downloadArchives(rgtk2.cran, tmpdir, quiet = !verbose)
            .catReportSuccess(verbose)
          },
          error = function(e)
            .terminateOnCondition(e, verbose))

          tryCatch({
            .catJobMessage("Extracting RGtk2 archive", verbose)

            if (untar(rtar, exdir = tmpdir, verbose = verbose) != 0L)
              .extractionFailedError(rgtk2)

            file.remove(rtar)
            .catReportSuccess(verbose)
          },
          error = function(e)
            .terminateOnCondition(e, verbose))

          # RGtk2 will be built from source. We use the option --no-test-load to
          # prevent attempts at loading the package, which could fail due to the
          # absence of Gtk+ binaries within the package at the time of
          # installation.
          .setGtkEnvironmentVariable()

          tryCatch({
            .catJobMessage("Installing RGtk2", verbose)

            devtools::install(
              pkg = file.path(tmpdir, rgtk2),
              reload = FALSE,
              build = TRUE,
              args = c("--no-multiarch", "--no-test-load"),
              upgrade = 'never',
              quiet = !verbose
            )
            .catReportSuccess(verbose)
          },
          error = function(e)
            .terminateOnCondition(e, verbose))
        }
      }

      if (.pkgExists(rgtk2) && dir.exists(.pkgLocalGtkPath()))
          return(invisible())    # TODO: Perhaps check contents

      if (!.pkgExists(rgtk2))
        stop("RGtk2 was not installed")

      dir.create(.pkgLocalGtkPath(), recursive = TRUE)

      tryCatch({
        .catJobMessage("Copying Gtk+ to RGtk2", verbose)

        successes <-
          file.copy(list.files(gtkroot(), full.names = TRUE),
                    to = .pkgLocalGtkPath(),
                    recursive = TRUE)

        if (!all(successes))
          stop("Gtk+ was not properly copied to RGtk2", call. = FALSE)

        .catReportSuccess(verbose)
      },
      error = function(e) {
        unlink(dirname(.pkgLocalGtkPath()),
               force = TRUE,
               recursive = TRUE)
        .terminateOnCondition(e, verbose)
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




# ---- Compatibility of the build system ----

#' @importFrom devtools has_devel
.ensureBuildReadiness <- function(verbose)
{
  if (!.onWindows()) {
    if (verbose)
      message("Check for build tools not performed on this platform")

    return(TRUE)
  }

  notready <- function() {
    ready <- has_devel(quiet = TRUE) && .usingCompatibleR(!verbose)
    isFALSE(ready)
  }

  if (notready())
    .installRcoreSoftware(verbose)

  !notready()
}









.usingCompatibleR <- function(quiet = TRUE) {
  v <- getRversion()
  R_IsCompatible <- v >= "4.0" && v < "4.2"

  if (!quiet && !R_IsCompatible)
    message("Incompatible R version (", sQuote(v), ") is in use")

  R_IsCompatible
}









#' @import utils
.installRcoreSoftware <- function(verbose)
{
  runInstaller <- function(path) {
    tryCatch({
      .catJobMessage(sprintf("* Running installer %s", sQuote(basename(path))),
                     verbose = TRUE)

      out <- system(path)
      # readline("Press <ENTER> to continue... ")
    },
    error = function(e) {
      warning(conditionMessage(e), call. = FALSE)
    })
  }

  no_compat_rversion_keys <- function(key) {
      !any(grepl("^4\\.[0-1]", names(regkeys[[key]])))
  }

  stopifnot({
    .onWindows()
    exists("verbose", envir = environment())
  })

  if (!interactive()) {
    message("Automatic installation of compatible versions of R or Rtools ",
            "can only take place during interactive R sessions")
    return()
  }

  tryCatch({
    regkeys <- readRegistry("SOFTWARE\\R-core", maxdepth = 3)
  },
  error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    stop("Registry keys in 'R-core' missing or could not be accessed",
         call. = FALSE)
  })

  urls <- files <- c()

  if (is.null(regkeys$Rtools$`4.0`)) {
    urls <-
    "https://github.com/r-windows/rtools-installer/releases/download/2022-02-06"
    files <- "rtools40-x86_64.exe"
  }

  if ((no_compat_rversion_keys("R") ||
       no_compat_rversion_keys("R64")) &&
      isFALSE(.usingCompatibleR())) {
    rUrl <- file.path(.cranIndex(), "bin/windows/base/old/4.1.3")
    urls <- c(urls, rUrl)
    files <- c(files, "R-4.1.3-win.exe")
  }

  if (numsoft <- length(files) == 0L) {
    message("Both compatible R and Rtools were found on this system")
    return()
  }

  downdir <- file.path(Sys.getenv("HOME"), "Downloads")
  softInDownloads <- logical(numsoft)

  if (dir.exists(downdir))
    softInDownloads <- files %in% list.files(downdir)
  else
    downdir <- tempdir()

  downpaths <- character()

  if (sum(softInDownloads) < numsoft) {

    tmp.paths <-
      suppressWarnings(
        .downloadArchives(
          paste(urls[!softInDownloads], files[!softInDownloads], sep = "/"),
          timeout = Inf,
          quiet = FALSE,
          mode = "wb"
        )
      )

    downpaths <- paste(downdir, files[!softInDownloads], sep = "/")
    notmoved <- !file.copy(tmp.paths, downpaths)

    if (any(notmoved)) {

      templocationMsg <-
        mapply(function(file, path) { sprintf("* %s: %s", file, path) },
               files[notmoved], downpaths[notmoved])

      templocationMsg <- paste(unlist(templocationMsg), collapse = "\n")

      moveErrMsg <-
        paste(
          "There was a problem moving the downloaded file(s) from tempdir().",
          "The software can be copied manually from this/these location(s):",
          templocationMsg,
          sep = "\n"
        )

      stop(moveErrMsg)
    }
  }

  if (sum(softInDownloads) > 0L) {
    .catJobMessage("Using installer(s) found on local machine", verbose = TRUE)
    pathLocalSoftware <- paste(downdir, files[softInDownloads], sep = "/")
    downpaths <- c(downpaths, pathLocalSoftware)
  }

  lapply(downpaths, runInstaller)
}








# ---- Unified messaging ----
.report <- function(newline = TRUE)
{
  stopifnot(is.logical(newline))
  p <-
    list(success = "done",
         failure = "failed",
         incompat = "Compatible version of R and/or Rtools not currently in use")

  nl <- if (newline) "\n" else ""
  lapply(p, function(x) paste0(x, nl))
}






.onWindows <- function() {
  .Platform$OS.type == "windows"
}







# Custom error conditions for RGtk2
.abortRgtk2 <- function() {
  msg <- sprintf("Could not install RGtk2")
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
#
#' @importFrom assertthat is.string
.validateArgs <- function(type, verbose)
{
  if (!is.logical(verbose))
    stop("'verbose' must be logical vector")

  if (length(verbose) > 1L) {
    verbose <- verbose[1]
    warning("First element of verbose was taken and the rest ignored")
  }

  .validateInstallType(type)
}





#' @importFrom assertthat is.string
.extractionFailedError <- function(str)
{
  stop(sprintf("Extraction of %s failed", sQuote(str)))
}





.terminateOnCondition <- function(cond, verbose = FALSE)
{
  stopifnot({
    inherits(cond, "condition")
    is.logical(verbose)
  })

  if (!verbose)
    cat(.report()$failure)

  stop(conditionMessage(cond), call. = FALSE)
}





.catReportSuccess <- function(verbose)
{
  stopifnot(is.logical(verbose))
  msg <- ""

  if (!verbose)
    msg <- .report()$success

  cat(msg)
}





#' @importFrom assertthat is.string
.catJobMessage <- function(str, verbose)
{
  stopifnot({
    assertthat::is.string(str)
    is.logical(verbose)
  })

  ending <- if (verbose) "\n" else " ... "
  cat(str, ending)
}




# ---- Resource locators ----
## Provides the index to CRAN directory
.cranIndex <- function() {
  "https://cran.r-project.org"
}






# Where Gtk+ is saved locally within the RGtk2 library
.pkgLocalGtkPath <- function() {
  pkgpath <- system.file(package = "RGtk2")
  file.path(pkgpath, 'gtk', .Platform$r_arch)
}






# Wrapper for `download.file` essentally for building download paths and
# various settings
#
# @param urls The URL of the file to be downloaded.
# @param destdir The directory where the file is to be saved.
# @param timeout The download timeout in secods.
# @param ... Additional arguments passed to `download.file`.
#
# @return The absolute path of the downloaded file(s).
#
#' @importFrom utils download.file
.downloadArchives <- function(urls, destdir = NULL, timeout = 300, ...)
{
  archnames <- vapply(urls, basename, character(1))

  if (is.null(destdir))
    destdir <- tempdir()

  downpaths <- paste(destdir, archnames, sep = "/")
  downloadMethod <- "auto"

  if (.onWindows() && capabilities(curl <- "libcurl"))
    downloadMethod <- curl

  arglist <- c(list(...), method = downloadMethod)

  tryCatch({
    oldOpts <- options(internet.info = 100, timeout = timeout)
    returncodes <-
      .mapply(download.file, dots = list(urls, downpaths), MoreArgs = arglist)
    returncodes <- unlist(returncodes)
    successes <- returncodes == 0

    if(sum(successes) != length(successes)) {
      fn <- function(x) paste(x[!successes], collapse = ", ")
      archs <- fn(archnames)
      codes <- fn(returncodes)
      errtemplate <- "Call to 'download.file()' for %s failed with code %s"
      errmsg <- sprintf(errtemplate, sQuote(archs), codes)

      if (sum(!successes) > 1L)
        errmsg <- paste0(errmsg, ", respectively")

      stop(errmsg)
    }
  },
  error = function(e) {
    stop(e)
  },
  finally = options(oldOpts))

  paste(destdir, archnames, sep = "/")
}






.pkgExists <- function(pkg = character())
{
  stopifnot(is.character(pkg))
  all(pkg %in% .packages(all.available = TRUE))
}







gtkroot <- function() {
  c(GTK_PATH = "C:/GTK")
}




# ToDO: Make permanent between sessions
.setGtkEnvironmentVariable <- function(silent = FALSE) {
  envarset <- function()
    !identical(Sys.getenv(envarname), "")

  envarname <- names(gtkroot())

  if (!envarset()) {
    Sys.setenv(GTK_PATH = gtkroot())

    if (!silent && envarset())
      cat(sprintf("Environment variable %s was set\n", sQuote(envarname)))
  }

  envarset()
}






# ---- Miscellaneous helpers ----

# Prompts the user - only once after package is loaded (see zzz.R)
#' @importFrom assertthat is.string
.sourceInstallPrompt <- function(inst.type)
{
  stopifnot(is.character(inst.type))

  if (!interactive() || inst.type == 'binary')
    return(TRUE)

  if (!as.logical(Sys.getenv("RQDA_ASST_HAS_RUN_INSTALL"))) {
    Sys.setenv(RQDA_ASST_HAS_RUN_INSTALL = TRUE)
    cont <- "Continue (Y/n)? "
    prompt1 <- paste("Source installation will take some time.", cont)
    ans <- tolower(readline(prompt1))

    repeat {
      if (ans == "")
        ans <- "y"

      pos <- regexpr("^(y(es)?|no?)$", ans)

      if (pos > 0)
        break

      prompt2 <- paste("Invalid input.", cont)
      ans <- tolower(readline(prompt2))
    }

    ans <- substr(ans, pos - 1, pos)

    if (ans == 'n')
      return(FALSE)
  }

  TRUE
}
