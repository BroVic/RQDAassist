#
# Source file: install.R
#
# ---- Exported functions ----
#' Install Archived RQDA
#'
#' Install RQDA from CRAN archives and at the same time installing its core
#' dependencies namely, RGtk2, cairoDevice, gWidgets, and gWidgetsRGtk2.
#'
#' @details The \code{install} function carries out wholesale installation of
#' all the packages required by RQDA, before actually installing it.
#'
#' \code{install_rgtk2_and_deps} installs RGtk2. Before doing so, it fetches
#' the Gtk+ distribution, making it available for RGtk2. When a source
#' installation is used, GTK is required for compiling RGtk2 (and cairoDevice);
#' for the binary installation, Gtk+ is required for proper usage of RGtk2.
#'
#'
#' @param type A string, one of \code{binary} or \code{source}.
#' @param verbose logical.
#'
#' @note The binary option will download RGtk2 from non-CRAN repository, while
#' RQDA, cairoDevice, gWidgets, and gWidgetsRGtk2 are downloaded as source
#' packages from the CRAN archive. Windows users will need a version of Rtools
#' that is compatible with the R version to build the packages e.g. Rtools40
#' for R 4.1.3.
#'
#' @examples
#' # install RQDA (and dependencies) using defaults
#' \dontrun{install()}
#'
#' # install all from source, verbosely
#' \dontrun{install("source", TRUE)}
#'
#' @importFrom devtools has_devel
#' @importFrom devtools install_version
#' @importFrom purrr iwalk
#' @importFrom utils install.packages
#'
#' @export
install <- function(type = c("binary", "source"), verbose = FALSE)
{
  type <- .validateArgs(type, verbose)
  .startupPrompt(type)

  tryCatch(
    install_rgtk2_and_deps(type, verbose),
    error = function(e)
      stop(conditionMessage(e), call. = FALSE)
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

    if (identical(name, "cairoDevice")) {
      if (!dir.exists(gtkroot())) {
        warning("'cairoDevice' was skipped due to absence of GTK distribution")
        return()
      }

      .setGtkEnvironmentVariable(silent = TRUE)
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

    .catJobMessage(sprintf("Installing '%s'", name), verbose)
    tryCatch({
      devtools::install_version(
        name,
        ver,
        repos = .cranIndex(),
        quiet = !verbose,
        upgrade = "never",
        INSTALL_opts = "--no-multiarch"
      )

      .catReportSuccess()
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
    type <- .validateArgs(type, verbose)
    tmpdir <- tempdir()

    if (.onWindows()) {
      .catJobMessage("Checking for the Gtk distribution", verbose)

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

        tryCatch({
          .catJobMessage("Downloading the GTK distribution", verbose)
          gtkzip <-
            .downloadArchive(gtk.path, tmpdir, quiet = !verbose)
          .catReportSuccess()
        }, error = function(e) .terminateOnError(e))

        tryCatch({
          .catJobMessage("Installing the GTK distribution to 'C:\\'", verbose)

          if (!length(unzip(gtkzip, exdir = gtkroot())))
            stop(.extractionFailMessage("GTK distribution"))

          file.remove(gtkzip)
          .catReportSuccess()
        }, error = function(e) .terminateOnError(e))

      }

      if (!.ensureBuildReadiness(verbose))
        stop(.report(FALSE)$incompat)

      .setGtkEnvironmentVariable()
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

            .catReportSuccess()
          },
          error = function(e) {
            .terminateOnError(e)
          },
          warning = function(w) {
          })

        }
        else if (type == "source") {
          rgtk2.cran <-
            file.path(.cranIndex(),
                      "src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz")

          tryCatch({
            .catJobMessage("Downloading RGtk2 archive", verbose)
            rtar <- .downloadArchive(rgtk2.cran, tmpdir, quiet = !verbose)
            .catReportSuccess()
          },
          error = function(e)
            .terminateOnError(e))

          tryCatch({
            .catJobMessage("Extracting RGtk2 archive", verbose)

            if (untar(rtar, exdir = tmpdir, verbose = verbose) != 0L)
              stop(.extractionFailMessage(rgtk2))

            file.remove(rtar)
            .catReportSuccess()
          },
          error = function(e)
            .terminateOnError(e))

          # RGtk2 will be built from source. We use the option --no-test-load to
          # prevent attempts at loading the package, which could fail due to the
          # absence of Gtk+ binaries within the package at the time of
          # installation.
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
            .catReportSuccess()
          },
          error = function(e)
            .terminateOnError(e))
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

        .catReportSuccess()
      },
      error = function(e) {
        unlink(dirname(.pkgLocalGtkPath()),
               force = TRUE,
               recursive = TRUE)
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




# ---- Compatibility of the build system ----

#' @importFrom devtools has_devel
.ensureBuildReadiness <- function(verbose)
{
  if (!.onWindows())
    return(TRUE)

  notready <- function() {
    ready <-
      has_devel(quiet = !verbose) && .usingCompatibleR(!verbose)
    isFALSE(ready)
  }

  if (notready())
    .installRRtools(verbose)

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
.installRRtools <- function(verbose)
{
  stopifnot(.onWindows())

  if (!interactive()) {
    message("Automatic installation of compatible versions of R or Rtools ",
            "can only take place during interactive sessions")
    return()
  }

  download.dir <- file.path(Sys.getenv("HOME"), "Downloads")

  if (!dir.exists(download.dir)) {
    message("There is no folder named ",
            sQuote(basename(download.dir)),
            ". Installer(s) will be downloaded to a temporary location.")
    download.dir <- tempdir()
  }

  rtools <- paste(
    "https://github.com",
    "r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe",
    sep = "/"
  )
  rexe <- file.path(.cranIndex(), "bin/windows/base/old/4.1.3/R-4.1.3-win.exe")

  tryCatch({
    regkeys <- readRegistry("SOFTWARE\\R-core", maxdepth = 3)
  },
  error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    stop("Registry keys in 'R-core' are missing or could not be accessed",
         call. = FALSE)
  })

  if (is.null(regkeys$Rtools$`4.0`))
    ..winInstallSoftware(rtools, download.dir, verbose)

  noCompatibleRversionKeys <-
    function(key) { !any(grepl("^4\\.[0-1]", names(regkeys[[key]]))) }

  if (isFALSE(.usingCompatibleR()) &&
      (noCompatibleRversionKeys("R") || noCompatibleRversionKeys("R64")))
    ..winInstallSoftware(rexe, download.dir, verbose)
}








..winInstallSoftware <- function(url, dest, verbose) {
  softname <- basename(url)
  ans <- winDialog("yesno", sprintf("Install %s?", softname))

  if (ans == "NO") {
    cat("You aborted the installation of", sQuote(softname), "\n")
    return()
  }

  installer <-
    if (basename(dest) == "Downloads" && softname %in% list.files(dest))
      file.path(dest, softname)
    else
      .downloadArchive(url, dest, quiet = FALSE)

  tryCatch({
    if (verbose)
      message("Installing ", sQuote(softname), " ... ", appendLF = FALSE)

    cmd <- paste("START", installer)
    shell(cmd, shell = "C:\\WINDOWS\\system32\\cmd.exe")
    readline("Press <ENTER> to continue... ")
    message(.report()$success)
  },
  error = function(e) {
    message(.report()$failure)
    warning(conditionMessage(e), call. = FALSE)
  })
}












# ---- Unified messaging ----
.report <- function(newline = TRUE)
{
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
  if (!is.logical(verbose))
    stop("'verbose' must be logical vector")

  if (length(verbose) > 1L) {
    verbose <- verbose[1]
    warning("First element of verbose was taken and the rest ignored")
  }

  .validateInstallType(type)
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





.catReportSuccess <- function()
{
  cat(.report()$success)
}





#' @importFrom assertthat is.string
.catJobMessage <- function(str, ...)
{
  stopifnot(assertthat::is.string(str))
  cat(str, " ... ")
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







gtkroot <- function() {
  c(GTK_PATH = "C:/GTK")
}




# ToDO: Make permanent between sessions
.setGtkEnvironmentVariable <- function(silent = FALSE) {
  gtkpath <- gtkroot()
  envarname <- names(gtkpath)

  envarunset <- function() { is.null(Sys.getenv(envarname)) }

  if (envarunset()) {
    Sys.setenv(GTK_PATH = gtkpath)

    if (!silent && !envarunset())
      cat(sprintf("Environment variable %s was set\n", sQuote(envarname)))
  }

  isFALSE(envarunset())
}






# ---- Miscellaneous helpers ----

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




# .installRRtools <- function(verbose)
# {
#   download.dir <- file.path(Sys.getenv("HOME"), "Downloads")
#
#   if (!dir.exists(download.dir)) {
#     message(
#       "There is no folder named ",
#       sQuote(basename(download.dir)),
#       ". Installer(s) will be downloaded to a temporary location."
#     )
#     download.dir <- tempdir()
#   }
#
#   installSoftware <- function(softurl) {
#     if (basename(download.dir) == "Downloads") {
#       softname <- basename(softurl)
#
#       installer <- if (softname %in% list.files(download.dir))
#         file.path(download.dir, softname)
#       else
#         .downloadArchive(softurl, download.dir, quiet = !verbose)
#     }
#
#     tryCatch({
#       if (verbose)
#         message("Install ", sQuote(softname), " ... ", appendLF = FALSE)
#
#       shell.exec(installer)
#       message(.report()$success)
#     },
#     error = function(e) {
#       message(.report()$failure)
#       warning(conditionMessage(e), call. = FALSE)
#     })
#   }
#
#   rexe <- file.path(.cranIndex(), "bin/windows/base/old/4.1.3/R-4.1.3-win.exe")
#   rtools <- paste(
#     "https://github.com",
#     "r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe",
#     sep = "/"
#   )
#
#   if (getRversion() < "4.0" || getRversion() >= "4.2")
#     installSoftware(rexe)
#
#   if (!dir.exists("C:/rtools40"))
#     installSoftware(rtools)
# }
