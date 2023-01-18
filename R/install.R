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
install <- function(type = c("binary", "source"), verbose = TRUE)
{
  type <- match.arg(type)
  .crosscheckArgs(type, verbose)
  .startupPrompt(type)

  ## Check for the availability of Rtools on Windows
  ## and if absent, tell the user where to get it.
  if (.onWindows() && !devtools::has_devel(quiet = !verbose)) {
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

  try(install_rgtk2_and_deps(type, verbose))

  ## Controls how each package is installed. This function is applied
  ## below to a named vector of the source package versions.
  installPackageByVersion <- function(ver, name) {
    ## Avoid repeat installations.
    if (.pkgExists(name)) {
      cat("Package already installed:", name, "\n")
      return()
    }

    rgtk2NotReady <- if (.onWindows())
      !(.pkgExists("RGtk2") && dir.exists(.pkgLocalGtkPath()))
    else
      isFALSE(.pkgExists("RGtk2"))

    if (name %in% c('gWidgetsRGtk2', 'RQDA') && rgtk2NotReady) {
      warning(name, " was not installed as RGtk2 was not found")
      return()
    }

    newLine <- if (verbose) "\n" else ""
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

  iwalk(
    c(    # maintain this order, so that RQDA is attempted last
      cairoDevice = "2.28.2.1",
      gWidgets = '0.0-54.2',
      gWidgetsRGtk2 = '0.0-86',
      RQDA = '0.3-1'
    ),
    installPackageByVersion
  )
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
        cat("absent\n")
        pref <- "gtk+-bundle_2.22.1-"

        if (.Platform$r_arch == "x64") {
          arch <- "win64"
          gtkarch <- sprintf("%s20101229_%s.zip", pref, arch)
        }
        else {
          arch <- "win32"
          gtkarch <- sprintf("%s20101227_%s.zip", pref, arch)
        }

        gtkarch.dir <-
          sprintf("https://download.gnome.org/binaries/%s/gtk+/2.22", arch)
        gtk.path <- paste(gtkarch.dir, gtkarch, sep = "/")
        gtkzip <- .downloadArchive(gtk.path, tmpdir)

        # Extract to root directory
        # TODO: Establish an option for re-downloading GTK+ in the event of
        # installation failure caused by missing dependencies for compilation
        #        unlink(gtkroot, recursive = TRUE, force = TRUE)
        tryCatch({
          cat("Installing GTK distribution to 'C:\' ... ")

          if (!length(unzip(gtkzip, exdir = gtkroot)))
            stop(.extractionFailMessage("GTK distribution"))
          file.remove(gtkzip)
          cat(.report()$success)
        },
        error = function(e) {
          cat(.report()$failure)
          stop(conditionMessage(e), call. = FALSE)
        })

        # Per instructions in 'RGtk2/INSTALL'
        cat("Set environment variable 'GTK_PATH'\n")
        Sys.setenv(GTK_PATH = gtkroot)
      }
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

    # Download the RGtk2 tarball from CRAN and extract package (all platforms)
    if (!.pkgExists("RGtk2")) {
      tryCatch({
        cat("Download RGtk2 archive ... ")

        rgtk2.cran <-
          file.path(.cranIndex(),
                    "src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz")

        rtar <- .downloadArchive(rgtk2.cran, tmpdir)
        cat(.report()$success)
      },
      error = function(e) {
        cat(.report()$failure)
        stop(conditionMessage(e), call. = FALSE)
      })

      tryCatch({
        cat("Extract RGtk2 archive ... ")

        if (untar(rtar, exdir = tmpdir, verbose = TRUE) != 0L)
          stop(.extractionFailMessage("RGtk2"))

        file.remove(rtar)

        # RGtk2 will be built from source. We use the option --no-test-load to
        # prevent attempts at loading the package, which could fail due to the
        # absence of Gtk+ binaries within the package at the time of
        # installation.
        devtools::install(
          pkg = file.path(tmpdir, "RGtk2"),
          reload = FALSE,
          build = TRUE,
          args = c("--no-multiarch", "--no-test-load"),
          upgrade = 'never',
          quiet = !verbose
        )
      },
      error = function(e) {
        cat(.report()$failure)
        stop(conditionMessage(e), call. = FALSE)
      })
    }

    if (!.onWindows())
      return(invisible())

    gtk.int.path <- .pkgLocalGtkPath()

    if (dir.exists(gtk.int.path))   # TODO: Perhaps check contents
      return(invisible())

    withCallingHandlers(
      dir.create(gtk.int.path, recursive = TRUE),
      warning = function(w) {
        warning("Run `library(RGtk2)` to install Gtk+ when prompted")
        return(invisible())
      }
    )

    cat("Copy Gtk+ distribution to RGtk2 ... ")

    tryCatch({
      successes <- vapply(
        list.files(gtkroot, full.names = TRUE),
        file.copy,
        logical(1),
        to = gtk.int.path,
        recursive = TRUE
      )

      if (!all(successes))
        stop(call. = FALSE)

      cat(.report()$success)
    },
    error = function(e) {
      cat(.report()$failure)
      warning("Gtk+ was not properly situated in RGtk2", call. = FALSE)
      warning(conditionMessage(e), call. = FALSE)
    })
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
  destpath <- file.path(destdir, archname)
  dwn.meth <- "auto"

  if (.onWindows() && capabilities(curl <- 'libcurl'))
    dwn.meth <- curl

  if(download.file(url, destpath, method = dwn.meth) != 0L)
    stop("Could not download ", archname)

  destpath
}






.pkgExists <- function(pkg)
{
  stopifnot(is.character(pkg))
  all(pkg %in% .packages(all.available = TRUE))
}






.extractionFailMessage <- function(str)
{
  stopifnot({
    is.character(str)
    length(str) == 1L
  })
  sprintf("Extraction of %s failed")
}
