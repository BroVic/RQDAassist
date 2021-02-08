#' Install RQDA Archive
#'
#' Install RQDA from CRAN while it is archived while performing an
#' installation of current and archived packages upon which it depends.
#' The key packages mentioned are RGtk2, gWidgets and gWidgetsRGtk2.
#'
#' @note If Gtk+ is not properly installed, this function will stop
#' and require the user to to install it. Once this is done, this
#' function should be run again to complete the installation of the
#' packages that directly or indirectly depend on it.
#'
#' @importFrom devtools has_devel
#' @importFrom devtools install_version
#' @importFrom utils install.packages
#'
#' @param verbose logical.
#'
#' @export
install <- function(verbose = FALSE)
{
  ## Provides the index to CRAN directory
  cran.index <- function() {
    c("https://cran.r-project.org")
  }

  ## Returns the address to RStudio's CRAN mirror
  rstudio <- function() {
    c('https://cran.rstudio.com')
  }

  local_gtk_bin_path <- function(pkg = 'RGtk2') {
    gtkdir <- file.path(.libPaths()[1], pkg, 'gtk')
    file.path(gtkdir, .Platform$r_arch)
  }


  ## Installs initial packages required by the script.
  ## What makes these ones special is that they are
  ## current package versions from CRAN and they are
  ## downloaded as binaries.
  ## @param cranry A character vector of packages.
  .install_init <- function(cranbry) {
    stopifnot(is.character(cranbry))
    tryCatch({
      notInstalled <-
        cranbry[!cranbry %in% .packages(all.available = TRUE)]
      install.packages(notInstalled, repos = rstudio(), quiet = !verbose)
    }, error = function(e) {
      stop(sprintf(
        "Initialization failed. Install %s",
        paste(cranbry, collapse = ', ')
      ))
    })
  }


  ## Checks the availability of Rtools on Windows (v35)
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

  .install_init(c('devtools', 'cairoDevice', "igraph"))

  .check_buildtools()

  ## Installs a given CRAN archive
  ## @param name Name of the package
  ## @param ver The package version
  inst <- function(name, ver) {
    rgtk2 <- "RGtk2"
    archOpts <- "--no-multiarch"
    isRGtk2 <- name == rgtk2
    pkgExists <- quote(name %in% .packages(all.available = TRUE))

    if (isRGtk2) {
      msgRGtk2 <-
        list(
          line1 = paste("Installing 'RGtk2'. If it fails, use",
                        "`install.packages` in R console ... "),
          line2 = paste("Run `library(RGtk2)` in R to install",
                        "Gtk+. Then, rerun this script.")
        )

      # Custom error condition
      abortRgtk2 <- function() {
        msg <-
          sprintf("Could not install %s. Try doing so in R console", rgtk2)
        stop(msg, call. = FALSE)
      }

      ## Install RGtk2
      ## Per RGtk2/R/zzz.R, Gtk+ can only be installed interactively.
      if (!eval(pkgExists)) {
        message(msgRGtk2$line1, appendLF = verbose)

        tryCatch({
          install.packages(
            rgtk2,
            repos = rstudio(),
            INSTALL_opts = archOpts,
            quiet = !verbose,
            verbose = verbose
          )
          message("Done")
        },
        error = function(e) {
          message("Failed")
          abortRgtk2()
        },
        warning = function(w) {
          wrnmsg <- "cannot remove prior installation of package 'RGtk2'"
          if (conditionMessage(w) == wrnmsg)
            abortRgtk2()
        },
        finally = return(message(msgRGtk2$line2)))
      }
    }

    ## Avoid repeat installations via an early return
    ## If we're dealing with RGtk2, just stop the script
    ## and install Gtk+ interactively, if it is required.
    if (eval(pkgExists)) {
      message(sQuote(name), " is already installed")
      if (isRGtk2) {

        # Consider that directory may be a dud.
        if (!dir.exists(local_gtk_bin_path(rgtk2))) {
          message(msgRGtk2$line2)
          stop('Execution stopped.', call. = FALSE)
        }
      }
      return()
    }

    ## Grab the package archives as desired.
    ## When installing them, the following are considered:
    ##   => asking for upgrade interrupts installation
    ##   => install only one of 32- or 64-bit, not both
    ## But first, if RGtk2 is not present, there's no
    ## point trying to install packages that depend on it.
    isRgtkDep <- (name == 'gWidgetsRGtk2' || name == 'RQDA')
    rgtkNotReady <- !(rgtk2 %in% .packages(all.available = TRUE) &&
                       dir.exists(local_gtk_bin_path(rgtk2)))
    if (isRgtkDep && rgtkNotReady) {
      message(sQuote(name), " was not installed because RGtk2 is not ready")
      return()
    }

    tryCatch({
      msg <- sprintf("Installing '%s' ... ", name)
      message(msg, appendLF = verbose)

      if (!isRGtk2)
        devtools::install_version(
          name,
          ver,
          repos = cran.index(),
          quiet = !verbose,
          upgrade = "never",
          INSTALL_opts = archOpts
        )
      message("Done")
    },
    error = function(e) {
      message("Failed")
    })

  } # end inst()

  pkgversions <- c(RGtk2 = "2.20.36",
                   gWidgets = '0.0-54.2',
                   gWidgetsRGtk2 = '0.0-86',
                   RQDA = '0.3-1')

  invisible(Map(inst, names(pkgversions), pkgversions))

}
