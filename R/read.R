globalVariables(c("doc_id", "txt_id"))

#' Create Text Files from Word
#'
#' Read text from a Word document and convert to plain text format
#'
#' @param destdir Destination path. Where the files will be saved.
#' @param docxfiles Character vector containing the filepath(s) of
#' the Word documents to be converted.
#'
#' @details When \code{destdir} is unchanged, the function uses the current
#' working directory as the location for saving any files converted from
#' Word.
#'
#' @import stringr
#' @importFrom dplyr mutate
#' @importFrom readtext readtext
#' @importFrom purrr walk2
#'
#' @return A character vector containing the paths of the generated text files.
#'
#' @export
read_transcript <- function(destdir, docxfiles) {
  if (is.null(destdir) || missing(destdir))
    destdir <- getwd()

  tryCatch({
    destdir <- normalizePath(destdir)
  },
  warning = function(warn) {
    stop(sprintf("Directory '%s' does not exist", basename(destdir)))
  })

  # TODO: Add '...' to control this function from without?
  docdt <- readtext::readtext(docxfiles)

  docdt <- docdt %>%
    mutate(txt_id = str_replace(doc_id, '(.+)(\\.docx?$)', '\\1.txt')) %>%
    mutate(txt_id = .makeSafeNames(txt_id))

  ## This function needs to be defined here so that
  ## it can find 'destdir'. Its purpose is to create
  ## the eventual filepath of the text file and return it.
  .createFileAndReturnPath <- function(txt, fname) {
    fpath <- file.path(destdir, fname)
    cat(txt, file = fpath)
    normalizePath(fpath, winslash = "/")
  }

  docdt$text %>%
    purrr::map2_chr(docdt$txt_id, .createFileAndReturnPath) %>%
    invisible
}



 .makeSafeNames <- function(str, to.lower = FALSE) {
    # stopifnot(is.character(str))
    str <- str %>%
      str_trim %>%
      str_squish %>%
      str_replace_all("\\s|\\?|\\!|\\*|\\^|&", '-') %>%
      str_replace_all('-{2,}', '-')

    if (to.lower)
      str <- str_to_lower(str)
    str
  }



