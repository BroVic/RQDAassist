#' Read text from a Wprd document and convert to plain text format
#'
#' @param destdir Destination path. Where the files will be saved.
#' @param docxfiles Character vector containing the filepath(s) of
#' the Word documents to be converted.
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
  docdt <- readtext::readtext(docxfiles)

  make_safe_names <- function(str, to.lower = FALSE) {
    # stopifnot(is.character(str))
    str <- str %>%
      str_trim() %>%
      str_squish() %>%
      str_replace_all("\\s|\\?|\\!|\\*|\\^|&", '-') %>%
      str_replace_all('-{2,}', '-')

    if (to.lower)
      str <- str_to_lower(str)
    str
  }

  docdt <- docdt %>%
    mutate(txt_id = str_replace(doc_id, '(.+)(\\.docx?$)', '\\1.txt')) %>%
    mutate(txt_id = make_safe_names(txt_id))

  .createFileAndReturnPath <- function(txt, fname) {
    fpath <- file.path(destdir, fname)
    cat(txt, file = fpath)
    normalizePath(fpath)
  }

  docdt$text %>%
    purrr::map2_chr(docdt$txt_id, .createFileAndReturnPath)
}







