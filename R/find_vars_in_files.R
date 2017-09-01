# find_vars_in_files ------------------------------------------------------

#' Find variable names in SPSS or CSV-files
#'
#' use a search string to find variables names in sav or csv files within a
#' specified directory
#'
#' @param path the absolute path to the file folder
#' @param subf should subfolders also be searched
#' @param file_type what file type to look in (sav or csv)
#' @param searchstring the searchstring
#' @param HTML_out should a HTML-report be produced
#'
#' @return HTML-file
#' @export
find_vars_in_files <- function(path = "",
                               subf = F,
                               file_type = "sav",
                               searchstring = "",
                               HTML_out = F) {

  # does file folder exist
  if (dir.exists(path) == F) {
    stop("The folder does not seem to exist, check path", call. = F)
  }

  # list files with given ending
  files <- list.files(path = path,
                      pattern = paste0(".+\\.",file_type),
                      recursive = subf,  # sub dirs?
                      include.dirs = T,
                      full.names = T,
                      ignore.case = T)

  # check if there are any files in this folder
  if (identical(files, character(0)) == T) {
    stop("No files found in folder", call. = F)
  }

  # pick out names from each file
  if (file_type == "sav") {
    df_names <- purrr::map(files, function(x) {
      tolower(names(haven::read_sav(x)))  # sav
    })
  } else {
    df_names <- purrr::map(files, function(x) {
      tolower(names(readr::read_csv2(x)))  # csv
    })
  }

  # extract matched names from each file
  regx <- paste0("^.*", searchstring, ".*$")
  res <- purrr::map(df_names, extract_names, regx)
  res <- purrr::map(res, function(x) {
    if (identical(x, character(0)) == T) {
      "no match"
    } else {
      x
    }
  })

  # present the results
  message(stringr::str_pad("-", side = "right", width = 80, pad = "-"))
  message(length(files), " files searched...", "\n")
  purrr::walk2(files, res, function(x, y) {
    message(x)
    print(y)
  })
  message(stringr::str_pad("-", side = "right", width = 80, pad = "-"), "\n")

  # write HTML
  if(HTML_out == T) {
    message("HTML file files.html constructed")
    cons_html(files, res)
  }
}
