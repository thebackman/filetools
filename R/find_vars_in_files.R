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

  # -- preprocess

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

  # -- process files

  # define search string
  regx <- paste0("^.*", searchstring, ".*$")

  # extract data, call helper functions
  res <- purrr::map(files, function(x) {

    df_in <- read_infile(x, file_type)
    df_names <- tolower(names(df_in))
    names_pos <- stringr::str_detect(df_names, regx)
    names_found <- df_names[names_pos]

    if (identical(names_found, character(0)) == T) {
      NULL
    } else {
      five <- extract_five(df_in, names_pos)
      res <- list(x, names_found, five)
    }
  })

  # remove empty list entries (files without matched variables)
  res <- purrr::compact(res)

  # -- present results

  message(stringr::str_pad("-", side = "right", width = 80, pad = "-"))
  message(length(files)," files searched,", " matches found in ", length(res), " files")
  message(stringr::str_pad("-", side = "right", width = 42, pad = "-"))

  # loop over results and print to console
  if(length(res) == 0) {
    message("no files")
  } else {
    purrr::walk(res, function(x) {
      message(x[[1]], "\n")
      message("matched variables with five first values:", "\n")
      purrr::walk2(x[[2]], x[[3]], function(y, z) {
        message(y)
        print(z)
      })
      message(stringr::str_pad("-", side = "right", width = 80, pad = "-"))
    })
  }

  # write HTML file
  if(HTML_out == T) {
    if (length(res) == 0) {
      message("no matches found, no HTML constructed")
    } else {
      message("HTML file files.html constructed")
      cons_html(res)
    }
  }
}
