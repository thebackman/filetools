
# cons_html ---------------------------------------------------------------

# create a HTML-file using a data frame as the input variable into a
# pre defined rmarkdown-file
cons_html <- function(files, res) {
  dir_write <- getwd()
  rmd_file <- system.file("rmd/rmd_file.rmd", package="filetools")
  # rmd_file <- file.path(getwd(), "inst", "rmd", "rmd_file.rmd")  # testpath
  rmarkdown::render(input = rmd_file,
                    output_file = "files.html",
                    output_dir = dir_write,
                    encoding = "UTF-8",
                    params = list(files = files, res = res))
}

# return_matched_names ----------------------------------------------------

# takes a vector and a regular expression
extract_names <- function(namevec, regx) {
  name_matches <- stringr::str_subset(namevec, regx)
}


