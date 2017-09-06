# cons_html ---------------------------------------------------------------

# create a HTML-file from a pre defined R-markdown template
cons_html <- function(res) {
  dir_write <- getwd()
  rmd_file <- system.file("rmd/rmd_file.rmd", package="filetools")
  # rmd_file <- file.path(getwd(), "inst", "rmd", "rmd_file.rmd")  # testpath
  rmarkdown::render(input = rmd_file,
                    output_file = "files.html",
                    output_dir = dir_write,
                    encoding = "UTF-8",
                    params = list(res = res))
}

# read_infile -------------------------------------------------------------

# switch indata read functions depending on file type
read_infile <- function(x, type) {
  switch(type,
         sav = haven::read_sav(x),
         csv = readr::read_csv2(x))
}

# extract_five ------------------------------------------------------------

# extracting the first five positions as charvar for each matched variable
extract_five <- function(df, pos_to_extract) {
  if (nrow(df) > 5) {
    df_lim <- df[1:5, pos_to_extract, drop = F]
  } else {
    df_lim <- df[pos_to_extract]
  }
  out <- purrr::map_chr(df_lim, function(x) {
    paste0(as.character(x), collapse = " ")
  })
}
