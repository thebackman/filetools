<!-- README.md is generated from README.Rmd. Please edit that file -->
filetools
=========

The purpose of this package is to provide some functionality that can be handy when you have a number of data files and you want to check for the existence of certain variables (names) within these files.

In a perfect world with perfect data documentation this would no be needed but in practice people do not always know which (of the many versions) of their files that contains certain variables.

### Installation

To install from GitHub use:

``` r
devtools::install_github("thebackman/filetools")
```

### Usage

To find variables within a file use the `find_vars_in_files` function. The syntax is easy. There are only five arguments:

1.  absolute path to folder with sav / csv-files
2.  include subfolders, TRUE / FALSE
3.  what kind of files do we have (SPSS = sav, csv = csv)
4.  what string to search for
5.  print HTML-report

``` r
# to test it with the installed example data files, use
find_vars_in_files(
  path = system.file("extdata", package="filetools"),
  subf = F,
  file_type = "sav",
  searchstring = "id",
  HTML_out = F)

find_vars_in_files(
  path = system.file("extdata", package="filetools"),
  subf = F,
  file_type = "sav",
  searchstring = "fruit",
  HTML_out = F)
```

The output produced shows the number of files found in your target directory, the number of files in which a matching variable name was found, the name of the matched variables in each file and the first five observations for each matched variable.

### Details

The HTML-file is called *files.html*. The search string is just a regular expression that allows arbitray characters before and after the provided string.

### Future

In the future I might add some matching possibility, searching of labels, optional variable removal etcetera.

### Notes

I hope you find what you are looking for :-)
