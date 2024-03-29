% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_file.R
\name{new_file}
\alias{new_file}
\title{Create new files}
\usage{
new_file(
  path,
  open = TRUE,
  template = NULL,
  create_dir = TRUE,
  overwrite = NULL
)
}
\arguments{
\item{path}{Path of the file to create.}

\item{open}{If `TRUE` (the default) open the file.}

\item{template}{A template file to copy.}

\item{create_dir}{If `TRUE` (the default) will create missing directories.}

\item{overwrite}{When `NULL` (the default) will warn and prompt the user if
attempting to overwrite, set to `TRUE` to force overwrite or `FALSE` to
not overwrite.}
}
\value{
The path to the new file (invisibly).
}
\description{
A convience wrapper around `fs::file_create()` and `fs::file_copy()` to
create new files, and (defaultly) open them in an editor.
}
\details{
This function is a replacement for `rstudioapi::documentNew()`, which only
allows you to create files of the type `r`, `rmarkdown` and `sql`. Instead,
`new_file()` allows you to create any empty file of any type, or use an
existing file as a template, and assuming in an interactive session to open
that file in an editor.
}
