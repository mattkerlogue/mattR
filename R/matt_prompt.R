#' Customised R prompt
#'
#' A function for use in an .Rprofile to update the R console prompt.
#'
#' Add the following to the .First() function in your .Rprofile file:
#'   mattR::matt_prompt()
#'   addTaskCallback(mattR::matt_prompt)
#'
#' @export
matt_prompt <- function() {

  proj_path <- here::here()
  my_loc <- getwd()

  if (!is.null(proj_path)) {

    if (grepl(proj_path, my_loc)) {

      my_base <- basename(proj_path)

      my_loc <- paste0(my_base, gsub(proj_path,  "", my_loc),
                       collapse = .Platform$file.sep)

    } else {

      home <- Sys.getenv("HOME")

      my_loc <- paste0("!! ", gsub(home, "~", my_loc))
    }
  }

  git_branch <- suppressWarnings(system("git rev-parse --abbrev-ref HEAD",
                                        ignore.stderr = TRUE, intern = TRUE))

  if (length(git_branch) != 0) {
    git_msg <- paste0(" @", git_branch)
    git_status <- suppressWarnings(system("git status -s",
                                          ignore.stderr = TRUE, intern = TRUE))
    git_ahead <- suppressWarnings(system("git status -sb",
                                         ignore.stderr = TRUE, intern = TRUE))
    git_ahead_chk <- grepl("ahead", git_ahead)

    if (length(git_status) != 0) {
      git_msg <- paste0(git_msg, " \u2718")
    } else if (git_ahead_chk) {
      git_msg <- paste0(git_msg, " \u2b06")
    }

  } else {
    git_msg <- ""
  }

  console_msg <- paste0("[",
                        format(Sys.time(), "%H:%M"),
                        " ", my_loc, "/",
                        git_msg,
                        "] > ")

  options(prompt = console_msg)

  invisible(TRUE)

}
