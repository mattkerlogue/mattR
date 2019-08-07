

#' @title Clear the global environment
#'
#' @description A function to quickly clear the global environment from the
#' console.
#'
#' @export
#'
#' @examples
#' clear_global()
clear_global <- function() {

  objs <- ls(pos = .GlobalEnv)
  n_objs <- length(objs)

  message <- paste0("There are ", n_objs, " in the global environment. ",
               "Are you sure you want to clear the environment? (y/n): ")

  x <- tolower(readline(message))

  if (x == "y" | x == "yes") {
    rm(list = objs, pos = .GlobalEnv)
    message("Environment clear, ", n_objs, " objects removed.")
  } else{
    message("Environment not cleared, ", n_objs," objects remain.")
  }

}

