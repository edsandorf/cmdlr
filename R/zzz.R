#' Print package startup message
#' 
#' The function is called when the package is loaded through library or require.
#' 
#' @param libname Library name
#' @param pkgname Package name
#' 
#' @return Nothing

.onAttach <- function (libname, pkgname) {
  installed_version <- utils::packageDescription("cmdlr", fields = "Version")
  
  description <- tryCatch({
    readLines("https://raw.githubusercontent.com/edsandorf/cmdlR/master/DESCRIPTION")
  }, warning = function(w) {
    return("NA")
  }, error = function(e) {
    return("NA")
  })
  
  if (length(description) == 1) {
    remote_version <- description
  } else {
    remote_version <- gsub("Version:\\s*", "", description[grep('Version:', description)])    
  }

  logo <- paste0("**************************************************************", "\n",
            "                                             _ _ _ _ _ _ _","\n",
            "                                            /             \\  ","\n",
            "                                            |      _ _     \\","\n",
            "      _ _ _ _ _ _ _ _ _ _ _ _ _ _ _   _ _   |     /   \\     \\ ","\n",
            "    /        |             |       \\ |   |  |     |   |     |","\n",
            "   /    _ _ _|             |   _    \\|   |  |     \\_ _/     / ","\n",
            "  /    /   |    ^     ^    |  |  \\   \\   |  |              /","\n",
            " |    |    |   | |   | |   |  |   |   |  |  |      _ _     \\  ","\n",
            " |    |    |   | |   | |   |  |   |   |  |  |     /   \\     \\","\n",
            " |    |    |   | |   | |   |  |   |   |  |  |    /     \\     \\","\n",
            "  \\    \\ _ |_  | |   | |   |  |_ /   /   |_ |_ _ |_    |     |","\n",
            "   \\         | | |   | |   |        /|             |   |     |","\n",
            "    \\ _ _ _ _|_| |_ _| |_ _|_ _ _ _/ |_ _ _ _ _ _ _|   |_ _ _|","\n",
            "","\n", "***************************************************************"
  )

  packageStartupMessage(logo, "\n\n",
    "Thank you for using cmdlr! \n\n",
    "You are currently using version: ",
    installed_version, "\n\n",
    "The latest version is: ", remote_version, "\n\n",
    "To access the latest version, please run \n",
    "devtools::install_github('edsandorf/cmdlr') \n\n",
    
    "To cite this package: \n",
    "utils::citation('cmdlr')")
}
