#' Detach all non-essential objects from all environments
#' 
#' This function detaches all non-essential objects from all environments and is 
#' a simple rewrite of a function available through the codes provided by
#' the Choice Modeling Centre in Leeds.
#' 
#' @examples
#' detach_all()
#' 
#' @export

detach_all <- function () {
  subset_environment <- substring(search(), 1L, 8L) != "package:" &
    search() != ".GlobalEnv" & search() != "Autoloads" & 
    search() != "CheckExEnv" & search() != "tools:rstudio" &
    search() != "TempEnv"
  
  object_positions <- (1L:length(search()))[subset_environment]
  
  for (i in 1L:length(object_positions)) {
    if (length(object_positions) > 0L){
      detach(pos = object_positions[1L])
      object_positions <- (1L:length(search()))[subset_environment]
    }
  }
}
