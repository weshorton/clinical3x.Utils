sortTimePoints <- function(timePoints_v) {
  #' Sort Time Points
  #' @description Sort timepoints into correct order
  #' @param timePoints_v vector of time points used
  #' @details this is specifically for timepoints of C#D# format
  #' (e.g. C1D1, C2D8, etc.). First sort by cycle, then by days within each cycle
  #' @return ordered vector
  #' @export
  
  ### Get all the different cycles
  toSplit_v <- unique(gsub("D.*$", "", timePoints_v))
  
  ### Sort them
  toSplit_v <- toSplit_v[order(as.numeric(gsub("C", "", toSplit_v)))]
  
  timePoints_lsv <- sapply(toSplit_v, function(x) {
    ### Subset for them
    y <- grep(x, timePoints_v, value = T)
    ### Sort on their days
    y <- y[order(as.numeric(gsub("C[0-9]D", "", y)))]
    return(y)
  }, simplify = F, USE.NAMES = F)
  return(unlist(timePoints_lsv))
} # sortTimePoints