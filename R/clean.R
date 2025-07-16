cleanFuncCols <- function(data_dt) {
  #' Clean Functional Columns
  #' @description
    #' Simplify the names for each population(s) functional markers from the FloJo output
  #' @param data_dt input table exported from FloJo
  #' @details
  #' Clean up columns by 
  #' 1. removing everything after the |
  #' 2. Split on slash and grab last two to get population and func marker
  #' 3. Remove negative functional markers (i.e. only keep CD38+, not CD38-)
  #' 4. Get rid of trailing +'s for easier grep
  #' e.g. Debris exclusion/Single Cells/Live cells/CD45+/Non-B cells/ab CD3+ T cells/CD8+ T cells/Activated CD8+ CD45RO+ T cells/CD8+ Tcm/CD39+ | Freq. of Parent
  #' turns into CD39 in the CD8 Tcm table
  #' @return input table with new column names
  #' @export
  
  ### Get colnames
  cols_v <- colnames(data_dt)
  
  ### Remove everything after the |
  cols_v <- gsub(" \\| .*$", "", cols_v)
  
  ### Split on slash
  cols_v <- unname(sapply(cols_v, function(x) {
    if (x == "File") {
      out <- x
    } else {
      y <- strsplit(x, split = "\\/")[[1]]
      out <- paste(trimws(y[(length(y)-1)]), y[length(y)], sep = "_")
    }
    return(out)}))
  
  ### Rename
  colnames(data_dt) <- cols_v
  
  ### Remove negative columns
  keepCols_v <- grep("\\-$", cols_v, invert = T, value = T)
  data_dt <- data_dt[,mget(keepCols_v)]
  
  ### Get rid of the + at the end of the functional markers
  colnames(data_dt) <- gsub("\\+$", "", colnames(data_dt))
  
  ### There is one double-positive
  colnames(data_dt) <- gsub("Ki67\\+ ", "Ki67 ", colnames(data_dt))
  
  ### Return
  return(data_dt)
} # cleanFuncCols

cleanGmfiCols <- function(data_dt) {
  #' Clean GMFI Columns
  #' @description
  #' Simplify the names for each population and marker from the FloJo output
  #' @param data_dt input table exported from FloJo
  #' @details
  #' Clean up columns by 
  #' 1. Extract population - after last / and before |
  #' 2. Get the marker in the parenthesis at the end: (marker)
  #' 3. Make new column combining that info
  #' @return input table with new column names
  #' @export
  
  ### Get colnames
  cols_v <- colnames(data_dt)
  
  ### Get the population (after last / and before the |)
  pops_v <- gsub("^.*\\/", "", gsub(" \\| .*$", "", cols_v))
  
  ### Get the markers in parenthesis at the end
  marker_v <- gsub("\\)$", "", gsub("^.* \\(", "", cols_v))
  
  ### Combine them together
  outCols_v <- gsub("_File", "", paste(pops_v, marker_v, sep = "_"))
  
  ### Add back
  colnames(data_dt) <- outCols_v
  
  ### Output
  return(data_dt)
  
} # cleanGmfiCols

cleanStackedCols <- function(data_dt) {
  #' Clean Stacked Columns
  #' @description
  #' Simplify the names for each population and marker from the FloJo output
  #' @param data_dt input table exported from FloJo
  #' @details
  #' Clean up columns by 
  #' 1. Extract population - after last / and before |
  #' @return input table with new column names
  #' @export
  
  ### Get colnames
  cols_v <- colnames(data_dt)
  
  ### Get the population (after last / and before the |)
  pops_v <- trimws(gsub("^.*\\/", "", gsub(" \\| .*$", "", cols_v)))
  
  ### Add back
  colnames(data_dt) <- pops_v
  
  ### output
  return(data_dt)
  
} # cleanStackedCols

cleanData <- function(data_dt) {
  #' Clean Data
  #' @description
  #' Clean up data from the FloJo output
  #' @param data_dt input table exported from FloJo
  #' @details
  #' Clean up data by 
  #' 1. Removing 'Mean' and 'SD' rows
  #' 2. Replacing 'n/a' and 'n/a %' with actual NA values
  #' 3. Change percentages to actual values
  #' 4. Remove empty columns
  #' @return input table with the above changes
  #' @export
  
  ### Remove mean and SD
  data_dt <- data_dt[!(File %in% c("Mean", "SD"))]
  
  ### Fix n/a
  for (j in colnames(data_dt)) set(data_dt, which(data_dt[[j]] %in% c("n/a", "n/a %")), j, NA)
  
  ### Convert to dataframe to change percentages
  data_df <- convertDFT(data_dt, col_v = "File")
  names_v <- row.names(data_df)
  data_df <- as.data.frame(apply(data_df, 2, function(x) as.numeric(gsub(" \\%", "", x))))
  rownames(data_df) <- names_v
  
  ### Convert back to data.table
  data_dt <- convertDFT(data_df, newName_v = "File")
  
  ### Remove any fully NA columns
  toRm_v <- names(which(apply(data_dt, 2, function(x) length(which(is.na(x)))) == nrow(data_dt)))
  for (col_v in toRm_v) data_dt[[col_v]] <- NULL
  
  ### Output
  return(data_dt)
  
} # cleanData