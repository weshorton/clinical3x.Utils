splitGmfiOrFuncData <- function(data_dt) {
  #' Split Gmfi Or Functional Data
  #' @description
    #' Split into different tables
  #' @param data_dt raw data
  #' @return list of data.tables made up of input data_dt
  #' @export
  
  ### Get unique cell types
  cellTypes_v <- setdiff(unique(gsub("_.*$", "", colnames(data_dt))), "File")
  
  ### Split
  out_lsdt <- sapply(cellTypes_v, function(x) {
    grep_v <- paste0(gsub("\\+", "\\\\\\+", x), "_")
    data_dt <- data_dt[,mget(c("File", grep(grep_v, colnames(data_dt), value = T)))]
    colnames(data_dt) <- gsub("^.*_", "", colnames(data_dt))
    return(data_dt)
  }, simplify = F, USE.NAMES = T)
  
  ### Output
  return(out_lsdt)
  
} # splitGmfiOrFuncData

splitMinorPops <- function(data_dt, 
                           map_lsv = list("Bcells" = c("CD19+ CD20+ B cells", "CD19+CD20- Plasmablasts Plasma cells"),
                                          "Tcells" = c("CD4+ T cells", "CD8+ T cells", "CD8+ CD4+ double positive T cells", 
                                                       "CD8- CD4- double negative T cells"),
                                          "NKcells" = c("CD56bright CD16- NK cells", "CD56dim CD16+ NK cells", "CD56dim CD16- NK cells"),
                                          "MyeloidAPCs" = c("CD14+ CD16+ intermediary Monocytes", "CD14+ CD16- classical Monocytes", 
                                                            "CD14dim CD16+ non-classical Monocytes", "CD11c+ Myeloid DCs", 
                                                            "CD123+ CD11c+ plasmacytoid DCs"),
                                          "MyeloidDCs" = c("CD1c+ cDC2", "CD141+ cDC1"),
                                          "Granulocytes" = c("CD15+ CD49d+ eosinophils", "CD15+ CD123+ basophils", 
                                                             "CD15+ putative neutrophils"))) {
  #' Split Minor Pops
  #' @description
  #' Split into different tables
  #' @param data_dt raw data
  #' @param map_lsv named list indicating which populations to put into which output data.table
  #' @return list of data.tables made up of input data_dt
  #' @export
  
  out_lsdt <- sapply(names(map_lsv), function(x) {
    pops_v <- map_lsv[[x]]
    data_dt[,mget(c("File", pops_v))]
  }, simplify = F, USE.NAMES = T)
  
  return(out_lsdt)
  
} # splitMinorPops

splitTSubsets <- function(data_dt, split_v = c("CD4", "CD8")) {
  #' Split T Subsets
  #' @description
  #' Split into different tables
  #' @param data_dt raw data
  #' @return list of data.tables made up of input data_dt
  #' @export
  
  out_lsdt <- sapply(split_v, function(x) {
    data_dt[,mget(c("File", grep(x, colnames(data_dt), value = T)))]
  }, simplify = F, USE.NAMES = T)
  
  return(out_lsdt)
  
} # splitTSubseets