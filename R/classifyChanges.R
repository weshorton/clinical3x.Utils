calcAndClassifyChanges <- function(data_dt, comparisons_lsv, idCol_v = "Time point",
                                   metaCols_v = c("RECIST", "CB"), mergeCol_v = "Patients",
                                   measureName_v = "population", change_v = 10) {
  #' Classify Changes
  #' @description classify changes between cycles for various populations
  #' @param data_dt data in melted format (1 row per patient + timepoint + population)
  #' @param comparisons_lsv list of comparisons to calculate. List name is name of comparison, values are columns to compare
  #' @param idCol_v column that identifies unique measurements (time for us usually)
  #' @param metaCols_v meta.data columns to ignore during merging (they'll be the same in all tables)
  #' @param mergecol_v column to merge tables on
  #' @param measureName_v name to give the variable column (tbd d)
  #' @param change_v percentage change to use for bins
  #' @details
  #' There might be an easier way to do this with fewer lines, but haven't looked into it. 
  #' For all of the comparisons in comparison_lsv, calculate the difference between the two
  #' time columns, bin it, and then combine them all together
  #' @return data.table
  #' @export
  
  ### List to hold results
  tmp_lsdt <- list()
  
  ### Run for each comparison
  for (i in 1:length(comparisons_lsv)) {
    
    ### Get vars
    currName_v <- names(comparisons_lsv)[i]
    currVars_v <- comparisons_lsv[[currName_v]]
    
    ### Split table
    curr1_dt <- subMelt_dt[`Time point` == currVars_v[1],]
    curr2_dt <- subMelt_dt[`Time point` == currVars_v[2],]
    
    ### Merge
    merge_dt <- merge(curr1_dt, curr2_dt,
                      by = c(mergeCol_v, metaCols_v, measureName_v),
                      suffixes = c("_1", "_2"), sort = F)
    
    ### Calculate
    merge_dt$Change <- merge_dt$value_2 - merge_dt$value_1
    merge_dt$pctChange <- merge_dt$Change / merge_dt$value_1 * 100
    
    ### New column to identify the comparison
    merge_dt$Compare <- currName_v
    
    ### Classify
    merge_dt[, Direction := ifelse(pctChange >= change_v, 'UP',
                                   ifelse(pctChange <= -change_v, "DN", "NC"))]
    
    ### Add
    tmp_lsdt[[currName_v]] <- merge_dt
    
  } # for i
  
  ### Combine
  plot_dt <- do.call(rbind, tmp_lsdt)
  
  ### Melt values
  plotMelt_dt <- melt(plot_dt, measure.vars = c("value_1", "value_2"))
  plotMelt_dt$Compare <- factor(plotMelt_dt$Compare, levels = c(names(comparisons_lsv)))
  
  ### Output
  return(plotMelt_dt)
  
}