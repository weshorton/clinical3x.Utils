plotCycleCompareLine <- function(data_dt, dataCols_v, 
                                 colors_v = c("UP" = "#47AD48", "DN" = "#D7222B", "NC" = "#0A0B09"),
                                 xVar_v = "variable", yVar_v = "value", lineVar_v = "CB",
                                 groupVar_v = "Patients", colorVar_v = "Direction") {
  #' Plot Cycle Compare Line
  #' @description Pairwise lineplot between different timepoints, 
  #' with lines colored by their change direction
  #' @param data_dt data.table that has been run through calcAndClassifyChanges (must have Compare column)
  #' @param dataCols_v columns to plot
  #' @param colors_v color vector for direction
  #' @param xVar_v x-axis variable (should always be 'variable' if calcAndClassifyChanges has been run)
  #' @param yVar_v y-axis variable (should always be 'value' if calcAndClassifyChanges has been run)
  #' @param lineVar_v variable to modify linetype. Should be "CB" or "RECIST" likely
  #' @param groupVar_v variable to connect lines
  #' @param colorVar_v name of variable to color by (should be 'Direction' if calcAndClassifyChanges has been run)
  #' @details
  #' This should only be run after running calcAndClassifyChanges, otherwise it's doubtful that data_dt will be
  #' correct
  #' @return list of plots (one for each element in dataCols_v)
  #' @export
  
  out_lsgg <- list()
  for (i in 1:length(dataCols_v)) {
    
    currPop_v <- dataCols_v[i]
    curr_dt <- plot_dt[population == currPop_v]
    
    curr_gg <- ggplot(data = curr_dt, 
                      aes(x = !!sym(xVar_v), y = !!sym(yVar_v), group = !!sym(groupVar_v), color = !!sym(colorVar_v))) +
      geom_line(linewidth = 1, aes(linetype = !!sym(lineVar_v))) +
      facet_wrap(~ Compare, nrow = 1) +
      scale_x_discrete(limits = c("value_1", "value_2"), expand = c(0,0)) +
      labs(x = NULL, y = NULL) +
      my_theme() + ggtitle(paste0(currPop_v, " - Cycle Changes")) +
      scale_color_manual(values = colors_v, breaks = names(colors_v)) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(angle = 0, size = 11))
    
    out_lsgg[[currPop_v]] <- curr_gg
  } # for i
  
  return(out_lsgg)
  
} # plotCycleCompareLine