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
    curr_dt <- data_dt[population == currPop_v]
    
    ### Make plot
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
    
    ### Extract legend
    currLegend_gg <- g_legend(curr_gg)
    curr_gg <- curr_gg + theme(legend.position = "none")
    
    ### Make axis
    labels_v <- sortTimePoints(as.character(unique(c(curr_dt$`Time point_1`, curr_dt$`Time point_2`))))
    axis_gg <- ggplot() +
      geom_point(aes(x = 1, y = 1), alpha = 0) +
      scale_x_continuous(limits = c(1, 4), breaks = 1:4, labels = c(labels_v)) +
      theme_void() +
      theme(axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_text(color = "black", hjust = 0.1, size = 14),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            margins = margin(t = -10, r = 0.5, b = 0, l = 0.5, "points"))
    
    ### Remove Bottom Margin
    curr_gg <- curr_gg + theme(margins = margin(t = 5.5, r = 5.5, b = -30, l = 5.5, "points"))
    
    ### Combine axis
    combo1_gg <- ggpubr::ggarrange(curr_gg, axis_gg, ncol = 1, heights = c(8, 1))
    
    ### Combine legend
    combo2_gg <- ggpubr::ggarrange(combo1_gg, currLegend_gg, ncol = 2, widths = c(5, 1))
    
    ### Combine table - tbd
    #ggarrange(combo2_gg, gridExtra::tableGrob(out_df), ncol = 1, nrow = 2, heights = c(5, 1))
    
    out_lsgg[[currPop_v]] <- combo2_gg
  } # for i
  
  return(out_lsgg)
  
} # plotCycleCompareLine




# ### Table of occurences?
# ### Work in Progress
# toComp_v <- unique(curr_dt$Compare)
# tmp_ls <- list()
# vals_v <- c("DN", "NC", "UP")
# for (j in 1:length(toComp_v)) {
#   
#   ### Make a table
#   currComp_v <- toComp_v[j]
#   currSub_dt <- curr_dt[Compare == currComp_v,]
#   currTable_df <- as.data.frame.matrix(table(currSub_dt[variable == "value_1", mget(c("Direction", lineVar_v))]))
#   
#   ### Fill in any missing ones
#   for (v_v in vals_v) {
#     if (!(v_v %in% rownames(currTable_df))) currTable_df <- rbind(currTable_df, data.frame("CB" = 0, "PD" = 0, row.names = v_v))
#   } # for v_v
#   
#   ### Sort
#   currTable_df <- currTable_df[match(rownames(currTable_df), vals_v),]
#   
#   ### Try adding a spacer
#   if (j != length(toComp_v)) {
#     tmp_df <- data.frame("X" = rep(" ", nrow(currTable_df)))
#     currTable_df <- cbind(currTable_df, tmp_df)
#     colnames(currTable_df)[colnames(currTable_df) == "X"] <- ""
#   }
#   
#   ### Output
#   tmp_ls[[j]] <- currTable_df
#   
# }
# 
# ### Bind
# out_df <- do.call(cbind, tmp_ls)
# 
# ### Remove spacer column names
# colnames(out_df) <- gsub("Var\\.[0-9]", "", colnames(out_df))
