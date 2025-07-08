propAndFuncBar <- function(data_dt, func_dt,
                           markerCol_v = "marker", markers_v, markerColors_v,
                           patientCol_v = "Patients", pt_v,
                           timeCol_v = "Time point", time_v,
                           xVar_v = "variable", yVar_v = "value", 
                           title_v, allOut_v = F) {
  #' Prop and Func Bar
  #' @description Combo plot showing stacked bar of cell proportions and
  #' regular bar of marker functionalitty
  #' @param data_dt cell proportion table
  #' @param func_dt functional table
  #' @param patientCol_v column for patients (should always be 'Patients')
  #' @param pt_v which patient to plot
  #' @param timeCol_v column for timepoints (should always be 'Time point')
  #' @param time_v which time to plot
  #' @param markerCol_v column for functional markers (should always be 'marker')
  #' @param markers_v which markers to plot
  #' @param markerColors_v color vector for marker color scale
  #' @param title_v title for plot
  #' @param allOut_v logical indicating whether to output list of stuff or just the combined plot
  #' @return either list or ggplot
  #' @export
  
  ### Subset data
  sub_dt <- data_dt[get(patientCol_v) == pt_v & 
                      get(timeCol_v) == time_v,]
  
  subFunc_dt <- func_dt[get(patientCol_v) == pt_v & 
                          get(timeCol_v) == time_v & 
                          get(markerCol_v) %in% markers_v,]
  
  ### Make sure populations match
  pops_v <- intersect(sub_dt[[xVar_v]], subFunc_dt[[xVar_v]])
  sub_dt <- sub_dt[get(xVar_v) %in% pops_v,]
  subFunc_dt <- subFunc_dt[get(xVar_v) %in% pops_v]
  
  ### Plots
  proportions_gg <- ggplot(data = sub_dt, aes(x = !!sym(patientCol_v), y = !!sym(yVar_v), fill = !!sym(xVar_v))) +
    geom_bar(stat = "identity") + 
    my_theme() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(x = NULL, y = "Cell Proportion", fill = "Cell Type")
  
  functional_gg <- ggplot(data = subFunc_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), fill = !!sym(markerCol_v))) +
    geom_bar(stat = "identity", position = "dodge") +
    my_theme() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = markerColors_v, breaks = names(markerColors_v)) +
    labs(x = NULL, y = "Percent Functional")
  
  ### Legends
  propLegend_gg <- wrh.rUtils::g_legend(proportions_gg)
  funcLegend_gg <- wrh.rUtils::g_legend(functional_gg)
  
  ### Remove legends
  proportions_gg <- proportions_gg + theme(legend.position = "none")
  functional_gg <- functional_gg  + theme(legend.position = "none")
  
  ### Combine Plots
  comboPlot_gg <- ggpubr::ggarrange(proportions_gg, functional_gg, widths = c(1,4))
  
  comboPlot_gg <- ggpubr::annotate_figure(comboPlot_gg,
                                          top = ggpubr::text_grob(label = title_v))
  
  ### Combine Legends
  comboLegend_gg <- ggpubr::ggarrange(propLegend_gg, funcLegend_gg, ncol = 1)
  
  ### Combine both
  combo_gg <- ggpubr::ggarrange(comboPlot_gg, comboLegend_gg, ncol = 1, nrow = 2, heights = c(2,1))
  
  ### Full output
  if (allOut_v) {
    out <- list("annot" = combo_gg, "plot" = comboPlot_gg, "legend" = comboLegend_gg)
  } else {
    out <- combo_gg
  }
  
  return(out)
  
} # propAndFuncBar