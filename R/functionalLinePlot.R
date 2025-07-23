functionalLine <- function(func_dt,
                           markerCol_v = "marker", markers_v, markerColors_v,
                           patientCol_v = "Patients", pt_v,
                           xVar_v = "Time point", yVar_v = "value", 
                           title_v) {
  #' Functional Line
  #' @description Line/spaghetti plot of functional expression
  #' @param func_dt functional table
  #' @param markerCol_v column for functional markers (should always be 'marker')
  #' @param markers_v which markers to plot
  #' @param markerColors_v color vector for marker color scale
  #' @param patientCol_v column for patients (should always be 'Patients')
  #' @param pt_v which patient to plot
  #' @param xVar_v column to put on x-axis. Should be 'Time point'
  #' @param yVar_v column to use for y-axis. Should be 'value'
  #' @param title_v title for plot
  #' @import data.table
  #' @import ggplot2
  #' @return ggplot
  #' @export
  
  ### Subset data
  subFunc_dt <- func_dt[get(patientCol_v) == pt_v & 
                          get(markerCol_v) %in% markers_v,]
  
  ### Plots
  plot_gg <- ggplot(data = subFunc_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), color = !!sym(markerCol_v), group = !!sym(markerCol_v))) +
    geom_point(cex = 1.5) +
    geom_line(size = 1.5) +
    scale_color_manual(values = markerColors_v, breaks = names(markerColors_v)) +
    big_label(angleY_v = F) + angle_x() +
    #massive_label() +
    ggtitle(title_v) +
    labs(y = "% Functional", x = NULL)
  
  return(plot_gg)
  
} # Functional Line

functionalLineFacet <- function(func_dt,
                                markerCol_v = "marker", markers_v, markerColors_v,
                                patientCol_v = "Patients", pt_v,
                                classCol_v = "variable",
                                xVar_v = "Time point", yVar_v = "value", 
                                title_v) {
  #' Functional Line Facet
  #' @description Line/spaghetti plot of functional expression
  #' @param func_dt functional table
  #' @param markerCol_v column for functional markers (should always be 'marker')
  #' @param markers_v which markers to plot
  #' @param markerColors_v color vector for marker color scale
  #' @param patientCol_v column for patients (should always be 'Patients')
  #' @param pt_v which patient to plot
  #' @param classCol_v column that holds cell classes. Used for facet
  #' @param xVar_v column to put on x-axis. Should be 'Time point'
  #' @param yVar_v column to use for y-axis. Should be 'value'
  #' @param title_v title for plot
  #' @import data.table
  #' @import ggplot2
  #' @return ggplot
  #' @export
  
  ### Subset data
  subFunc_dt <- func_dt[get(patientCol_v) == pt_v & 
                          get(markerCol_v) %in% markers_v,]
  
  ### Formula
  formula_v <- as.formula(paste0("~", "`", classCol_v, "`"))
  
  ### Plots
  plot_gg <- ggplot(data = subFunc_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), color = !!sym(markerCol_v), group = !!sym(markerCol_v))) +
    geom_point(cex = 1.5) +
    geom_line(size = 1.5) +
    facet_wrap(formula_v, axes = "all_x") +
    scale_color_manual(values = markerColors_v, breaks = names(markerColors_v)) +
    #big_label(angleY_v = F) + angle_x() +
    massive_label() +
    ggtitle(title_v) +
    labs(y = "Percent Functional", x = NULL)
  
  return(plot_gg)
  
} # propAndFuncBar
