popLine <- function(bar_dt,
                    popCol_v = "variable", pop_v, 
                    patientCol_v = "Patients", ptColors_v,
                    xVar_v = "Time point", yVar_v = "value", 
                    yLab_v = "Pct CD45",
                    title_v) {
  #' Prop and Func Bar
  #' @description Line/spaghetti plot of functional expression
  #' @param bar_dt barchart table (melted)
  #' @param popCol_v column that holds populations. Should be 'variable' in melted data
  #' @param pop_v which population to plot
  #' @param markerColors_v color vector for marker color scale
  #' @param patientCol_v column for patients (should always be 'Patients')
  #' @param ptColors_v named color vector for patients
  #' @param xVar_v column to put on x-axis. Should be 'Time point'
  #' @param yVar_v column to use for y-axis. Should be 'value'
  #' @param title_v title for plot
  #' @import data.table
  #' @import ggplot2
  #' @return ggplot
  #' @export
  
  ### Subset data
  sub_dt <- bar_dt[get(popCol_v) %in% pop_v,]
  
  ### Plots
  plot_gg <- ggplot(data = sub_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), color = !!sym(patientCol_v), group = !!sym(patientCol_v))) +
    geom_point(cex = 1.5) +
    geom_line(size = 1.5) +
    scale_color_manual(values = ptColors_v, breaks = names(ptColors_v)) +
    #massive_label() +
    big_label() + angle_x() +
    ggtitle(title_v) +
    labs(y = yLab_v, x = NULL)
  
  return(plot_gg)
  
} # popLine