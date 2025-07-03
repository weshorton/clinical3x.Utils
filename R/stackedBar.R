facetStackedBar <- function(data_dt, xVar_v, facetVar_v,
                       yVar_v = "value", fillVar_v = "variable", 
                       title_v = NULL, colors_v = NULL) {
  #' Stacked Bar
  #' @description
    #' Stacked bar of different populations
  #' @param data_dt melted data.table
  #' @param xVar_v variable to put on x-axis (should be 'Patients' or 'Time point')
  #' @param facetVar_v variable to facet by (should be opposite of xVar_v, i.e. 'Time point' if xVar_v == 'Patients')
  #' @param yVar_v variable to put on y-axis (should always be 'value')
  #' @param fillVar_v variable to fill stacked bars b y (should always be 'variable')
  #' @param title_v optional plot title
  #' @param colors_v named color vector that matches values in fillVar_v (optional, but should be provided)
  #' @details
    #' This is made to make two different versions:
    #' 1. Facet by time, patients on the x-axis
    #' 2. Facet by patients, time on the x-axis
  #' @return ggplot
  #' @export
  
  ### Make facet expression
  facet_v <- as.formula(paste0("~", "`", facetVar_v, "`"))
  
  ### Make base plot
  plot_gg <- ggplot(data = data_dt, 
                    aes(x = !!sym(xVar_v), y = !!sym(yVar_v), fill = !!sym(fillVar_v))) +
    geom_bar(position = "stack", stat = "identity") +
    my_theme() + angle_x() + labs(y = "Population Pct")
  
  ### Add colors
  if (!is.null(colors_v)) {
    plot_gg <- plot_gg + scale_fill_manual(values = colors_v, breaks = names(colors_v))
  } # fi
  
  ### Change facet depending
  if (facetVar_v == "Patients") {
    plot_gg <- plot_gg + facet_wrap(facet_v, ncol = 1)
  } else if (facetVar_v == "Time point") {
    plot_gg <- plot_gg + facet_wrap(facet_v)
  } else {
    stop("Only support 'Patients' and 'Time point' as facet variables")
  } # fi
  
  ### Add Title
  if (!is.null(title_v)) {
    plot_gg <- plot_gg + ggtitle(title_v)
  } # fi
  
  ### Output
  return(plot_gg)
    
} # facetStackedBar

stackedBar <- function(data_dt, 
                       xVar_v = "Time point", patientCol_v = "Patients", popCol_v = "variable",
                       patient_v, pops_v = NULL,
                       yVar_v = "value", fillVar_v = "variable",
                       title_v = NULL, colors_v = NULL) {
  #' Stacked Bar
  #' @description
  #' Stacked bar of different populations
  #' @param data_dt melted data.table
  #' @param patientCol_v column name specifying different patients. Should be 'Patients' always.
  #' @param popCol_v column name specifying different populations. Should always be 'variable'
  #' @param patient_v specific value in patientCol_v to subset for
  #' @param pops_v specific populations to display, if NULL (default), will use all available
  #' @param xVar_v variable to put on x-axis (should be 'Patients' or 'Time point')
  #' @param yVar_v variable to put on y-axis (should always be 'value')
  #' @param fillVar_v variable to fill stacked bars b y (should always be 'variable')
  #' @param title_v optional plot title
  #' @param colors_v named color vector that matches values in fillVar_v (optional, but should be provided)
  #' @import data.table
  #' @import ggplot2
  #' @return ggplot
  #' @export

  ### Subset data for patient
  sub_dt <- data_dt[get(patientCol_v) == patient_v, ]
  
  ### Subset data for measure vars
  if (!is.null(pops_v)) {
    sub_dt <- sub_dt[get(popCol_v) %in% pops_v,]
  } # fi
  
  ### Make base plot
  plot_gg <- ggplot(data = sub_dt, 
                    aes(x = !!sym(xVar_v), y = !!sym(yVar_v), fill = !!sym(fillVar_v))) +
    geom_bar(position = "stack", stat = "identity") +
    # my_theme() + 
    massive_label() +
    #big_label(angleY_v = F) +
    angle_x() + labs(y = "Population Pct") #+
    #theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 2)) # messing with different legend formats
  
  ### Add colors
  if (!is.null(colors_v)) {
    plot_gg <- plot_gg + scale_fill_manual(values = colors_v, breaks = names(colors_v))
  } # fi
  
  ### Add Title
  if (!is.null(title_v)) {
    plot_gg <- plot_gg + ggtitle(title_v)
  } # fi
  
  ### Output
  return(plot_gg)
  
} # stackedBar




