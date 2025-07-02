cbTimeSeries <- function(data_dt, yVar_v, xVar_v = "Time point", 
                         pairVar_v = "Patients", shapeVar_v = "Patients", lineVar_v = "CB",
                         colorVar_v = "CB", colorColors_v = cbColors_v,
                         fillVar_v = "Time point", fillColors_v = timeColors_v) {
  #' Pairwise Boxplot
  #' @description Pairwise boxplot for clinical data
  #' @param data_dt table of population values and a few meta columns
  #' @param yVar_v variable to use for y-axis (should be some population column name, e.g. "Live cells")
  #' @param xVar_v variable to put on x-axis (usually 'Time point')
  #' @param pairVar_v variable to connect paired samples (usually "Patients")
  #' @param shapeVar_v variable to shape points by (usually "Patients")
  #' @param lineVar_v variable to change linetypes
  #' @param colorVar_v variable to color LINES by (usually "Patients")
  #' @param colorColors_v named color vector to use as color scale for color variable
  #' @param fillVar_v variable to color BOXES by (usually "Time point")
  #' @param fillColors_v named color vector to use as color scale for fill variable
  #' @return ggplot
  #' @export
  
  ###
  ### Line with points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Base Plot
  currLinePoint_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), 
                                                 y = !!sym(yVar_v), 
                                                 group = !!sym(pairVar_v), 
                                                 color = !!sym(colorVar_v)))
  
  ### Add vertical lines to distinguish cycles
  currLinePoint_gg <- currLinePoint_gg +
    geom_vline(xintercept = "C1D-8", color = "lightgrey") +
    geom_vline(xintercept = "C2D1", color = "lightgrey") +
    geom_vline(xintercept = "C3D1", color = "lightgrey") +
    geom_vline(xintercept = "C4D1", color = "lightgrey")
    
  ### Add lines, points, and color
  currLinePoint_gg <- currLinePoint_gg +
    geom_line(aes(linetype = !!sym(lineVar_v))) + 
    geom_point(aes(shape = !!sym(shapeVar_v)), size = 2) +
    scale_color_manual(values = colorColors_v, breaks = names(colorColors_v))
  
  ### Add theme stuff
  currLinePoint_gg <- currLinePoint_gg +
    my_theme() + angle_x() +
    ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent") 
  
  return(currLinePoint_gg)
  
  # ### Box with shaped points
  # currBoxShapedPoint_gg <- ggplot(data = data_dt, 
  #                                 aes(x = !!sym(xVar_v), y = !!sym(yVar_v), fill = !!sym(fillVar_v))) + 
  #   geom_boxplot() +
  #   scale_fill_manual(values = colors_v, breaks = names(colors_v)) +
  #   geom_point(aes(shape = !!sym(pairVar_v)), size = 3) + 
  #   my_theme() + angle_x() +
  #   ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent") +
  #   labs(subtitle = "box + shaped points (ok)")
  # 
  # out_ls <- list("line" = currLinePoint_gg, "box" = currBoxShapedPoint_gg)
  # 
  # return(out_ls)
  
} # pairTimeSeries
  