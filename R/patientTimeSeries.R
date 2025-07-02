patientTimeSeries <- function(data_dt, yVar_v, xVar_v = "Time point", D1Lines_v = T,
                              pairVar_v = "Patients", shapeVar_v = "Patients", lineVar_v = "Patients",
                              colorVar_v = "Patients", colorColors_v = ptColors_v,
                              fillVar_v = "Time point", fillColors_v = timeColors_v) {
  #' Pairwise Boxplot
  #' @description Pairwise boxplot for clinical data
  #' @param data_dt table of population values and a few meta columns
  #' @param yVar_v variable to use for y-axis (should be some population column name, e.g. "Live cells")
  #' @param xVar_v variable to put on x-axis (usually 'Time point')
  #' @param D1Lines_v logical indicating if vertical grey lines should be made at each D1 (to help distinguish cycle starts)
  #' @param pairVar_v variable to connect paired samples (usually "Patients")
  #' @param shapeVar_v variable to shape points by (usually "Patients")
  #' @param lineVar_v variable to change linetypes (usually "Patients")
  #' @param colorVar_v variable to color LINES by (usually "Patients")
  #' @param colorColors_v named color vector to use as color scale for color variable
  #' @param fillVar_v variable to color BOXES by (usually "Time point")
  #' @param fillColors_v named color vector to use as color scale for fill variable
  #' @return list of ggplots
  #' @export
  
  ###
  ### Line with points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Base plot
  currLinePoint_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), 
                                                 y = !!sym(yVar_v), 
                                                 group = !!sym(pairVar_v), 
                                                 color = !!sym(colorVar_v)))
  
  ### Add vertical lines to distinguish cycles
  if (D1Lines_v) {
    currLinePoint_gg <- currLinePoint_gg + 
      geom_vline(xintercept = "C1D-8", color = "lightgrey") +
      geom_vline(xintercept = "C2D1", color = "lightgrey") +
      geom_vline(xintercept = "C3D1", color = "lightgrey") +
      geom_vline(xintercept = "C4D1", color = "lightgrey")
  } # fi
    
  ### Add lines
  if (!is.null(lineVar_v)) {
    currLinePoint_gg <- currLinePoint_gg +
      geom_line(aes(linetype = !!sym(lineVar_v)))
  } else {
    currLinePoint_gg <- currLinePoint_gg +
      geom_line()
  } # fi
    
  ### Add points, and color
  currLinePoint_gg <- currLinePoint_gg +
    geom_point(size = 2) +
    scale_color_manual(values = colorColors_v, breaks = names(colorColors_v))
  
  ### Add theme stuff
  currLinePoint_gg <- currLinePoint_gg +
    my_theme() + angle_x() +
    ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent")
  
  ###
  ### Boxplot with Points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Base Plot
  currBoxShapedPoint_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), 
                                                      y = !!sym(yVar_v), 
                                                      fill = !!sym(fillVar_v))) + 
    geom_boxplot() +
    scale_fill_manual(values = timeColors_v, breaks = names(timeColors_v))
    
  ### Add points
  currBoxShapedPoint_gg <- currBoxShapedPoint_gg +
    geom_point(aes(shape = !!sym(pairVar_v)), size = 3)
  
  ### Add theme stuff
  currBoxShapedPoint_gg <- currBoxShapedPoint_gg +
    my_theme() + angle_x() +
    ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent")
  
  ###
  ### Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  out_ls <- list("line" = currLinePoint_gg, "box" = currBoxShapedPoint_gg)
  return(out_ls)
  
} # pairTimeSeries


### These are older versions that we didn't go with

# ### Line only
# currLine_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v),
#                                           group = !!sym(pairVar_v), color = !!sym(colorVar_v))) +
#   geom_vline(xintercept = "C1D-8", color = "lightgrey") +
#   geom_vline(xintercept = "C2D1", color = "lightgrey") +
#   geom_vline(xintercept = "C3D1", color = "lightgrey") +
#   geom_vline(xintercept = "C4D1", color = "lightgrey") +
#   geom_line(aes(linetype = !!sym(colorVar_v))) + my_theme() + angle_x() +
#   scale_color_manual(values = ptColors_v, breaks = names(ptColors_v), guide = T) +
#   ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent")
# 
# ### Box only
# currBox_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), fill = !!sym(fillVar_v))) +
#   geom_boxplot() +
#   scale_fill_manual(values = colors_v, breaks = names(colors_v)) +
#   my_theme() + angle_x() +
#   ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent") +
#   labs(subtitle = "box (don't like b/c too few points)")
# 
# ### Box with colored points
# currBoxColoredPoint_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), 
#                                                      y = !!sym(yVar_v), fill = !!sym(fillVar_v))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = colors_v, breaks = names(colors_v)) +
#   geom_point(aes(color = !!sym(pairVar_v))) + scale_color_manual(values = ptColors_v, breaks = names(ptColors_v)) +
#   my_theme() + angle_x() +
#   ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent") +
#   labs(subtitle = "box + colored points (don't like)")
# 
# ### Box with points and lines
# currBoxPointLine_gg <- ggplot(data = data_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), fill = !!sym(fillVar_v))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = colors_v, breaks = names(colors_v)) +
#   my_theme() + angle_x() +
#   geom_point() + geom_line(aes(group = !!sym(pairVar_v))) +
#   ggtitle(paste0("Pct ", yVar_v)) + ylab("Percent") + labs(subtitle = "box + points + lines (busiest, don't like)")
# 
# currOut_gg <- ggarrange(currLine_gg, currLinePoint_gg, currBox_gg, currBoxColoredPoint_gg,
#                         currBoxShapedPoint_gg, currBoxPointLine_gg, ncol = 2, nrow = 3)
# 
# pdf(file = file.path(outDir_v, paste0("tests_", yVar_v, ".pdf")), width = 18, height = 18, onefile = T)
# print(currOut_gg)
# dev.off()

# ### "Facet"
# if (!is.null(cycles_lsv)) {
#   tmp_lsgg <- list()
#   lims_v <- c(min(data_dt[[yVar_v]], na.rm = T)*.95, max(data_dt[[yVar_v]], na.rm = T)*1.05)
#   for (i in 1:length(cycles_lsv)) {
#     ### Get info
#     currCycle_v <- names(cycles_lsv)[i]
#     currTimepoints_v <- cycles_lsv[[currCycle_v]]
#     
#     ### Subset
#     currData_dt <- data_dt[`Time point` %in% currTimepoints_v,]
#     currData_dt$`Time point` <- droplevels(currData_dt$`Time point`)
#     
#     ### Plot
#     curr_gg <- ggplot(data = currData_dt, aes(x = !!sym(xVar_v), y = !!sym(yVar_v), 
#                                               group = !!sym(pairVar_v), color = !!sym(colorVar_v))) + 
#       scale_y_continuous(limits = lims_v) +
#       geom_line() + my_theme() + angle_x() +
#       theme(panel.grid.major.y = element_line(color = "lightgrey")) +
#       scale_color_manual(values = ptColors_v, breaks = names(ptColors_v)) +
#       ggtitle(currCycle_v) + ylab("Percent")
#     
#     if (i != 1) {
#       curr_gg <- curr_gg + theme(axis.title.y = element_blank(),
#                                  axis.text.y = element_blank())
#     }
#     tmp_lsgg[[currCycle_v]] <- curr_gg
#   } # for i
#   facetLine_gg <- ggarrange(plotlist = tmp_lsgg, nrow = 1, widths = sapply(tmp_lsgg, length))
#   facetLine_gg <- annotate_figure(p = facetLine_gg, top = text_grob(label = paste0("Pct ", yVar_v), size = 26))
#   out_ls[["facetLine"]] <- facetLine_gg
#   # pdf(file = file.path(outDir_v, paste0("facetLine_", xVar_v, ".pdf")), width = 12)
#   # print(facetLine_gg)
#   # dev.off()
# } # fi "facet"