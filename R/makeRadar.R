#' Build one radar chart
#'
#' @param curr_dt        data.table with the data for one time point
#' @param currTime_v     character, title of the plot
#' @param patientCol_v   column that identifies the patient / group
#' @param patientColors_v named vector with colours
#' @param legend_v logical indicating if legend should be printed.
#' @param maxPt_v tbd
#' @return               ggplot object
#' @export
makeRadar <- function(curr_dt,
                      currTime_v,
                      patientCol_v,
                      patientColors_v,
                      #legend_v, 
                      plotGlobalMax_v,
                      maxPt_v,
                      globalMin_v, globalMid_v, globalMax_v) {
  
  ### Axis label values ----------------------------------------------------
  plotCols_v <- setdiff(colnames(curr_dt), patientCol_v)
  max_v <- max(curr_dt[, mget(plotCols_v)])
  min_v <- min(curr_dt[, mget(plotCols_v)])
  mid_v <- round((max_v - min_v) / 2, 2)
  
  ### Colours --------------------------------------------------------------
  plotColors_v <- patientColors_v[names(patientColors_v) %in%
                                    curr_dt[[patientCol_v]]]
  
  if (plotGlobalMax_v) {
    radarVals_v <- c(paste0(round(globalMin_v, 2), " (0%)"),
                     paste0(globalMid_v,          " (50%)"),
                     paste0(round(globalMax_v, 2), " (100%)"))
    
    ### Get argument values (required for testing)
    gMin_v <- globalMin_v; gMax_v <- globalMax_v; gMid_v <- globalMid_v
    centerY_v <- gMin_v - ((1/3) * (gMax_v - gMin_v))
  } else {
    
    radarVals_v <- c(paste0(round(min_v, 2), " (0%)"),
                     paste0(mid_v,          " (50%)"),
                     paste0(round(max_v, 2), " (100%)"))
    
    ### Get argument values (required for testing)
    gMin_v <- 0; gMax_v <- 1; gMid_v <- 0.5
    centerY_v <- gMin_v - ((1/3) * (gMax_v - gMin_v))
    
  }
  
  ### Build plot -----------------------------------------------------------
  curr_gg <- ggradar(plot.data = curr_dt,
                     base.size = 18,
                     font.radar = "sans",
                     axis.labels = plotCols_v,
                     
                     ### Control radar grid appearance
                     values.radar = eval(radarVals_v),                       # values to print at min, mean, and max grid lines
                     grid.min = gMin_v,                                           # value to plot minimum grid line
                     grid.mid = gMid_v,                                           # value to plot 'average' grid line
                     grid.max = gMax_v,                                           # value to plot maximum grid line
                     label.gridline.min = TRUE,                                   # write label?
                     label.gridline.mid = TRUE,                                   # write label?
                     label.gridline.max = TRUE,                                   # write label?
                     
                     ### Control radar grid lines
                     grid.line.width = 0.5,                                       # width for all grid lines
                     gridline.min.linetype = "longdash",                          # linetype for min
                     gridline.mid.linetype = "longdash",                          # linetype for mid
                     gridline.max.linetype = "longdash",                          # linetype for max
                     gridline.min.colour = "grey",                                # line color for min
                     gridline.mid.colour = "#007A87",                             # line color for mid
                     gridline.max.colour = "grey",                                # line color for max
                     
                     ### Control labels
                     grid.label.size = 10,                                         # text size of gridline labels
                     gridline.label.offset = -0.1 * (gMax_v - centerY_v),         # left (-)/right (+) offset of labels relative to central vert axis
                     axis.label.offset = 1.15,                                    # vertical displacement of axis labels from maximum grid line, measured relative to circle diameter
                     axis.label.size = 12,                                         # text size of axis labels
                     axis.line.colour = "grey",                                   # axis line color
                     
                     ### Control groups
                     group.line.width = 1,                                        # line width of group lines
                     group.point.size = 2,                                        # point size for group points
                     group.colours = plotColors_v,                                # colors to use for group
                     fill = T,                                                    # fill polygons?
                     fill.alpha = 0.1,                                            # transparency of filled polygons
                     
                     ### Legend and title
                     plot.legend = if (nrow(curr_dt) > 1) TRUE else FALSE,        # include legend? Yes if >1
                     legend.title = patientCol_v,                                 # legend title
                     plot.title = currTime_v,                                     # plot title
                     legend.text.size = 20,                                       # legend text size
                     legend.position = "left",                                    # legend location (top, bottom, left, right)
                     
                     ### Other plotting params
                     plot.extent.x.sf = 1,                                        # relative size of plot horizontally
                     plot.extent.y.sf = 1.2,                                      # relative size of plot vertically
                     background.circle.colour = "#FFFFFF",                        # radar background (their default was grey)
                     background.circle.transparency = 0.2,                        # how transparent to make it (doesn't matter if white, I think)
                     centre.y = centerY_v,                                        # center value for y (Default is -0.1111)
                     x.centre.range = 0.02 * (gMax_v - centerY_v),                # x label alignment (Default is 0.02222)
                     label.centre.y = F                                           # plot center value of y?
  )
  
  #if (legend_v & nrow(curr_dt) == maxPt_v) {
  legendGrob <- ggpubr::as_ggplot(g_legend(curr_gg))
  #} # fi
  
  ### Adjust title size
  curr_gg <- curr_gg + theme(plot.title = element_text(size = 48, hjust = 0.5))
  
  return(list("plot" = curr_gg, "legend" = legendGrob))
  
} # makeRadar