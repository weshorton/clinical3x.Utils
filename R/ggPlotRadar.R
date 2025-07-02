ggPlotRadar <- function(data_dt, markerCol_v = "marker", markers_v, 
                        patientCol_v = "Patients", patientColors_v,
                        timeCol_v = "Time point", time_v = NULL, 
                        name_v = NULL,
                        file_v = NULL) {
  #' Plot Radar
  #' @description Wrapper for FSMB Radar plot
  #' @param data_dt data.table with functional markers
  #' @param markerCol_v column in data_dt that identifies markers that can be plotted (should be 'marker')
  #' @param markers_v character vector indicating which markers in markerCol_v to include in the plot
  #' @param patientCol_v which column identifies different patient observations (should be 'Patients')
  #' @param patientColors_v color vector for the patients
  #' @param timeCol_v which column in data_dt identifies different time points. One radar plot per time
  #' @param time_v optional vector to only plot specific time points instead of all available
  #' @param name_v optional vector to label the entire plot (e.g. CD4+ Tcm)
  #' @param file_v optional file to save output. If NULL, will print to console
  #' @return returns NULL (print to console). Save pdf if file_v is provided.
  #' @export
  
  ###
  ### Wrangle Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Get timepoints
  if (is.null(time_v)) {
    time_v <- sortTimePoints_v(unique(data_dt[[timeCol_v]]))
  } # fi
  
  ### Max number of entries
  maxPt_v <- length(unique(data_dt[[patientCol_v]]))
  
  ### Prepare each data.frame
  ### Remove any that don't fit
  data_lsdt <- list()
  for (i in 1:length(time_v)) {
    
    ### Get time and subset
    currTime_v <- time_v[i]
    curr_dt <- data_dt[get(timeCol_v) == currTime_v,]
    
    ### Get columns and subset
    plotCols_v <- c(patientCol_v, markers_v)
    curr_dt <- curr_dt[,mget(plotCols_v)]
    
    ### Remove empty marker columns (messes up the output)
    colSums_dt <- curr_dt[,lapply(.SD, sum), .SDcols = markers_v]
    rmCols_v <- colnames(colSums_dt)[which(colSums_dt == 0)]
    for (col_v in rmCols_v) curr_dt[[col_v]] <- NULL
    
    ### Must have at least 3 variables in order to plot
    if (ncol(curr_dt) < 4) next
    
    ### Add to list
    data_lsdt[[currTime_v]] <- curr_dt
    
  } # for i
  
  ###
  ### Make Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  plot_lsgg <- list()
  gotLegend_v <- F
  for (i in 1:length(data_lsdt)) {
    
    ### Get info
    currTime_v <- names(data_lsdt)[i]
    curr_dt <- data_lsdt[[currTime_v]]
    
    ### Get argument values (required for testing)
    gMin_v <- 0; gMax_v <- 1; gMid_v <- 0.5
    centerY_v <- gMin_v - ((1/3) * (gMax_v - gMin_v))
    
    ### Other arg values
    plotCols_v <- setdiff(colnames(curr_dt), patientCol_v)
    max_v <- max(curr_dt[,mget(plotCols_v)])
    min_v <- min(curr_dt[,mget(plotCols_v)])
    mid_v <- round((max_v - min_v) / 2, digits = 2)
    radarVals_v <- c(paste0(min_v, " (0%)"), paste0(mid_v, " (50%)"), paste0(max_v, " (100%)"))
    
    ### Get colors (turns grey if the color vector has extra labels not in data)
    plotColors_v <- patientColors_v[names(patientColors_v) %in% curr_dt[[patientCol_v]]]

    ### Make plot
    curr_gg <- ggradar(plot.data = curr_dt,
                       base.size = 15,
                       font.radar = "sans",
                       axis.labels = plotCols_v,
                       
                       ### Control radar grid appearance
                       values.radar = radarVals_v,                       # values to print at min, mean, and max grid lines
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
                       grid.label.size = 5,                                         # text size of gridline labels
                       gridline.label.offset = -0.1 * (gMax_v - centerY_v),         # left (-)/right (+) offset of labels relative to central vert axis
                       axis.label.offset = 1.15,                                    # vertical displacement of axis labels from maximum grid line, measured relative to circle diameter
                       axis.label.size = 8,                                         # text size of axis labels
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
                       legend.text.size = 14,                                       # legend text size
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
    
    if (!gotLegend_v & nrow(curr_dt) == maxPt_v) {
      legendGrob <- ggpubr::as_ggplot(g_legend(curr_gg))
      gotLegend_v <- T
    } # fi
    
    ### Add to list
    plot_lsgg[[currTime_v]] <- curr_gg

  } # for i
  
  ###
  ### Assemble ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Add legend
  plot_lsgg[["legend"]] <- legendGrob
  
  ### Combine
  plot_gg <- ggpubr::ggarrange(plotlist = plot_lsgg, legend = "none")
  
  ### Add title
  if (!is.null(name_v)) {
    plot_gg <- ggpubr::annotate_figure(plot_gg, top = ggpubr::text_grob(label = paste0(name_v, " Functionality"), size = 24))
  }
  
  ### Get dims
  dims_v <- getDims(length(plot_lsgg))
  
  ### Make file
  if (!is.null(file_v)) pdf(file = file_v, width = 9*dims_v[1], height = 10*dims_v[2])
  
  print(plot_gg)
  
  if (!is.null(file_v)) dev.off()
  
} # plotRadar