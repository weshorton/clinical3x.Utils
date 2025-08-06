ggPlotRadar <- function(data_dt, markerCol_v = "marker", markers_v, 
                        patientCol_v = "Patients", patientColors_v,
                        timeCol_v = "Time point", time_v = NULL, 
                        name_v = NULL,
                        file_v = NULL) {
  #' Plot Radar
  #' @description Wrapper for FSMB Radar plot
  #' @param data_dt data.table with functional markers
  #' @param markerCol_v column in data_dt that identifies markers that can be plotted (should be 'marker') - not used?
  #' @param markers_v character vector indicating which markers in markerCol_v to include in the plot
  #' @param patientCol_v which column identifies different patient observations (should be 'Patients')
  #' @param patientColors_v color vector for the patients
  #' @param timeCol_v which column in data_dt identifies different time points. One radar plot per time
  #' @param time_v optional vector to only plot specific time points instead of all available
  #' @param name_v optional vector to label the entire plot (e.g. CD4+ Tcm)
  #' @param file_v optional file to save output. If NULL, will print to console
  #' @return returns NULL (print to console). Save pdf if file_v is provided.
  #' @details I think this is more difficult than it needs to be? If I give a melted table then I might have more flexibility on what
  #' gets put where. Also I should make the variables more generic. timeCol_v should be "perPlotCol" (or something indicating that
  #' this variable determines how many plots to make); patientCol_v should be "lineCol" or whatever to indicate that's the lines;
  #' finally markerCol_v isn't used, but would be if I did the melt thing and I want to change this to axisLabels or something to indicate
  #' these will be around the edge of the radar.
  #' @import ggradar
  #' @export
  
  ###
  ### Wrangle Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Get timepoints
  if (is.null(time_v)) {
    time_v <- sortTimePoints(unique(data_dt[[timeCol_v]]))
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
  
  if (!length(data_lsdt)) return(NULL)
  
  ###
  ### Make Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  plot_lsgg <- list()
  makeLegend_v <- T
  for (i in 1:length(data_lsdt)) {
    
    currTime_v <- names(data_lsdt)[i]
    curr_dt <- data_lsdt[[currTime_v]]
    
    plot_lsgg[[currTime_v]] <- makeRadar(curr_dt, currTime_v, patientCol_v, patientColors_v, legend_v = makeLegend_v, maxPt_v)
    makeLegend_v <- F
    
  } # for i
  
  ###
  ### Assemble ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Add legend
  plot_lsgg[["legend"]] <- legendGrob
  
  ### Get rows/columns
  # dims_v <- getDims(length(plot_lsgg)) # original version copies ggarrange behavior
  # width_v <- 9*dims_v[1]
  # height_v <- 10*dims_v[2]
  dims_v <- c(ceiling(length(plot_lsgg)/3), 3) # always 3 columns
  width_v <- 5*dims_v[1]
  height_v <- 15*dims_v[2]
  
  ### Combine
  plot_gg <- ggpubr::ggarrange(plotlist = plot_lsgg, legend = "none", nrow = dims_v[1], ncol = dims_v[2])
  
  ### Add title
  if (!is.null(name_v)) {
    plot_gg <- ggpubr::annotate_figure(plot_gg, top = ggpubr::text_grob(label = paste0(name_v, " Functionality"), size = 64))
  }
  
  ### Make file
  if (!is.null(file_v)) {
    pdf(file = file_v, width = width_v, height = height_v)
    print(plot_gg)
    dev.off()
  } else {
    return(plot_gg)
  }
  
} # ggPlotRadar

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
                      legend_v, maxPt_v) {
  
  ### Get argument values (required for testing)
  gMin_v <- 0; gMax_v <- 1; gMid_v <- 0.5
  centerY_v <- gMin_v - ((1/3) * (gMax_v - gMin_v))
  
  ### Axis label values ----------------------------------------------------
  plotCols_v <- setdiff(colnames(curr_dt), patientCol_v)
  max_v <- max(curr_dt[, mget(plotCols_v)])
  min_v <- min(curr_dt[, mget(plotCols_v)])
  mid_v <- round((max_v - min_v) / 2, 2)
  
  radarVals_v <- c(paste0(round(min_v, 2), " (0%)"),
                   paste0(mid_v,          " (50%)"),
                   paste0(round(max_v, 2), " (100%)"))
  
  ### Colours --------------------------------------------------------------
  plotColors_v <- patientColors_v[names(patientColors_v) %in%
                                    curr_dt[[patientCol_v]]]
  
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
  
  if (legend_v & nrow(curr_dt) == maxPt_v) {
    legendGrob <- ggpubr::as_ggplot(g_legend(curr_gg))
  } # fi
  
  ### Adjust title size
  curr_gg <- curr_gg + theme(plot.title = element_text(size = 48, hjust = 0.5))
} # makeRadar