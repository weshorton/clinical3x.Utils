ggPlotRadar <- function(data_dt, markerCol_v = "marker", markers_v, 
                        patientCol_v = "Patients", patientColors_v,
                        timeCol_v = "Time point", time_v = NULL, 
                        name_v = NULL, plotGlobalMax_v = T,
                        removeEmpties_v = F,
                        metaCols_v = NULL, file_v = NULL) {
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
  #' @param plotGlobalMax_v logical indicating if scale should be global (T) or individual (F)
  #' @param removeEmpties_v logical indicating if empty markers should be removed from the plot (T) or kept (F)
  #' @param metaCols_v optional metadata columns that need to be excluded from calculations
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
  
  ### New - get globals
  plotCols_v <- setdiff(colnames(data_dt), unique(c(patientCol_v, timeCol_v, metaCols_v)))
  globalMax_v <- max(data_dt[, mget(plotCols_v)])
  globalMin_v <- min(data_dt[, mget(plotCols_v)])
  globalMid_v <- round((globalMax_v - globalMin_v) / 2, 2)
  
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
    if (removeEmpties_v) {
      colSums_dt <- curr_dt[,lapply(.SD, sum), .SDcols = markers_v]
      rmCols_v <- colnames(colSums_dt)[which(colSums_dt == 0)]
      for (col_v in rmCols_v) curr_dt[[col_v]] <- NULL
    }
    
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
  gotLegend_v <- F
  for (i in 1:length(data_lsdt)) {
    
    currTime_v <- names(data_lsdt)[i]
    curr_dt <- data_lsdt[[currTime_v]]
    
    plot_lsgg[[currTime_v]] <- makeRadar(curr_dt = curr_dt, 
                                         currTime_v = currTime_v, 
                                         patientCol_v = patientCol_v, 
                                         patientColors_v = patientColors_v, 
                                         #legend_v = makeLegend_v, 
                                         plotGlobalMax_v = plotGlobalMax_v,
                                         maxPt_v = maxPt_v,
                                         globalMin_v = globalMin_v, 
                                         globalMid_v = globalMid_v, 
                                         globalMax_v = globalMax_v)
    
    
    if (!gotLegend_v & nrow(curr_dt) == maxPt_v) {
      leg <- plot_lsgg[[currTime_v]]$legend
      gotLegend_v <- T
    }
    
    plot_lsgg[[currTime_v]] <- plot_lsgg[[currTime_v]]$plot
    #makeLegend_v <- F
    
  } # for i
  
  ###
  ### Assemble ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Add legend
  plot_lsgg[["legend"]] <- leg
  
  ### Get rows/columns
  # dims_v <- getDims(length(plot_lsgg)) # original version copies ggarrange behavior
  # width_v <- 9*dims_v[1]
  # height_v <- 10*dims_v[2]
  dims_v <- c(ceiling(length(plot_lsgg)/3), 3) # always 3 columns
  width_v <- 5*dims_v[1]
  height_v <- 15*dims_v[2]
  
  ### Combine
  plot_gg <- suppressMessages(ggpubr::ggarrange(plotlist = plot_lsgg, legend = "none", nrow = dims_v[1], ncol = dims_v[2]))
  
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
