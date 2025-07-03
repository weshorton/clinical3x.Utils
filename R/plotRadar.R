plotRadar <- function(data_dt, markerCol_v = "marker", markers_v, 
                      patientCol_v = "Patients", patientColors_v,
                      timeCol_v = "Time point", time_v = NULL, global_v = T,
                      file_v = NULL, title_v = NULL) {
  #' Plot Radar
  #' @description Wrapper for FSMB Radar plot
  #' @param data_dt data.table with functional markers
  #' @param markerCol_v column in data_dt that identifies markers that can be plotted (should be 'marker')
  #' @param markers_v character vector indicating which markers in markerCol_v to include in the plot
  #' @param patientCol_v which column identifies different patient observations (should be 'Patients')
  #' @param patientColors_v color vector for the patients
  #' @param timeCol_v which column in data_dt identifies different time points. One radar plot per time
  #' @param time_v optional vector to only splot specific time points instead of all available
  #' @param global_v logical indicating if extend of radar should be the global min/max or local (see details)
  #' @param file_v optional file to save output. If NULL, will print to console
  #' @details
    #' If global_v == T, then each marker's scale will go from 0 to globalMax (i.e. same scale)
    #' If global_v == F, then each marker's scale will go from 0 to markerMax (i.e. different scales)
  #' @return returns NULL (print to console). Save pdf if file_v is provided.
  #' @export
  
  ###
  ### Wrangle Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Get timepoints
  if (is.null(time_v)) {
    time_v <- sortTimePoints(unique(data_dt[[timeCol_v]]))
  } # fi
  
  ### Prepare each data.frame
  ### Remove any that don't fit
  data_lsdf <- list()
  for (i in 1:length(time_v)) {
    
    ### Get time and subset
    currTime_v <- time_v[i]
    curr_dt <- data_dt[get(timeCol_v) == currTime_v,]
    
    ### Get columns and subset
    plotCols_v <- c(patientCol_v, markers_v)
    curr_dt <- curr_dt[,mget(plotCols_v)]
    
    ###
    ### Add minimum and maximum values ~~~~~~~~~~
    ### (required for FSMB package)
    ###
    
    ### Convert to df for ease
    curr_df <- convertDFT(curr_dt, col_v = patientCol_v)
    
    ### Get specified min and max values
    if (global_v) {
      max_v <- rep(max(curr_df), ncol(curr_df))
      min_v <- rep(min(curr_df), ncol(curr_df))
    } else {
      max_v <- apply(curr_df, 2, max)
      min_v <- apply(curr_df, 2, min)
    } # fi global
    
    ### Add to data and fix names
    curr_df <- rbind(max_v, min_v, curr_df)
    rownames(curr_df)[1:2] <- c("Max", "Min")
    
    ### Remove empty marker columns (messes up the output)
    ### If both the minimum and the maximum are 0, then there are no values...
    rmCols_v <- names(which(sapply(colnames(curr_df), function(x) {
      max_v <- curr_df[rownames(curr_df) == "Max", x]
      min_v <- curr_df[rownames(curr_df) == "Min", x]
      (max_v == 0 & min_v == 0) 
    })))
    for (col_v in rmCols_v) curr_df[[col_v]] <- NULL
    
    ### Must have at least 3 variables in order to plot
    if (ncol(curr_df) < 3) next
    
    ### Add to list
    data_lsdf[[currTime_v]] <- curr_df
    
  } # for i
  
  ### Get arrangement dimensions for number of plots
  len_v <- length(data_lsdf)
  nCol_v <- ifelse(len_v <= 6, 2, ifelse(len_v <= 15, 3, 4))
  nRow_v <- ceiling(len_v / nCol_v)
  
  ### Make file
  if (!is.null(file_v)) pdf(file = file_v, width = 9*nCol_v, height = 10*nRow_v)
  
  ### Make pars
  #opar <- par()
  par(mar = rep(0.7,4)) # started with 0.8
  par(mfrow = c(nRow_v, nCol_v))
  
  ### Print plots
  for (i in 1:length(data_lsdf)) {
    
    currTime_v <- names(data_lsdf)[i]
    curr_df <- data_lsdf[[currTime_v]]
    
    ### Make plot
    radarchart(df = curr_df,
               
               ### Radar Line/Area Options
               pty = 16,                                  # point symbol (16 is closed circle)
               pcol = patientColors_v,                    # color vector
               pfcol = alpha(patientColors_v, 0.2),       # fill vector
               plwd = 3,                                  # radar area line width
               plty = 1:6,                                # radar line type (recycled if more than 6)
               pdensity = NULL,                           # This indicates the 'hatching' density
                                                              # NULL fills the whole thing
                                                              # integer makes as many hatches as the number (50 -> 50 lines)
               pangle = 45,                               # Angle of the hatches
               
               ### Radar Grid Options
               cglty = 3,                                 # line type for the grid
               cglwd = 2,                                 # line width for the grid
               cglcol = 'darkgrey',                       # color of gridlines (default is 'navy')
               
               ### Axis Options
               axistype = 5,                              # specifies which labels to display
               seg = 3,                                   # number of segments on the axis (4 is default)
               axislabcol = 'blue',                       # color of axis labels
               caxislabels = NULL,                        # center axis labels (don't use)
               calcex = 1.5,                                # magnification of center axis labels
               paxislabels = NULL,                        # peripheral axis labels (don't use)
               palcex = 3,                                # magnification of peripheral axis labels
               
               ### Label Options
               vlabels = NULL,                            # Vector for variable names. We just use the column names so this is NULL
               vlcex = 3,                                 # text label magnification
               title = currTime_v,
               
               ### Misc
               maxmin = T,
               na.itp = F,
               centerzero = F)
    
    ### Add legend
    legend(x = "bottom", legend = names(patientColors_v), horiz = T, pch = 20, inset = c(0, -0.025),
           bty = "n", col = patientColors_v, text.col = "black", cex = 1, pt.cex = 1.5)
    
    #if (!is.null(title_v)) title(main = title_v)
    
  } # for i
  
  ### Close device
  if (!is.null(file_v)) invisible(dev.off())
  
  #par <- par(opar)
  
} # plotRadar