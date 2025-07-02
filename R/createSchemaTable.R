### Function to create diagonal split cells
createSchemaTable <- function(data_dt, colorMap_dt, direction_v = "backward") {
  #' Create Diagonal Table
  #' @description
  #' Make a gtable with specified cells specially-colored with diagonals
  #' @param data_dt input table to be displayed
  #' @param colorMap_dt reference table that has a color for each cell designation. If data_dt has value not in colorMap, it will not get filled
  #' @param direction_v passed to drawDiagonalCell to determine triangle direction (forward or backward for / or \ respectively)
  #' @details tbd
  #' @return tbd
  #' @export
  
  ### Convert data to matrix for easier handling
  dataMat_m <- as.matrix(data_dt)
  
  ### Get dimensions
  nRows_v <- nrow(dataMat_m)
  nCols_v <- ncol(dataMat_m)
  
  ### Calculate cell dimensions from table dimensions
  cellWidth_v <- 1 / nCols_v
  cellHeight_v <- 1 / nRows_v
  
  ### Create new graphics device
  grid.newpage()
  
  ### Set up viewport for the table
  pushViewport(viewport(width = 0.8, height = 0.4))
  
  
  ### Draw each cell
  for (i in 1:nRows_v) {
    for (j in 1:nCols_v) {
      
      ### Calculate cell position
      x_v <- (j - 1) * cellWidth_v
      y_v <- 1 - i * cellHeight_v  ### Flip y-axis for proper table orientation
      
      ### Get value of this cell
      cellValue_v <- dataMat_m[i, j]
      
      ### Get its color from the map table
      currColorMap_v <- colorMap_dt[Tx == cellValue_v,][["Color"]]
      
      if (length(currColorMap_v)) {
        
        ### Get color(s)
        colors_v <- strsplit(currColorMap_v, "\\+")[[1]]
        
        if (length(colors_v) == 1) {
          
          ### Draw the cell
          grid.rect(x = x_v, y = y_v, width = cellWidth_v, height = cellHeight_v,
                    just = c("left", "bottom"), gp = gpar(fill = colors_v, col = "black"))
          
        } else if (length(colors_v) == 2) {
          
          ### Draw split cell
          drawDiagonalCell(x_v = x_v, y_v = y_v, width_v = cellWidth_v, height_v = cellHeight_v, 
                           topColor_v = colors_v[1], bottomColor_v = colors_v[2], direction_v = direction_v)
          
        } # fi
        
      } else {
        
        ### Draw regular cell
        grid.rect(x = x_v, y = y_v, width = cellWidth_v, height = cellHeight_v,
                  just = c("left", "bottom"), gp = gpar(fill = "white", col = "black"))
        
        ### Add text
        grid.text(dataMat_m[i, j], 
                  x = x_v + cellWidth_v/2, 
                  y = y_v + cellHeight_v/2,
                  just = "centre")
        
      } # fi
      
    } ### end for j
  } ### end for i
  
  popViewport()
} ### end createDiagonalTable

### Helper function to draw diagonal split cell
drawDiagonalCell <- function(x_v, y_v, width_v, height_v, 
                             topColor_v, bottomColor_v, direction_v = "backward") {
  #' Draw Diagonal Cell
  #' @description
  #' Make a diagonal cell inside of a gtable
  #' @param x_v row value of cell that needs a diagonal
  #' @param y_v column value of cell that needs a diagonal
  #' @param width_v width of cell
  #' @param height_v height of cell
  #' @param topColor_v color vector for top portion
  #' @param bottomColor_v color vector for bottom portion
  #' @param direction_v Direction of the triangle. Either 'forward' for a forward slash (/) or 'backward" for backslash
  #' @details tbd
  #' @return tbd
  #' @export
  
  if (direction_v == "forward") {
    
    x1_v <- x_v + width_v
    x2_v <- x_v
    y1_v <- y_v + height_v
    y2_v <- y_v
    
  } else if (direction_v == "backward") {
    
    x1_v <- x_v
    x2_v <- x_v + width_v
    y1_v <- y_v
    y2_v <- y_v + height_v
    
  } else {
    
    stop("direction_v can only be 'forward' or 'backward'")
    
  }
  ### Draw bottom triangle (below diagonal)
  grid.polygon(x = c(x1_v, x2_v, x1_v),
               y = c(y1_v, y1_v, y2_v),
               gp = gpar(fill = bottomColor_v, col = "black"))
  
  ### Draw top triangle (above diagonal)
  grid.polygon(x = c(x1_v, x2_v, x2_v),
               y = c(y2_v, y1_v, y2_v),
               gp = gpar(fill = topColor_v, col = "black"))
  
  ### Draw diagonal line
  grid.lines(x = c(x1_v, x2_v),
             y = c(y2_v, y1_v),
             gp = gpar(col = "black", lwd = 2))
  
  ### Draw cell border
  grid.rect(x = x_v, y = y_v, width = width_v, height = height_v,
            just = c("left", "bottom"), gp = gpar(fill = NA, col = "black"))
} ### end drawDiagonalCell