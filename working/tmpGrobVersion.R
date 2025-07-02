library(grid)
library(data.table)

### Function to create diagonal table as grob object
createDiagonalTableGrob <- function(data_dt, colorMap_dt) {
  
  ### Convert data to matrix for easier handling
  dataMat_m <- as.matrix(data_dt)
  
  ### Get dimensions
  nRows_v <- nrow(dataMat_m)
  nCols_v <- ncol(dataMat_m)
  
  ### Calculate cell dimensions
  cellWidth_v <- 1 / nCols_v
  cellHeight_v <- 1 / nRows_v
  
  ### Create list to store all grobs
  grobList_lsg <- list()
  grobIndex_v <- 1
  
  ### Create each cell as a grob
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
          
          ### Make regular cell grob
          cellGrob_g <- gTree(children = gList(
            rectGrob(x = x_v, y = y_v, width = cellWidth_v, height = cellHeight_v,
                     just = c("left", "bottom", gp = gpar(fill = colors_v, col = "black")))
          ))
    
          
        } else if (length(colors_v) == 2) {
          
          ### Draw split cell
          cellGrob_g <- createDiagonalCellGrob(x_v = x_v, y_v = y_v, width_v = cellWidth_v, height_v = cellHeight_v,
                                               topColor_v = colors_v[1f])
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
      
      
      
      
      
      if (!is.null(splitCells_v) && cellName_v %in% splitCells_v) {
        ### Create diagonal split cell grob
        cellGrob_g <- createDiagonalCellGrob(x_v, y_v, cellWidth_v, cellHeight_v, 
                                             topColor_v, bottomColor_v)
      } else {
        ### Create regular cell grob
        cellGrob_g <- gTree(children = gList(
          rectGrob(x = x_v, y = y_v, width = cellWidth_v, height = cellHeight_v,
                   just = c("left", "bottom"), gp = gpar(fill = "white", col = "black")),
          textGrob(dataMat_m[i, j], 
                   x = x_v + cellWidth_v/2, 
                   y = y_v + cellHeight_v/2,
                   just = "centre")
        ))
      } ### end if split cell
      
      grobList_lsg[[grobIndex_v]] <- cellGrob_g
      grobIndex_v <- grobIndex_v + 1
    } ### end for j
  } ### end for i
  
  ### Combine all grobs into single grob tree
  tableGrob_g <- gTree(children = do.call(gList, grobList_lsg))
  
  return(tableGrob_g)
} ### end createDiagonalTableGrob

### Helper function to create diagonal split cell grob
createDiagonalCellGrob <- function(x_v, y_v, width_v, height_v, 
                                   topColor_v, bottomColor_v) {
  ### Create grob tree with all diagonal cell elements
  diagonalCellGrob_g <- gTree(children = gList(
    ### Bottom triangle (below diagonal)
    polygonGrob(x = c(x_v, x_v + width_v, x_v),
                y = c(y_v, y_v, y_v + height_v),
                gp = gpar(fill = bottomColor_v, col = "black")),
    
    ### Top triangle (above diagonal)
    polygonGrob(x = c(x_v, x_v + width_v, x_v + width_v),
                y = c(y_v + height_v, y_v, y_v + height_v),
                gp = gpar(fill = topColor_v, col = "black")),
    
    ### Diagonal line
    linesGrob(x = c(x_v, x_v + width_v),
              y = c(y_v + height_v, y_v),
              gp = gpar(col = "black", lwd = 2)),
    
    ### Cell border
    rectGrob(x = x_v, y = y_v, width = width_v, height = height_v,
             just = c("left", "bottom"), gp = gpar(fill = NA, col = "black"))
  ))
  
  return(diagonalCellGrob_g)
} ### end createDiagonalCellGrob