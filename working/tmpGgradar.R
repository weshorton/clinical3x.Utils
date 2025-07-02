# base.size = 15
# font.radar = "sans"
# values.radar = c("0%", "50%", "100%")
# axis.labels = colnames(plot.data)[-1]
# grid.min = 0
# grid.mid = 0.5
# grid.max = 1
# centre.y = grid.min - ((1/9) * (grid.max - grid.min))
# plot.extent.x.sf = 1
# plot.extent.y.sf = 1.2
# x.centre.range = 0.02 * (grid.max - centre.y)
# label.centre.y = FALSE
# grid.line.width = 0.5
# gridline.min.linetype = "longdash"
# gridline.mid.linetype = "longdash"
# gridline.max.linetype = "longdash"
# gridline.min.colour = "grey"
# gridline.mid.colour = "#007A87"
# gridline.max.colour = "grey"
# grid.label.size = 6
# gridline.label.offset = -0.1 * (grid.max - centre.y)
# label.gridline.min = TRUE
# label.gridline.mid = TRUE
# label.gridline.max = TRUE
# axis.label.offset = 1.15
# axis.label.size = 5
# axis.line.colour = "grey"
# group.line.width = 1.5
# group.point.size = 6
# group.colours = NULL
# background.circle.colour = "#D7D6D1"
# background.circle.transparency = 0.2
# plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE
# legend.title = ""
# plot.title = ""
# legend.text.size = 14
# legend.position = "left"
# fill = FALSE 
# fill.alpha = 0.5
# draw.points = TRUE
# point.alpha = 1
# line.alpha = 1
# 
# plot.data

ggradar <- function (plot.data, base.size = 15, font.radar = "sans", 
                     values.radar = c("0%", "50%", "100%"), axis.labels = colnames(plot.data)[-1], grid.min = 0, 
                     grid.mid = 0.5, grid.max = 1, centre.y = grid.min - ((1/9) * (grid.max - grid.min)), 
                     plot.extent.x.sf = 1, plot.extent.y.sf = 1.2, 
                     x.centre.range = 0.02 * (grid.max - centre.y), label.centre.y = FALSE, 
                     grid.line.width = 0.5, gridline.min.linetype = "longdash", 
                     gridline.mid.linetype = "longdash", gridline.max.linetype = "longdash", 
                     gridline.min.colour = "grey", gridline.mid.colour = "#007A87", 
                     gridline.max.colour = "grey", grid.label.size = 6, gridline.label.offset = -0.1 * 
                       (grid.max - centre.y), label.gridline.min = TRUE, label.gridline.mid = TRUE, 
                     label.gridline.max = TRUE, axis.label.offset = 1.15, axis.label.size = 5, 
                     axis.line.colour = "grey", group.line.width = 1.5, group.point.size = 6, 
                     group.colours = NULL, background.circle.colour = "#D7D6D1", 
                     background.circle.transparency = 0.2, plot.legend = if (nrow(plot.data) > 
                                                                             1) TRUE else FALSE, legend.title = "", plot.title = "", 
                     legend.text.size = 14, legend.position = "left", fill = FALSE, 
                     fill.alpha = 0.5, draw.points = TRUE, point.alpha = 1, line.alpha = 1) 
{
  plot.data <- as.data.frame(plot.data)
  plot.data <- aggregate(x = plot.data[, -1], by = list(plot.data[, 
                                                                  1]), FUN = "mean")
  if (!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 
                                                       1]))
  }
  var.names <- colnames(plot.data)[-1]
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", 
         call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }
  if (max(plot.data[, -1]) > grid.max) {
    plot.data[, -1] <- (plot.data[, -1]/max(plot.data[, -1])) * 
      grid.max
    warning("'plot.data' contains value(s) > grid.max, data scaled to grid.max", 
            call. = FALSE)
  }
  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + 
    abs(centre.y)
  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), 
                                 grid.max + abs(centre.y))
  axis$label <- data.frame(text = axis.labels, x = NA, y = NA)
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0, 0), grid.min + 
                                          abs(centre.y), npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0, 0), grid.mid + 
                                          abs(centre.y), npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0, 0), grid.max + 
                                          abs(centre.y), npoints = 360)
  gridline$min$label <- data.frame(x = gridline.label.offset, 
                                   y = grid.min + abs(centre.y), text = as.character(grid.min))
  gridline$max$label <- data.frame(x = gridline.label.offset, 
                                   y = grid.max + abs(centre.y), text = as.character(grid.max))
  gridline$mid$label <- data.frame(x = gridline.label.offset, 
                                   y = grid.mid + abs(centre.y), text = as.character(grid.mid))
  theme_clear <- theme_bw(base_size = base.size) + theme(axis.text.y = element_blank(), 
                                                         axis.text.x = element_blank(), axis.ticks = element_blank(), 
                                                         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                         panel.border = element_blank(), legend.key = element_rect(linetype = "blank"))
  if (plot.legend == FALSE) 
    legend.position <- "none"
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() + 
    geom_text(data = subset(axis$label, axis$label$x < (-x.centre.range)), 
              aes(x = x, y = y, label = text), size = axis.label.size, 
              hjust = 1, family = font.radar) + scale_x_continuous(limits = c(-1.5 * 
                                                                                plot.extent.x, 1.5 * plot.extent.x)) + scale_y_continuous(limits = c(-plot.extent.y, 
                                                                                                                                                     plot.extent.y))
  base <- base + geom_path(data = gridline$min$path, aes(x = x, 
                                                         y = y), lty = gridline.min.linetype, colour = gridline.min.colour, 
                           linewidth = grid.line.width)
  base <- base + geom_path(data = gridline$mid$path, aes(x = x, 
                                                         y = y), lty = gridline.mid.linetype, colour = gridline.mid.colour, 
                           linewidth = grid.line.width)
  base <- base + geom_path(data = gridline$max$path, aes(x = x, 
                                                         y = y), lty = gridline.max.linetype, colour = gridline.max.colour, 
                           linewidth = grid.line.width)
  base <- base + geom_text(data = subset(axis$label, abs(axis$label$x) <= 
                                           x.centre.range), aes(x = x, y = y, label = text), size = axis.label.size, 
                           hjust = 0.5, family = font.radar)
  base <- base + geom_text(data = subset(axis$label, axis$label$x > 
                                           x.centre.range), aes(x = x, y = y, label = text), size = axis.label.size, 
                           hjust = 0, family = font.radar)
  base <- base + theme_clear
  base <- base + geom_polygon(data = gridline$max$path, aes(x, 
                                                            y), fill = background.circle.colour, alpha = background.circle.transparency)
  base <- base + geom_path(data = axis$path, aes(x = x, y = y, 
                                                 group = axis.no), colour = axis.line.colour)
  theGroupName <- names(group$path[1])
  if (length(line.alpha) == 1) {
    base <- base + geom_path(data = group$path, aes(x = .data[["x"]], 
                                                    y = .data[["y"]], group = .data[[theGroupName]], 
                                                    colour = .data[[theGroupName]]), linewidth = group.line.width, 
                             alpha = line.alpha)
  }
  else {
    base <- base + geom_path(data = group$path, aes(x = .data[["x"]], 
                                                    y = .data[["y"]], group = .data[[theGroupName]], 
                                                    colour = .data[[theGroupName]]), linewidth = group.line.width) + 
      scale_alpha_manual(values = line.alpha)
  }
  if (draw.points) {
    if (length(point.alpha) == 1) {
      base <- base + geom_point(data = group$path, aes(x = .data[["x"]], 
                                                       y = .data[["y"]], group = .data[[theGroupName]], 
                                                       colour = .data[[theGroupName]]), size = group.point.size, 
                                alpha = point.alpha)
    }
    else {
      base <- base + geom_point(data = group$path, aes(x = .data[["x"]], 
                                                       y = .data[["y"]], group = .data[[theGroupName]], 
                                                       colour = .data[[theGroupName]]), size = group.point.size) + 
        scale_alpha_manual(values = point.alpha)
    }
  }
  if (fill == TRUE) {
    base <- base + geom_polygon(data = group$path, aes(x = .data[["x"]], 
                                                       y = .data[["y"]], group = .data[[theGroupName]], 
                                                       fill = .data[[theGroupName]]), alpha = fill.alpha)
  }
  if (plot.legend == TRUE) 
    base <- base + labs(colour = legend.title, size = legend.text.size)
  if (label.gridline.min == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[1]), 
                             data = gridline$min$label, size = grid.label.size * 
                               0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.mid == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[2]), 
                             data = gridline$mid$label, size = grid.label.size * 
                               0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.max == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[3]), 
                             data = gridline$max$label, size = grid.label.size * 
                               0.8, hjust = 1, family = font.radar)
  }
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text), 
                             data = centre.y.label, size = grid.label.size, hjust = 0.5, 
                             family = font.radar)
  }
  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, length(unique(plot.data[, 
                                                                1]))/length(group.colours))
  }
  else {
    colour_values <- generate_color_values(length(unique(plot.data[, 
                                                                   1])))
  }
  base <- base + theme(legend.key.width = unit(3, "line"), 
                       text = element_text(size = 20, family = font.radar)) + 
    theme(legend.text = element_text(size = legend.text.size), 
          legend.position = legend.position) + theme(legend.key.height = unit(2, 
                                                                              "line")) + scale_colour_manual(values = colour_values) + 
    theme(text = element_text(family = font.radar)) + theme(legend.title = element_blank())
  if (isTRUE(fill)) {
    base <- base + scale_fill_manual(values = colour_values, 
                                     guide = "none")
  }
  if (legend.title != "") {
    base <- base + theme(legend.title = element_text())
  }
  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  return(base)
}