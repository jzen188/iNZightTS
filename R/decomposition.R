decomposition <-
function(obj, trendCol = "black",
                                    seasonCol = "#45a8ff",randCol = seasonCol) {

    xlist = get.x(obj$tsObj)
    x = xlist$x
    x.units = xlist$x.units


    if (obj$freq > 1) {
        decomp = stl(obj$tsObj, "periodic")
    }
    else {
        trend.comp = loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted + obj$tsObj * 0
        residuals.comp = obj$tsObj - trend.comp
        seasons.comp = obj$tsObj * 0
        decomp = list()
        decomp$time.series = as.ts(data.frame(seasonal = seasons.comp,
                                              trend = trend.comp,
                                              remainder = residuals.comp))
    }

    decompData = decomp$time.series    # returns matrix


    ### x and y coordinates
    y = obj$tsObj@.Data
    y.trend = decompData[,"trend"]
    y.season = decompData[,"seasonal"]
    y.random = decompData[,"remainder"]
    y.trend.units = unit(y.trend, "native")
    y.season.units = unit(y.season, "native")
    y.random.units = unit(y.random, "native")


    ### We want each component plotted on the same scale, so we need
    ### to find the y-axis interval for the most variable component
    minmax = apply(decompData, 2, function(x) range(x))
    ranges = minmax[2,] - minmax[1,]
    ranges[2] = max(y) - min(y)


    expandBy = (max(y) - min(y)) * 0.1
    # here we edit the viewport y values to provide an extra gap at
    # the top of each panel
    trend.vp.y = y
    trend.vp.y[which.max(y)] = max(y) + expandBy
    season.vp.y = y.season
    max.index = ifelse(obj$freq > 1, which.max(y.season), 1)
    season.vp.y[max.index] = max(y.season) + expandBy
    if (obj$freq == 1) season.vp.y[2] = -expandBy
    random.vp.y = y.random
    random.vp.y[which.max(y.random)] = max(y.random) + expandBy


    ranges = ranges + expandBy
    which.max.range = which.max(ranges)
    y.interval = diff(pretty(minmax[,which.max.range]))[1]


    ### Need to find the proportion of the plot each subplot will be
    ### allocated, and create the viewports
    props = ranges/sum(ranges)


    ### The following defines the viewport layout for the plot
    ### parent.vp holds everything - with a main central viewport
    ### and 4 viewports around it that act as margins
    vp.heights = c(.6, 1, .6)
    vertMargins = sum(vp.heights[-2])
    parent.vp = viewport(name = "parent",
                         layout = grid.layout(3, 3,
                                              heights = unit(vp.heights,
                                                 c("inches", "null", "inches")),
                                              widths = unit(c(.7, 1, .7),
                                                 c("inches", "null", "inches"))))
    head.vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2, name = "head")
    left.vp = viewport(layout.pos.row = 2, layout.pos.col = 1, name = "left")
    right.vp = viewport(layout.pos.row = 2, layout.pos.col = 3, name = "right")
    bottom.vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2, name = "bottom")
    plots.vp = viewport(layout = grid.layout(3, 1, heights = props[c(2, 1, 3)]),
                         name = "plots", layout.pos.row = 2, layout.pos.col = 2)
    trend.vp = dataViewport(x, trend.vp.y, name = "trend", layout.pos.row = 1)
    season.vp = dataViewport(x, season.vp.y, name = "season", layout.pos.row = 2)
    random.vp = dataViewport(x, random.vp.y, name = "random", layout.pos.row = 3)

    plots.vptree = vpTree(plots.vp, vpList(trend.vp, season.vp, random.vp))
    final.vptree = vpTree(parent.vp, vpList(head.vp, left.vp, plots.vptree,
                                             right.vp, bottom.vp))

    xlims = season.vp$xscale
    dotted.xcoords = c(xlims[1], x, xlims[2])
    dotted.ycoords = rep(0, length(dotted.xcoords))

    ### The following creates a gTree which contains all of our grobs
    grobs = gList()

    grobs$trendBorder = rectGrob(vp = vpPath("parent", "plots", "trend"),
                                 name = "trendBorder", gp = gpar(lwd = 2))
    grobs$raw.ghost = linesGrob(x.units, unit(y, "native"),
                                vp = vpPath("parent", "plots", "trend"),
                                name = "raw.ghost",
                                gp = gpar(col = "#bbbbbb"))
    grobs$trendLine = linesGrob(x.units, y.trend.units,
                                vp = vpPath("parent", "plots", "trend"),
                                name = "trendLine",
                                gp = gpar(col = trendCol, lwd = 2))
    grobs$trendYaxis = yaxisGrob(main = TRUE,
                                 vp = vpPath("parent", "plots", "trend"),
                                 name = "trendYaxis",
                                 gp = gpar(cex = .8))

    gap = unit(2, "mm")
    space = unit(8, "mm")
    lineWidth = unit(6, "mm")
    xc = unit(2, "mm")
    grobs$trendLabel = textGrob("Trend", just = c("left", "bottom"), x = xc,
                                y = unit(1, "npc") - unit(1, "lines"), name = "trendLabel",
                                vp = vpPath("parent", "plots", "trend"),
                                gp = gpar(cex = .9, col = "black",
                                          fontface = "bold"))
    xc = xc + stringWidth(grobs$trendLabel$label) + gap
    grobs$trendKey = linesGrob(x = unit.c(xc, xc + lineWidth),
                               y = unit(1, "npc") - unit(0.6, "lines"),
                               vp = vpPath("parent", "plots", "trend"),
                               name = "trendKey",
                               gp = gpar(col = trendCol, lwd = 2))
    xc = xc + lineWidth + space
    grobs$rawKeyText = textGrob("Raw data", just = c("left", "bottom"),
                               x = xc,
                               y = unit(1, "npc") - unit(1, "lines"), name = "rawKeyText",
                               vp = vpPath("parent", "plots", "trend"),
                               gp = gpar(cex = .9, col = "#bbbbbb",
                                         fontface = "bold"))
    xc = xc + stringWidth(grobs$rawKeyText$label) + gap
    grobs$rawKey = linesGrob(unit.c(xc, xc + lineWidth),
                             unit(1, "npc") - unit(0.6, "lines"),
                             vp = vpPath("parent", "plots", "trend"),
                             name = "rawKey",
                             gp = gpar(col = "#bbbbbb"))

    grobs$seasonBorder = rectGrob(vp = vpPath("parent", "plots", "season"),
                                  name = "seasonBorder", gp = gpar(lwd = 2))
    grobs$season.y0 = linesGrob(unit(dotted.xcoords, "native"),
                                unit(dotted.ycoords, "native"),
                                vp = vpPath("parent", "plots", "season"),
                                name = "season.y0",
                                gp = gpar(col = "#aaaaaa", lty = "1313"))
    grobs$seasonLine = linesGrob(x.units, y.season.units,
                                 vp = vpPath("parent", "plots", "season"),
                                 name = "seasonLine",
                                 gp = gpar(col = seasonCol))
    grobs$seasonYaxis = yaxisGrob(main = FALSE,
                                  vp = vpPath("parent", "plots", "season"),
                                  name = "seasonYaxis",
                                  gp = gpar(cex = .8))
    grobs$seasonLabel = textGrob("Seasonal",
                                 vp = vpPath("parent", "plots", "season"),
                                 name = "seasonLabel", gp = gpar(cex = .9, col = "black",
                                                                 fontface = "bold"),
                                 x = unit(0.02, "npc"), y = unit(0.97, "npc"),
                                 hjust = 0, vjust = 1)


    grobs$randomBorder = rectGrob(vp = vpPath("parent", "plots", "random"),
                                  name = "randomBorder", gp = gpar(lwd = 2))
    grobs$random.y0 = linesGrob(unit(dotted.xcoords, "native"),
                                unit(dotted.ycoords, "native"),
                                vp = vpPath("parent", "plots", "random"),
                                name = "random.y0",
                                gp = gpar(col = "#aaaaaa", lty = "1313"))
    grobs$randomLine = linesGrob(x.units, y.random.units,
                                 vp = vpPath("parent", "plots", "random"),
                                 name = "randomLine",
                                 gp = gpar(col = randCol))
    grobs$randomYaxis = yaxisGrob(main = TRUE,
                                  vp = vpPath("parent", "plots", "random"),
                                  name = "randomYaxis",
                                  gp = gpar(cex = .8))
    grobs$Xaxis = xaxisGrob(gp = gpar(cex = .8),
                            vp = vpPath("parent", "plots", "random"),
                            name = "Xaxis")
    grobs$randomLabel = textGrob("Residuals",
                                 vp = vpPath("parent", "plots", "random"),
                                 name = "randomLabel", gp = gpar(cex = .9, col = "black",
                                                                fontface = "bold"),
                                 x = unit(0.02, "npc"), y = unit(0.97, "npc"),
                                 hjust = 0, vjust = 1)


    grobs$statusText = textGrob(paste("Decomposition of data:", obj$currVar),
                                vp = vpPath("parent", "head"),
                                name = "statusText")

    image = gTree(name = "image", children = grobs,
                   childrenvp = final.vptree)

    ### return a list with all the variables we need
    decompVars = list(tree = image, ranges = ranges, props = props,
                      raw = obj$tsObj@.Data, components = decomp$time.series,
                      vertMargins = vertMargins)
    obj$decompVars = decompVars
    obj
}



decompositionplot <-
function(obj) {
    vars <- decomposition(obj)
    newdevice(width = 6, height = 5)
    drawImage(vars$decompVars$tree)
    vars
}