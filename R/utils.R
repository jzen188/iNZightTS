### File : utils.R
### Modified by : Chris Park <cpar137@aucklanduni.ac.nz>
### Description : Modified the "newdevice" function.
### Date modified : December 12, 2014.

get.x <-
    function(tsObj) {
### figure out the limits and step size along the x axis
        f = frequency(tsObj)
        s = start(tsObj)
        if (f == 1) {
            start.x = s[1]
            step.x = 1
            end.x = start.x + length(tsObj) - 1
        }
        else {
            step.x = 1/f
            start.x = s[1] + (s[2] - 1) * step.x
            end.x = start.x + step.x * (length(tsObj) - 1)
        }

        x = seq(start.x, end.x, by = step.x)
        x.units = unit(x, "native")
        list(x = x, x.units = x.units)
    }

get.x2 <-
    function(tsObj) {
### figure out the limits and step size along the x axis
        f = frequency(tsObj)
        s = start(tsObj)
        if (f == 1) {
            start.x = s[1]
            step.x = 1
            end.x = start.x + length(tsObj) - 1
        }
        else {
            step.x = 1/f
            start.x = s[1] + (s[2] - 1) * step.x
            end.x = start.x + step.x * (length(tsObj) - 1)
        }

        x = seq(start.x, end.x, by = step.x)
        x = order(x)
        x = x/max(x)
        x.units = unit(x, "native")
        list(x = x, x.units = x.units)
    }

get.line.coords <-
    function(vars.decomp, vpName, lineGrobName) {
        decomp = vars.decomp$decompVars
        seekViewport(vpName)
        line = getGrob(decomp$tree, lineGrobName)
        line.y = convertUnit(line$y, attr(line$y[1], "unit"), valueOnly = TRUE)
        line.vp.yrange = current.viewport()$yscale
        line.y.npc = (line.y - line.vp.yrange[1]) / diff(line.vp.yrange)
        line.y.parent = switch(vpName,
            season = decomp$props["remainder"] +
            line.y.npc * decomp$props["seasonal"],
            random = line.y.npc * decomp$props["remainder"],
            trend = line.y.npc * decomp$props["trend"] +
            decomp$props["seasonal"] +
            decomp$props["remainder"])
        line.x = convertUnit(line$x, "native", valueOnly = TRUE)
        line.vp.xrange = current.viewport()$xscale
        line.x.npc = (line.x - line.vp.xrange[1]) / diff(line.vp.xrange)
        x.parent = line.x.npc

        list(line.y = line.y, line.vp.yrange = line.vp.yrange,
             line.y.npc = line.y.npc, line.y.parent = line.y.parent,
             line.x = line.x, line.vp.xrange = line.vp.xrange,
             line.x.npc = line.x.npc, x.parent = x.parent,
             line.col = line$gp$col)
    }



add.line.plots.vp <-
    function(vars.decomp, vpName, lineCol = "red",
             name = paste(vpName, "copy", sep = ".")) {
        z = get.line.coords(vars.decomp, vpName, paste(vpName, "Line", sep = ""))
        lineCopy = linesGrob(unit(z$x.parent, "npc"),
            unit(z$line.y.parent, "npc"),
            name = name,
            vp = vpPath("parent", "plots"),
            gp = gpar(col = lineCol))
        updated.tree = addGrob(vars.decomp$decompVars$tree, lineCopy)
        vars.decomp$decompVars$tree = updated.tree
        vars.decomp
    }



newdevice <-
    function(width, height, ...) {
        ##  Check if "shiny" is currently loaded.
        if ("package:shiny" %in% search())
            ##  Let shiny set default graphics device, i.e. height and width.
            return()    
        ##  Check if "Acinonyx" is installed. This is a logical vector that returns
        ##  TRUE if installed and FALSE otherwise.
        checkAcinonyx <- "Acinonyx" %in% rownames(installed.packages())        
        if (checkAcinonyx) {
            ##  If "Acinonyx" is installed, check if it can be successfully loaded.
            ##  If so, use the "idev" device provided by the package.
            tryAcinonyx <- suppressWarnings(try(library(Acinonyx), silent = TRUE))
            if (!inherits(ac, "try-error")) {             
                ##  Acinonyx uses pixels rather than inches, convert inches to
                ##  pixels to determine dims. Assume 90 dpi.        
                width.in <- round(width * 90)
                height.in <- round(height * 90)
                Acinonyx::idev(width = width.in, height = height.in)
                ##  If we cannot load "Acinonyx" successfully, issue a helpful warning
                ##  message for users.
            } else {
                gmessage(paste("Unfortunately, the package used for animation",
                               "in iNZightVIT is incompatible with your system.",
                               "While you can still use VIT, you may experience",
                               "some animation issues. We suggest you download",
                               "our iNZightVIT module for older Mac OS: \n\n", ,
                               "https://www.stat.auckland.ac.nz/~wild/iNZight/mac.html"),
                         title = "VIT Animation Compatiblity Issue")            
            }
            ##  If Acinonyx is NOT installed, we handle the windows and non-windows
            ##  Operating Systems separately.
        } else {
            ##  If OS is non-windows...
            if (.Platform$OS.type != "windows") {
                ##  Note that there exist three variants of the cairo-based device.
                ##  See ?X11() for more details.
                ## 
                ##  If the "cairoDevice" package is loaded, we use the Cairo() graphics
                ##  device as it supports anti-aliasing and produces visually pleasing
                ##  plots across all platforms.
                if ("package:cairoDevice" %in% search()) { # For consistency    
                    cairoDevice::Cairo(width = width, height = height, ...)
                } else {
                    ##  If "cairoDevice" is NOT intalled, issue a helpful warning message.
                    if (!("cairoDevice" %in% rownames(installed.packages()))) {
                        warning("We suggest you install the `cairoDevice`
                                package for better animations")
                        ##  We use a buffered device as otherwise repainting when the
                        ##  window is exposed will be slow.
                        X11(width = width, height = height, type = "cairo", ...)
                        ##  If the "cairoDevice" package is installed but not loaded,
                        ##  load the package and use the cross-platform "Cairo" device.
                    } else { 
                        library(cairoDevice)
                        cairoDevice::Cairo(width = width, height = height, ...)
                    }
                }
                ##  If OS is "windows", then we use the default windows device, which
                ##  supports double buffering by default.
            } else { 
                dev.new(width = width, height = height, ...)
            }
        }
    }



drawImage <-
    function(image) {
        ##  if ("Acinonyx" %in% rownames(installed.packages()))
        ##  if "Acinonyx" is loaded, then use plot.new(.)
        ##  Unsure as to why this is necessary, but left as is
        ##  for now. 
        if ("package:Acinonyx" %in% search())
            plot.new()
        ## Draws current image in device.
        grid.newpage()
        ## On some devices (notably on Mac) we end up being unable to
        ## see anything besides a single frame due to buffering.
        ## dev.hold() and dev.flush() will force the device to hold
        ## and flush currently buffered frames.
        if (exists("dev.hold")) 
            dev.hold(1)
        grid.draw(image)
        if (exists("dev.flush"))
            dev.flush(1)   
    }


###  The dev.hold() and dev.flush() functions hold and flush frames.
###  Moreover, they are part of the grDevices package, which is
###  included in R by default. For non-windows graphics devices
###  that may or may not come with "double buffering", holding
###  and flushing an image for every iteration allows us to overcome
###  the problem of "flickering".

pauseImage <-
    function(image, pause = 1) {
        for (i in 1:pause) {
            if (exists("dev.hold"))
                dev.hold(1)
            drawImage(image)
            if (exists("dev.flush"))
                dev.flush(1)
        }
    }



rmGrobs <-
    function(image, grobs) {
        for (i in grobs) {
            if (i %in% childNames(image)) {
                image <- removeGrob(image, gPath(i))
            }
        }
        image
    }

