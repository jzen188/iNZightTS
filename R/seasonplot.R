seasonplot <-
function(obj)
    UseMethod("seasonplot")




seasonplot.forecast <-
function(x, s, season.labels=NULL, year.labels=FALSE, year.labels.left=FALSE,
    type="o", main, ylab="", xlab=NULL, col=1, labelgap=0.1, ...)
{
  if(missing(main))
    main = paste("Seasonal plot:", deparse(substitute(x)))
  if(missing(s))
    s = frequency(x)
  if(s<=1)
    stop("Frequency must be > 1")

  # Pad series
  tsx <- x
  if(start(x)[2]>1)
    x <- c(rep(NA,start(x)[2]-1),x)
  x <- c(x,rep(NA,s-length(x)%%s))
  Season <- rep(c(1:s,NA),length(x)/s)
  xnew <- rep(NA,length(x))
  xnew[!is.na(Season)] <- x

  if(s == 12)
  {
    labs <- month.abb
    xLab <- "Month"
  }
  else if(s == 4)
  {
    labs <- paste(month.abb[c(1, 4, 7, 10)],
                  month.abb[c(3, 6, 9, 12)],
                  sep = " - ")
    xLab <- "Quarter"
  }
  else if(s == 7)
  {
    labs <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
    xLab <- "Day"
  }
  else
  {
    labs <- NULL
    xLab <- "Season"
  }
  if(is.null(xlab))
    xlab <- xLab
  if(is.null(season.labels))
    season.labels <- labs
  if(year.labels)
    xlim <- c(1-labelgap,s+0.4+labelgap)
  else
    xlim<-c(1-labelgap,s)
  if(year.labels.left)
    xlim[1] <- 0.4-labelgap
  plot(Season,xnew,xaxt="n",xlab=xlab,type=type,ylab=ylab,main=main,xlim=xlim,col=0,...)
  nn <- length(Season)/s
  col <- rep(col,nn)[1:nn]
  for(i in 0:(nn-1))
    lines(Season[(i*(s+1)+1) : ((s+1)*(i+1))], xnew[(i*(s+1)+1) : ((s+1)*(i+1))], type = type, col = col[i+1], ...)
  if(year.labels)
  {
    idx <- which(Season[!is.na(xnew)]==s)
    year <- time(tsx)[idx]
    text(x=rep(s+labelgap,length(year)),y=tsx[idx],labels=paste(c(trunc(year))),adj=0,...,col=col[1:length(idx)])
  }
  if(year.labels.left)
  {
    idx <- which(Season[!is.na(xnew)]==1)
    year <- time(tsx)[idx]
    if(min(idx)>1) # First year starts after season 1n
      col <- col[-1]
    text(x=rep(1-labelgap,length(year)),y=tsx[idx],labels=paste(c(trunc(year))),adj=1,...,col=col[1:length(idx)])
  }
  if(is.null(labs))
    axis(1,...)
  else
    axis(1,labels=season.labels,at=1:s,...)
}




seasonplot.iNZightTS <-
function(obj) {

    # if there is no season component to the ts, can't create season plot
    if (length(obj$start) == 1)
        stop("Time Series does not have a seasonal component")


    dev.new(width = 9, height = 7)

    freq = obj$freq
    s = obj$start[2]
    n = length(obj$tsObj)
    r = n - (freq + 1 - s)
    numSeries = 1 + r %/% freq + (r %% freq > 0)
    cols = colorRampPalette(c("darkorange", "blue"))(numSeries)
    title = paste("Seasonal plot for", obj$currVar)

    opar = par(mfrow = c(1, 2), cex.axis = 0.9)
    on.exit(par(opar))

    seasonplot.forecast(obj$tsObj, freq, col = cols, pch = 19,
               year.labels = TRUE, year.labels.left = TRUE,
               main = title)

    obj = decomposition(obj)
    season = obj$decompVars$components[,"seasonal"]
    season = if (s > 1) season[-(1:(freq + 1 - s))][1:freq]
    else season[1:freq]

    season.ts = ts(season, start = c(1, 1), frequency = freq)

    if (freq == 12) {
        labs = month.abb
        xlab = "Month"
    }
    else if (freq == 4) {
        labs = paste(month.abb[c(1, 4, 7, 10)],
                     month.abb[c(3, 6, 9, 12)],
                     sep = " - ")
        xlab = "Quarter"
    }
    else if (freq == 7) {
        labs = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
        xlab = "Day"
    }
    else {
        labs = 1:freq
        xlab = "Season"
    }

    plot(season.ts, type = "n", ylab = NULL, xlab = xlab,
         xaxt = "n", main = "Estimated seasonal effects")
    abline(h = 0, col = "#aaaaaa", lty = "dashed")
    lines(season.ts, type = "o", lwd = 2, cex = 1.2)
    axis(1, at = get.x(season.ts)$x, labels = labs)
}