##' The function \code{iNZightTS} is used to create time-series objects used
##' in iNZight.
##'
##' The function \code{iNZgithTS} is used to create time-series
##' objects. Unlike \code{ts} objects, these are lists containing
##' information about the time-series as well as the data and the
##' time-series (\code{ts} object) itself.
##' \cr \cr
##' If a \code{ts} object is used to create the \code{iNZightTS} object,
##' all the domain information is extracted from that object.
##'\cr \cr
##' In case of \code{data} being a data.frame or path to a \code{.csv}
##' file and \code{start} being omitted, the starting date and the
##' \code{freq} is extracted from the column that includes the time
##' information. This column is either named \code{"Time"} or is the first
##' column. If \code{end} is omitted, all of the data will be used for the
##' time-series.
##'
##' @title iNZightTS (Time-Series) Objects
##' 
##' @aliases iNZightMTS
##'
##' @param data a \code{data.frame} containing time information and observation or a
##' path to a \code{.csv} file with such information or a \code{ts} object
##'
##' @param start the time of the first observation. Either a single number or a vector
##' of two integers, which specify a natural time unit and a (1-based)
##' number of samples into the time unit
##'
##' @param end the time of the last observation, specified in the same way as \code{start}
##'
##' @param freq the number of observations per unit of time
##'
##' @param var the column number or name for the observations used from \code{data}
##' in the actual time series
##'
##' @param ... additional information passed to \code{read.csv()} and used when
##' \code{data} is a path
##'
##' @seealso \code{\link{ts}}, \code{\link{print.iNZightTS}}, \code{\link{rawplot}}
##'
##' @examples # create from a ts object
##' z <- iNZightTS(UKgas)
##' rawplot(z)
##'
##' # create from a data.frame
##' x <- iNZightTS(data.frame(Return = rnorm(100), Time = 1:100), var = "Return")
##'
##' # create from a data.frame with modified time frame
##' y <- iNZightTS(data.frame(Return = rnorm(100)), start = c(1990, 1), end =
##' c(1993, 5), freq = 12, var = 1)
##' rawplot(y)
##'
##' @export
iNZightTS <-
    function(data, start=1, end=numeric(), freq=1, var=2, ...) {

        inzightts <- list()

        ## if the input is an object of class "ts", just extract info
        if (any(grepl("^ts$", class(data)))) {
            inzightts$start <- start(data)
            inzightts$end <- end(data)
            inzightts$freq <- frequency(data)
            inzightts$tsObj <- data
            if (is.null(dim(data))) {
                inzightts$currVar <- 1
                inzightts$data <- as.vector(data)
            } else {
                if (is.null(colnames(data)))
                    inzightts$currVar <- 1:dim(data)[2]
                else
                    inzightts$currVar <- colnames(data)
                inzightts$data <- data.frame(matrix(as.vector(data), ncol=dim(data)[2]))
                colnames(inzightts$data) <- colnames(data)
            }
        } else {
            ## use either a data.frame or a file location as input
            if (is.character(data))
                data <- read.csv(data, as.is=TRUE, ...)

            inzightts <- list()
            inzightts$data <- data

            ## try to find the time column
            ## search through the names
            time.col <- grep("time", names(data), ignore.case = TRUE)[1]
            if (is.na(time.col))
                time.col <- 1

            ts.struc <- get.ts.structure(data[, time.col])

            if (missing(start))
                start <- ts.struc$start

            if (missing(freq))
                freq <- ts.struc$frequency

            if (any(c(is.na(start), is.na(freq))))
                stop("There is an error in your time series, a hole in the series perhaps?")

            inzightts$start <- start
            inzightts$freq <- freq
            ## calculate end if it is missing
            if (missing(end)) {
                if (length(start) > 1L) {
                    end <- numeric(2)
                    end[1] <- start[1] + (length(data[, var[1]]) + start[2] - 1) %/% freq
                    end[2] <- (length(data[, var[1]]) + start[2] - 1) %% freq
                } else{
                    end <- start + length(data[, var[1]]) - 1
                }
            }
            inzightts$end <- end
            inzightts$tsObj <- ts(data[, var], start = start, end = end,
                                  frequency = freq)
            if (is.numeric(var))
                inzightts$currVar <- names(data)[var]
            else
                inzightts$currVar <- var
        }

        class(inzightts) <- "iNZightTS"
        if (length(inzightts$currVar) > 1)
            class(inzightts) <- c("iNZightMTS", "iNZightTS")

        inzightts
    }


get.ts.structure <-
    function(vardata) {
        if (is.factor(vardata))
            vardata <- as.character(vardata)

        if (any(is.na(vardata))) {
            return(list(start = NA, freq = NA))
        }

        if (is.numeric(vardata)) {
            if (any(vardata != round(vardata)))
                return(list(start = NA, frequency = NA))
            vardata <- as.character(vardata)
            if (any(nchar(vardata) > 4))
                return(list(start = NA, frequency = NA))
        }

        ## What frequency is the data at (monthly, quarterly, yearly)
        firstval <- vardata[1]
        if (nchar(firstval) > 7)
            return(list(start = NA, frequency = NA))
        if (nchar(firstval) > 4) {
            interval <- substring(firstval, 5, 5)
            freq <- ifelse(interval == "Q", 4, 12)
        } else {
            interval <- "A"
            freq <- 1
        }

        ## form the start value (used when we create ts objects)
        start <- as.integer(substring(firstval, 1 ,4))
        if (interval != "A")
            start <- c(start, as.integer(substring(firstval, 6)))

        ## Checking that we have no holes in the time variable itself
        n <- length(vardata)
        lastyear <- if (is.character(vardata))
            as.integer(substring(tail(vardata, 1), 1, 4))
        else
            tail(vardata, 1)
        if (interval == "A") {
            valid.time <- all(as.character(start:(start + (n - 1))) == vardata)
            if (! valid.time) {
                return(list(start = NA, frequency = NA))
            }
        }

        if (interval == "Q") {
            n.in.first.year <- freq - start[2] + 1
            n.rest <- n - n.in.first.year
            expected.years <- c(rep(start[1], n.in.first.year),
                                rep((start[1] + 1):lastyear, each = freq, length.out = n.rest))
            expected.quarters <- c(start[2]:(start[2] + (n.in.first.year - 1)),
                                   rep(1:4, length.out = n.rest))
            expected.times <- paste(expected.years, "Q", expected.quarters, sep = "")
            if (length(expected.times) != length(vardata) || ! all(vardata == expected.times)) {
                return(list(start = NA, frequency = NA))
            }
        }

        if (interval == "M") {
            n.in.first.year <- freq - start[2] + 1
            n.rest <- n - n.in.first.year
            expected.years <- c(rep(start[1], n.in.first.year),
                                rep((start[1] + 1):lastyear, each = freq, length.out = n.rest))
            expected.months <- c(start[2]:(start[2] + (n.in.first.year - 1)),
                                 rep(1:12, length.out = n.rest))
            expected.times <- paste(expected.years, "M",
                                    sprintf("%02d", expected.months), sep = "")
            if (length(expected.times) != length(vardata) ||
                ! all(vardata == expected.times)) {
                return(list(start = NA, frequency = NA))
            }
        }

        list(start = start, frequency = freq)
    }

