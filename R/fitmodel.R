#' Load, trim, fit, and display model
#'
#' @param fname Path to the tcx file
#' @param startminutes Time (default: 0 minutes) at the start of the first step
#' @param endminutes Time (default: 1000, in minutes) at the end of the last step
#' @param speedmin (default: 6 km/h) Speed of the first step (set on treadmill)
#' @param speedstep (default: 1 km/h) Speed increment of each step
#' @param timestep (default: 1.5 minutes) Length of time of each step in minutes
#' @param useDeviceSpeed (default: FALSE) If TRUE, use the speed as returned by
#' the device instead of the manually-set step speeds
#' @details
#' Actually you don't need to import a TCX file, what matters for the
#' `fitmodel()` function is that the data.frame has columns `time`, `heart_rate`,
#' and optionally `speed`.
#'
#' If you import a TCX file that is not from Garmin, you may need to rename the
#' column containing heart rate to `heart_rate` and the column containing
#' time to `time`. The `time` column should be in seconds or a format that
#' can be coerced to seconds using `as.numeric()`, such as the POSIXct/POSIXlt
#' formats that most services likely provide. If `useDeviceSpeed` is FALSE, then
#' the speed column should be `speed`.
#'
#' @return a data.frame with early and late times potentially trimmed, and
#' speed potentially over-ridden with manually set step values.
#' @export
#' @importFrom dplyr select mutate group_by filter arrange
#' @importFrom ggplot2 ggplot geom_point geom_line annotate aes
#' @importFrom methods show
#' @importFrom stats time
#' @importFrom utils tail
#'
#' @examples
#' fname = system.file(file = "extdata/2023-09-15.tcx", package = "ConconiAnaerobicThresholdTest")
#' # These plots can help get the start and end time correct.
#' x0 <- prepdata(fname, useDeviceSpeed = TRUE)
#' par(mfrow=c(2, 2))
#' plot(x0$minutes, x0$speed)
#' plot(x0$minutes, x0$cadence_running)
#' plot(x0$minutes, x0$heart_rate)
#' # Once you have start and end times correct, set useDeviceSpeed = FALSE
#' # if speeds were set manually on the treadmill.
#' x1 <- prepdata(fname, startminutes = 23.8, endminutes = 40.1,
#'          useDeviceSpeed = FALSE)
#' par(mfrow=c(2, 2))
#' plot(x1$minutes, x1$speed)
#' plot(x1$minutes, x1$cadence_running)
#' plot(x1$minutes, x1$heart_rate)
prepdata <-
    function(fname,
             startminutes = 0,
             endminutes = 1000,
             speedmin = 6,
             speedstep = 1,
             timestep = 1.5,
             useDeviceSpeed=FALSE
             ) {
        x = trackeR::readTCX(fname)
        x = arrange(x, time) |>
            mutate(minutes = as.numeric((time - time[1]) / 60)) |>
            filter(minutes >= startminutes & minutes <= endminutes) |>
            mutate(minutes = minutes - min(minutes))
        endminutes <- min(endminutes, max(x$minutes))
        if (!useDeviceSpeed) {
            calcspeed <- (floor((x$minutes - min(x$minutes) + .001) / timestep) * speedstep) + speedmin
            x$speed = calcspeed
        }
        x <- filter(x, !is.na(x$speed))
        return(x)
    }

#' Fit piecewise linear model
#'
#' @param dat data.frame output by the prepdata() function
#' @param alldata If FALSE (default), only the final 5 heart rate measurements
#' of each step are used to fit the changepoint model. If TRUE, all data are used.
#' @param textsize size of the breakpoint speed & pace text printed on plot (default: 5)
#' @param title title of plot (default: '')
#'
#' @return creates a plot showing the piecewise fit and breakpoint
#' @export
#' @importFrom dplyr select group_by mutate
#' @importFrom SiZer piecewise.linear
#' @importFrom ggplot2 ggplot geom_point geom_line annotate xlab ylab ggtitle
#'
#' @examples
#' fname = system.file(file = "extdata/2023-09-15.tcx", package = "ConconiAnaerobicThresholdTest")
#' x1 <- prepdata(fname, startminutes = 23.8, endminutes = 40.1,
#'          useDeviceSpeed = FALSE)
#' fitmodel(x1)
fitmodel <- function(dat, alldata=FALSE, textsize=5, title=""){
    dat <- select(dat, speed, heart_rate)
    if (!alldata) {
        # Use only the average of the last 5 HR measurements in each step
        dat <- group_by(dat, speed) |>
            mutate(heart_rate = mean(tail(heart_rate, 5))) |>
            unique()
    }
    fit <- piecewise.linear(x=dat$speed, y=dat$heart_rate, CI = FALSE)
    message(show(fit))
    fitdat2 <- data.frame(speed = fit$x, heart_rate = fit$y)
    spd <- paste(round(fit[[1]], 1), "km/h")
    pace <- paste(round(60/fit[[1]], 1), "min/km")
    hr <- paste(round(fit$y[max(which(fit$x < fit$change.point))]), "bpm")
    ggplot(dat, aes(x=speed, y=heart_rate)) +
        geom_point() +
        geom_line(data = fitdat2, aes(x=speed, y = heart_rate), col = "red", linewidth = 1) +
        annotate(geom = 'text', label = spd, x = Inf, y = -Inf, hjust = 1.1, vjust = -1, size=textsize) +
        annotate(geom = 'text', label = pace, x = Inf, y = -Inf, hjust = 1.1, vjust = -2.5, size=textsize) +
        annotate(geom = 'text', label = hr, x = Inf, y = -Inf, hjust = 1.1, vjust = -4, size=textsize) +
        xlab("Speed (km/h)") +
        ylab("Heart rate (bpm)") +
        ggtitle(title)
}

heart_rate = NULL
speed = NULL
minutes = NULL
