#' Load, trim, fit, and display model
#'
#' @param fname Path to the tcx file
#' @param startminutes Time (default: 0 minutes) at the start of the first step
#' @param endminutes Time (default: 1000, in minutes) at the end of the last step
#' @param speedmin (default: 6 km/h) Speed of the first step (set on treadmill)
#' @param speedstep (default: 1 km/h) Speed increment of each step
#' @param timestep (default: 1.5 minutes) Length of time of each step
#' @param useDeviceSpeed (default: FALSE) If TRUE, use the speed as returned by
#' the device instead of the manually-set step speeds
#'
#' @return a data.frame with early and late times potentially trimmed, and
#' speed potentially over-ridden with manually set step values.
#' @export
#'
#' @examples
#' fname = system.file(file = "extdata/2023-09-15.tcx", package = "TreadmillStepTest")
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
#' library(SiZer)
#' # Fit and plot HR vs time, with ALL HR measurements
#' (fit <- piecewise.linear(x=x1$speed, y=x1$heart_rate, CI = TRUE))
#' fitdat <- data.frame(speed = fit$x, heart_rate = fit$y)
#' ggplot(x1, aes(x=speed, y=heart_rate)) +
#'     geom_point() +
#'     geom_line(data = fitdat, aes(x=speed, y = heart_rate), col = "red", linewidth = 1)
#' # Using only the average of the last 5 HR measurements in each step
#' x2 <- group_by(x1, speed) %>%
#'     mutate(heart_rate = mean(tail(heart_rate, 5))) %>%
#'         select(speed, heart_rate) %>%
#'             unique()
#' (fit2 <- piecewise.linear(x=x2$speed, y=x2$heart_rate, CI = TRUE))
#' fitdat2 <- data.frame(speed = fit2$x, heart_rate = fit2$y)
#' ggplot(x2, aes(x=speed, y=heart_rate)) +
#'     geom_point() +
#'     geom_line(data = fitdat2, aes(x=speed, y = heart_rate), col = "red", linewidth = 1)
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
            filter(minutes >= startminutes & minutes <= endminutes)
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
#'
#' @return creates a plot showing the piecewise fit and breakpoint
#' @export
#'
#' @examples
#' fname = system.file(file = "extdata/2023-09-15.tcx", package = "TreadmillStepTest")
#' x1 <- prepdata(fname, startminutes = 23.8, endminutes = 40.1,
#'          useDeviceSpeed = FALSE)
#' fitmodel(x1)
fitmodel <- function(dat, alldata=FALSE, textsize=5){
    data <- select(dat, speed, heart_rate)
    if (!alldata) {
        # Use only the average of the last 5 HR measurements in each step
        dat <- group_by(dat, speed) %>%
            mutate(heart_rate = mean(tail(heart_rate, 5))) %>%
            unique()
    }
    library(SiZer)
    fit <- piecewise.linear(x=dat$speed, y=dat$heart_rate, CI = FALSE)
    message(show(fit))
    fitdat2 <- data.frame(speed = fit$x, heart_rate = fit$y)
    spd <- paste(round(fit[[1]], 1), "km/h")
    pace <- paste(round(60/fit[[1]], 1), "min/km")
    ggplot(dat, aes(x=speed, y=heart_rate)) +
        geom_point() +
        geom_line(data = fitdat2, aes(x=speed, y = heart_rate), col = "red", linewidth = 1) +
        annotate(geom = 'text', label = spd, x = Inf, y = -Inf, hjust = 1.1, vjust = -1, size=textsize) +
        annotate(geom = 'text', label = pace, x = Inf, y = -Inf, hjust = 1.1, vjust = -2.5, size=textsize)
}
