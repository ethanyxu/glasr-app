## --------------------------
## Plot LASR result
## --------------------------
plot.lasr <- function(x, ...){
    if(!inherits(x, "lasr"))
        stop("Input must be of class 'lasr', i.e. the result of lasr()")
    par(mfrow = c(2,2), mar = c(2,2,2,2))
    aspRatio <- nrow(x$data)/ncol(x$data)
    image(x$data, main = "Original Image",
          asp = aspRatio, xaxt = "n", yaxt = "n", ...)
    image(x$predicted, main = "Smoothed Image",
          asp = aspRatio, xaxt = "n", yaxt = "n", ...)
    image(x$q, main = "q-map",
          asp = aspRatio, xaxt = "n", yaxt = "n", ...)
    image(x$sig, main = "Significant Pixels",
          asp = aspRatio, xaxt = "n", yaxt = "n", ...)
}
