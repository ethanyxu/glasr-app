## This function is taken from 'bisoreg' package by
## S. McKay Curtis <s.mckay.curtis at gmail.com>
## and modified to handle more than one predictor

choose_h<- function(oneframe,
                    span.vals = seq(0.01, 0.5, by = 0.05),
                    folds = 10){
    nrow1 <- nrow(oneframe)
    ncol1 <- ncol(oneframe)
    onedf <- data.frame(row = rep(1:nrow1, ncol1),
                        col = rep(1:nrow1, each = ncol1),
                        pixel = as.vector(oneframe))
    x <- onedf[,1:2]
    y <- onedf[,3]
    ## mae <- numeric(length(span.vals))
    ## mae <- vector('numeric', length = length(span.vals) )
    theta.fit <- function(x, y, span){
        tp.frame <- as.data.frame(cbind(x,y))
        loess(y ~ row * col, data = tp.frame, span = span, degree = 2)
    }
    theta.predict <- function(fit, x0) predict(fit, newdata = x0)

    evaluate_span <- function(span){
        y.cv <- crossval(x, y, theta.fit, theta.predict,
                         span = span, ngroup = folds)$cv.fit
        fltr <- !is.na(y.cv)
        return(mean((y[fltr]-y.cv[fltr])^2))
    }
    mse <- pbsapply(span.vals, FUN = evaluate_span)

        ## for(span in span.vals){
        ## ii <- ii+1
        ## y.cv <- crossval(x, y, theta.fit, theta.predict,
        ##                  span = span, ngroup = folds)$cv.fit
        ## fltr <- !is.na(y.cv)
        ## mae[ii] <- mean(abs(y[fltr]-y.cv[fltr]))
    ## }
    span <- span.vals[which.min(mse)]
    ## out <- loess(y ~ x,span = span)
    return(span)
}
