## --------------------------
## LASR Statistcal testing part
## --------------------------
lasrTest <- function(oneframe, span = NULL, method = "BH",
                     cutoffq = 0.05){
    ## if(inherits(oneframe, "ext.tum")) oneframe = oneframe$frame
    nrow1 <- nrow(oneframe)
    ncol1 <- ncol(oneframe)
    onedf <- data.frame(row = rep(1:nrow1, ncol1),
                        col = rep(1:nrow1, each = ncol1),
                        pixel = as.vector(oneframe))
    if(is.null(span)){
        span <- choose_h(oneframe)
    }

    loessFit <- tryCatch({
        loess(pixel ~ row * col,
              data = onedf, span = span, degree = 2)
    }, warning = function(war){
        
    }, error = function(err){
        b <- e #e is the error message
    }, finally = {
        print("lalala\n")
    })
    
    
    loessFit <- loess(pixel ~ row * col,
                      data = onedf, span = span, degree = 2)
    ## Extract fitted value, standard error, etc. See predict.loess
    fitPred <- predict(loessFit,se = TRUE)
    ## Calculate the t-statistics-frame (as a 1d string)
    t.stat <- fitPred$fit / fitPred$se 
    p.value <- 2 * (1 - pt(abs(t.stat), fitPred$df))
    q.value <- p.adjust(p.value, method = method)
    ## res <- onedf
    ## res$pvalue <- p.value
    ## res$qvalue <- q.value
    ## res$se <- fitRes$se
    ## res$pred <- fitRes$fit
    ## res$is.sig <- q.value < 0.05
    toM <- function(x) return(matrix(x, nrow1, ncol1))
    ans <- list(model = loessFit, predicted = toM(fitPred$fit),
                p = toM(p.value), q = toM(q.value), 
                sig = toM(q.value < cutoffq), data = oneframe)
        class(ans) <- c("lasr", class(ans))
    return(ans)
}

