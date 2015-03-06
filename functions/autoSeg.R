## --------------------------
## Segmentation
## --------------------------
autoSeg <- function(oneframe, nclust = 3){
    ## ans <- densityMclust(as.vector(oneframe), G = nclust)
    ## ans1 <- Mclust(as.vector(oneframe), G = nclust)
    oneframe_trimmed <- oneframe
    upper_q <- quantile(oneframe, 0.95)
    oneframe_trimmed[oneframe > upper_q] <- upper_q
    
    mydensity <- tryCatch({
        densityMclust(as.vector(oneframe_trimmed), G = nclust)
    },
                          warning = function(war){
                              list(type = "warning", data = oneframe, note = war)
                          },
                          error = function(err){
                              list(type = "error", data = oneframe, note = err)
                          })
    
    if(is.null(mydensity$type)){
        mydensity$type <- "good"
        mydensity$nrow <- nrow(oneframe)
        mydensity$ncol <- ncol(oneframe)
        mydensity$orgData <- oneframe
    }
    ## image(matrix(ans1$classification, 32))
    ## incOrder <- order(oneframe)
    ## plot(oneframe[incOrder], mydensity$density[incOrder], type = "l")
    return(mydensity)
}


