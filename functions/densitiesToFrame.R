densitiesToFrame <- function(x){
    if(x$type %in% c("warning", "error")){
        oneframe <- x$data
    }else{
        oneframe <- x$orgData
        oneframe[x$classification == 1] <- 0
        oneframe <- matrix(oneframe, x$nrow, x$ncol)
    }
    return(oneframe)
}
