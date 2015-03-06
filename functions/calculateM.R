############################################
##--Calculate slope from the first image--##
############################################
calculateM <- function(oneframe){
    rowcount <- nrow(oneframe)
    colcount <- ncol(oneframe)
    zerocount <- 0
    total <- 0
    middle <- floor(rowcount/2)
    midline <- matrix(rep(0, colcount), ncol = 1)
    
    for (col in 1:colcount){
        counthalf1 <- 0
        counthalf2 <- 0
        ##first half of the image
        for (row in 1:middle){
            if(oneframe[row, col] == 0){
                zerocount <- zerocount + 1
            }else{
                counthalf1 <- counthalf1 + 1
            }
        }
        ##second half of the image
        for (p in (middle+1) : rowcount){
            if(oneframe[p, col] == 0){
                zerocount <- zerocount + 1
            }else{
                counthalf2 <- counthalf2 + 1
            }
        }
        hold1 <- (counthalf1 - counthalf2) / 2
        midline[col,1] <- middle - hold1
        ##this is the location of the middle of the non-zero points
        total <- total + midline[col,1]
    }
    
    ##fit a linear model using the middle points
    x <- as.matrix(c(1 : colcount), ncol = 1)
    N <- dim(x)[1]
    
    a <- matrix(0, nrow = 2, ncol = 2)
    a[1,1] <- sum(x)
    a[1,2] <- sum(x * x)
    a[2,1] <- N
    a[2,2] <- a[1,1]
    
    b <- matrix(0, nrow = 1, ncol = 2)
    b[1,1] <- sum(midline)
    b[1,2] <- sum(x * midline)
    
    con <- b %*% solve(a)
    ##slope
    M <- con[,1]
    return(M)
}
