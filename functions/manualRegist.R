manualRegist <- function(oneframe, first, second, third){
    ## x0 <- 5
    ## y0 <- 15
    ## x1 <- 25
    ## y1 <- 18
    ## x3 <- 28
    ## y3 <- 17
    nrows <- nrow(oneframe)
    ncols <- ncol(oneframe)
    x0 <- first$x
    y0 <- first$y
    x1 <- second$x
    y1 <- second$y
    x3 <- third$x
    y3 <- third$y
    
    M <- (y1 - y0) / (x1 - x0)

    xie <- sqrt(1 + M^2)
    sin1 <- -M / xie
    cos1 <- 1 / xie
    
    Rmid = matrix(NA, 2,2)
    Rmid[1,1] = cos1
    Rmid[1,2] = -sin1
    Rmid[2,1] = sin1
    Rmid[2,2] = cos1
    
    target = c(ncols, nrows/2)
    
    thePoint = Rmid %*% c(x3, y3)
    
    midpointdiff = target - thePoint
    u = midpointdiff[1,1]
    v = midpointdiff[2,1]
    
    R <- matrix(c(cos1, sin1, 0, -sin1, cos1, 0, u, v, 1), 3)
    newCoordinates <- matrix(c(rep(1:nrows, times = ncols),
                               rep(1:nrows, each = ncols),
                               rep(1, nrows * ncols)), byrow = TRUE, nrow = 3)
    
    oldCoordinates <- solve(R) %*% newCoordinates

    X2 <- matrix(oldCoordinates[1,], nrows)
    Y2 <- matrix(oldCoordinates[2,], nrows)
    ZZ <- matrix(interp2(1:ncols, 1:nrows, t(oneframe), oldCoordinates[1,],
                         oldCoordinates[2,], method = "linear"), nrow = nrows, ncol = ncols)
    ZZ[is.na(ZZ)] <- 0
    return(ZZ)
}
