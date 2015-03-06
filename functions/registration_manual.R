registration_manual <- function(oneframe){
    p <- nrow(oneframe)
    n <- ncol(oneframe)
    graph.f <- data.frame(x = rep(1 : p, n),
        y = rep(1 : n,each = p), pixel = c(oneframe))
    mycol <- gray((0:32)/32)
    
    image(1 : (n+1), 1:(p+1), rot(oneframe), asp = 1,
          col = mycol, xlab = "", ylab = "",
          main = "Select two points from the graph.", cex.main = 0.8)
    xy1 <- locator(1)
    points(xy1$x, xy1$y, col = "red", cex = 0.4, pch = 19)
    points(xy1$x, xy1$y, col = "red", cex = 0.8)
    xy2 <- locator(1)
    points(xy2$x, xy2$y, col = "red", cex = 0.4, pch = 19)
    points(xy2$x, xy2$y, col = "red", cex = 0.8)
    
    x1 <- xy1$x; y1 <- xy1$y
    x2 <- xy2$x; y2 <- xy2$y
    segments(x1, y1, x2, y2, col = "red", cex = 0.5)
    
    return(data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
}

## registration_manual(oneframe)
