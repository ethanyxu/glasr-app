## --------------------------
## Automatic Registration
## --------------------------
## install.packages("RNiftyReg")
## library(RNiftyReg)

registration <- function(source, target, scope = "rigid"){
    ans <- niftyreg.linear(source, target, scope = scope)
    return(as.matrix(ans$image))
}
