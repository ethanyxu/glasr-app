## Gaussian density function

gf <- function(x, probs, mus, vars){
    ncomp <- length(probs)
    ans <- rep(0, length(x))
    for(i in 1:length(probs)){
        ans <- ans + probs[i] * dnorm(x, mean = mus[i], sd = sqrt(vars[i]))
    }
    return(ans)
}
