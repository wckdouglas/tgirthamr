binomTest <- function(a,b,seqErr){
    return(pbinom(b-a-1,b, p = 1- seqErr, lower.tail=T))
}
