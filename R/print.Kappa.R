# Add a column showing z values 
## NB: should probably set digits =  
print.Kappa <-
function (x, ...) 
{
    tab <- rbind(x$Unweighted, x$Weighted)
    z <- tab[,1] / tab[,2]
#    lower95 <- tab[,1] - qnorm(.975) * tab[,2]
#    upper95 <- tab[,1] + qnorm(.975) * tab[,2]
    tab <- cbind(tab, z)
    rownames(tab) <- names(x)[1:2]
    print(tab, ...)
    invisible(x)
}
