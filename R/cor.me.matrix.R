"cor.me.matrix" <-
function(exp,se)
  {

    p <- nrow(exp)
    n <- ncol(exp)

    cor.me <- diag(rep(1,p))
    
   for ( i in 1:(p-1))
     for (j in (i+1):p)
       {
  #       cat("i=",i," j=",j,"\n")
         cor.me[i,j] <- cor.me.vector(as.numeric(exp[i,]),as.numeric(se[i,]),as.numeric(exp[j,]),as.numeric(se[j,]))$estimate[2]
         cor.me[j,i] <- cor.me[i,j]
       }
    return(list(corr.true=cor.me))
  }
