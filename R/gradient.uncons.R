"gradient.uncons" <-
function(x, exp1,se1,exp2,se2)
{
  y <- TransformUncon2Con(x)
  ans <- gradient(y,exp1,se1,exp2,se2)
  
  ans[1] <- ans[1] * 2/pi/(1+x[1]^2)
  ans[2] <- ans[2] * 2/pi/(1+x[2]^2)
  ans[5] <- ans[5] * exp(x[5])
  ans[6] <- ans[6] * exp(x[6])

 return(ans)
 }
