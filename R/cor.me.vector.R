"cor.me.vector" <-
function(exp1,se1,exp2,se2)
{
  npar <- 6
  init <- vector(mode="numeric",length=npar)
  init[1] <- init[2] <- cor(exp1,exp2)
  init[c(3,5)] <- CalcInitials(exp1)
  init[c(4,6)] <- CalcInitials(exp2)
 
  init.uncons <- TransformCon2Uncon(init)

  result.uncons <- optim(init.uncons,fcn.uncons,gradient.uncons,"BFGS",hessian=T,exp1=exp1,se1=se1,exp2=exp2,se2=se2)
  result.par <- TransformUncon2Con(result.uncons$par)
  result.par[5:6] <- sqrt(result.par[5:6])
  names(result.par) <- c("corr.me","corr.true","mu1","mu2","s1","s2")
  return(list(estimate=result.par,counts=result.uncons$counts,convergence=result.uncons$convergence))
  
}
