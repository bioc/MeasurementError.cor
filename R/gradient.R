"gradient" <-
function(x,exp1,se1,exp2,se2)
{
  
  r1 <- x[1]
  r2 <- x[2]
  mu1 <- x[3]
  mu2 <- x[4]
  s1 <- x[5]
  s2 <- x[6]

  g <- vector(mode="numeric",length=length(x))

  g[1] <- sum(-(-2.0*se1*mu1*se2*mu2+2.0*se1*exp2*mu1*se2-2.0*se1*exp1*exp2*se2+2.0*se1*exp1*se2*mu2)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)/2.0+(exp2^2*se1^2-2.0*se1^2*exp2*mu2+mu2^2*se1^2-2.0*se1*r1*mu1*se2*mu2+2.0*se1*r1*exp2*mu1*se2-2.0*se1*r1*exp1*exp2*se2+2.0*se1*r1*exp1*se2*mu2+2.0*exp1*sqrt(s1*s2)*r2*mu2+2.0*r2*mu1*sqrt(s1*s2)*exp2+exp2^2*s1+exp1^2*se2^2+exp1^2*s2+mu1^2*se2^2+mu1^2*s2-2.0*r2*mu1*mu2*sqrt(s1*s2)-2.0*exp1*r2*sqrt(s1*s2)*exp2+mu2^2*s1-2.0*exp1*s2*mu1-2.0*exp1*se2^2*mu1-2.0*exp2*mu2*s1)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)^2*(2.0*se1^2*r1*se2^2+2.0*se1*r2*sqrt(s1*s2)*se2)/2.0+0.5*(-2.0*se1^2*r1*se2^2-2.0*se1*r2*sqrt(s1*s2)*se2)/(se1^2*se2^2+s1*se2^2+se1^2*s2+s1*s2-s1*r2^2*s2-se1^2*r1^2*se2^2-2.0*r1*se1*r2*sqrt(s1*s2)*se2))
  
  g[2] <- sum(-(2.0*exp1*sqrt(s1*s2)*mu2+2.0*mu1*sqrt(s1*s2)*exp2-2.0*mu1*mu2*sqrt(s1*s2)-2.0*exp1*sqrt(s1*s2)*exp2)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)/2.0+(exp2^2*se1^2-2.0*se1^2*exp2*mu2+mu2^2*se1^2-2.0*se1*r1*mu1*se2*mu2+2.0*se1*r1*exp2*mu1*se2-2.0*se1*r1*exp1*exp2*se2+2.0*se1*r1*exp1*se2*mu2+2.0*exp1*sqrt(s1*s2)*r2*mu2+2.0*r2*mu1*sqrt(s1*s2)*exp2+exp2^2*s1+exp1^2*se2^2+exp1^2*s2+mu1^2*se2^2+mu1^2*s2-2.0*r2*mu1*mu2*sqrt(s1*s2)-2.0*exp1*r2*sqrt(s1*s2)*exp2+mu2^2*s1-2.0*exp1*s2*mu1-2.0*exp1*se2^2*mu1-2.0*exp2*mu2*s1)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)^2*(2.0*s1*r2*s2+2.0*r1*se1*sqrt(s1*s2)*se2)/2.0+0.5*(-2.0*s1*r2*s2-2.0*r1*se1*sqrt(s1*s2)*se2)/(se1^2*se2^2+s1*se2^2+se1^2*s2+s1*s2-s1*r2^2*s2-se1^2*r1^2*se2^2-2.0*r1*se1*r2*sqrt(s1*s2)*se2))
  
  g[3] <- sum(-(-2.0*se1*r1*se2*mu2+2.0*se1*r1*exp2*se2+2.0*r2*sqrt(s1*s2)*exp2+2.0*mu1*se2^2+2.0*mu1*s2-2.0*r2*mu2*sqrt(s1*s2)-2.0*exp1*s2-2.0*exp1*se2^2)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)/2.0)
  
  g[4] <- sum(-(-2.0*se1^2*exp2+2.0*mu2*se1^2-2.0*se1*r1*mu1*se2+2.0*se1*r1*exp1*se2+2.0*exp1*sqrt(s1*s2)*r2-2.0*r2*mu1*sqrt(s1*s2)+2.0*mu2*s1-2.0*exp2*s1)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)/2.0)
  
  g[5] <- sum(-(exp1/sqrt(s1*s2)*r2*mu2*s2+r2*mu1/sqrt(s1*s2)*exp2*s2+exp2^2-r2*mu1*mu2/sqrt(s1*s2)*s2-exp1*r2/sqrt(s1*s2)*exp2*s2+mu2^2-2.0*exp2*mu2)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)/2.0+(exp2^2*se1^2-2.0*se1^2*exp2*mu2+mu2^2*se1^2-2.0*se1*r1*mu1*se2*mu2+2.0*se1*r1*exp2*mu1*se2-2.0*se1*r1*exp1*exp2*se2+2.0*se1*r1*exp1*se2*mu2+2.0*exp1*sqrt(s1*s2)*r2*mu2+2.0*r2*mu1*sqrt(s1*s2)*exp2+exp2^2*s1+exp1^2*se2^2+exp1^2*s2+mu1^2*se2^2+mu1^2*s2-2.0*r2*mu1*mu2*sqrt(s1*s2)-2.0*exp1*r2*sqrt(s1*s2)*exp2+mu2^2*s1-2.0*exp1*s2*mu1-2.0*exp1*se2^2*mu1-2.0*exp2*mu2*s1)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)^2*(-se2^2-s2+r2^2*s2+r1*se1*r2/sqrt(s1*s2)*se2*s2)/2.0+0.5*(se2^2+s2-r2^2*s2-r1*se1*r2/sqrt(s1*s2)*se2*s2)/(se1^2*se2^2+s1*se2^2+se1^2*s2+s1*s2-s1*r2^2*s2-se1^2*r1^2*se2^2-2.0*r1*se1*r2*sqrt(s1*s2)*se2))
  
  g[6] <- sum(-(exp1/sqrt(s1*s2)*r2*mu2*s1+r2*mu1/sqrt(s1*s2)*exp2*s1+exp1^2+mu1^2-r2*mu1*mu2/sqrt(s1*s2)*s1-exp1*r2/sqrt(s1*s2)*exp2*s1-2.0*exp1*mu1)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)/2.0+(exp2^2*se1^2-2.0*se1^2*exp2*mu2+mu2^2*se1^2-2.0*se1*r1*mu1*se2*mu2+2.0*se1*r1*exp2*mu1*se2-2.0*se1*r1*exp1*exp2*se2+2.0*se1*r1*exp1*se2*mu2+2.0*exp1*sqrt(s1*s2)*r2*mu2+2.0*r2*mu1*sqrt(s1*s2)*exp2+exp2^2*s1+exp1^2*se2^2+exp1^2*s2+mu1^2*se2^2+mu1^2*s2-2.0*r2*mu1*mu2*sqrt(s1*s2)-2.0*exp1*r2*sqrt(s1*s2)*exp2+mu2^2*s1-2.0*exp1*s2*mu1-2.0*exp1*se2^2*mu1-2.0*exp2*mu2*s1)/(-se1^2*se2^2-s1*se2^2-se1^2*s2-s1*s2+s1*r2^2*s2+se1^2*r1^2*se2^2+2.0*r1*se1*r2*sqrt(s1*s2)*se2)^2*(-se1^2-s1+s1*r2^2+r1*se1*r2/sqrt(s1*s2)*se2*s1)/2.0+0.5*(se1^2+s1-s1*r2^2-r1*se1*r2/sqrt(s1*s2)*se2*s1)/(se1^2*se2^2+s1*se2^2+se1^2*s2+s1*s2-s1*r2^2*s2-se1^2*r1^2*se2^2-2.0*r1*se1*r2*sqrt(s1*s2)*se2))

  return(g)
}
