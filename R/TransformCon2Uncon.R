"TransformCon2Uncon" <-
function(con)
{
  uncon <- vector(mode="numeric",length=length(con))
  
  uncon[1] <- tan(pi/2*con[1])
  uncon[2] = tan(pi/2*con[2])
  uncon[3] = con[3]
  uncon[4] = con[4]
  uncon[5] = log(con[5])
  uncon[6] = log(con[6])

  return(uncon)
}
