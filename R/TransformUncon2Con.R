"TransformUncon2Con" <-
function(uncon)
{
  con <- vector(mode="numeric",length=length(uncon))
  
  con[1] = 2/pi*atan(uncon[1])
  con[2] = 2/pi*atan(uncon[2])
  con[3] = uncon[3]
  con[4] = uncon[4]
  con[5] = exp(uncon[5])
  con[6] = exp(uncon[6])

  return(con)
}
