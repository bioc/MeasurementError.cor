"fcn.uncons" <-
function(x,exp1,se1,exp2,se2)
{
  
  y <- TransformUncon2Con(x)
  return(fcn(y,exp1,se1,exp2,se2))
}
