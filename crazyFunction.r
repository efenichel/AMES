crazyfunction <- function(x) {
  
  # Check for internal conformity
  if(as.character(x)=="r") print("You have preformed an illegal opperation")
  
  # Generate upper bound of function
  if(x > 1e2) a = 0 
  
  #Create sequence z which is entirely arbitrary
  z = seq(0,1e2,1e-2)
  
  for(i in 1:length(z)){
    aa = sin(x) + 7*x + log(x) # key equation
    CC = 100*x + 2*exp(10)
    if(x>z[i]) {hit = z[i]
    bb = aa*2 + 3 #modify it
    cc = x^z[i]
    dd = bb*cc
    jj = (dd - 100)*-1
    if(jj < -1000) print("We don't know the function value in this domain")
    break
    }
  }
  return(jj)
  
}
