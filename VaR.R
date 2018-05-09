 

VaR <- function(N,percent,sigma,amount){
  ValueAtRisk = sqrt(N/252)*sigma*qnorm(percent)*amount
  return (ValueAtRisk)
}

VaR2 <- function(N,oldN , percent, oldPercent, var){
  
  ValueAtRisk = sqrt(N/oldN)*(qnorm(percent)/qnorm(oldPercent))*var
  return(ValueAtRisk)
}
 A = VaR(10,.99,.25,100000000)
 B = VaR(10,.99,.30,100000000)
 C = VaR(10,.99,.32,100000000)
 
 sigma1 = c(.25^2, .25*.30*.25 ,.25*.32*-0.25)
 sigma2 = c(.25*.30*.25,.30^2,.32*.30*.50)
 sigma3 = c(.25*.32*-0.25,.32*.30*.50,.32^2)
 u = c1= matrix(data =1,3,1)
 covm = matrix(c(sigma1,sigma2,sigma3),nrow(u),nrow(u))
 
 x = solve(covm,u)
 
 c1 = matrix(data =1,nrow(u),1)
 w = as.numeric((1/(t(c1)%*%x)))*(x)
 sigmap = sqrt(t(w)%*%covm%*%w) 
 D = VaR(10,.99,sigmap,100000000) 
 vr = 10000000
 VaR2(10,2,.99,.98,vr)
 