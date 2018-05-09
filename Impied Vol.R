
library(rJava)
library(xlsxjars)
library(xlsx)
Af = function(x){
  if(x<0){
    A = (1/2)-(1/2)*sqrt(1-exp(-(2*(x^2))/pi))
  }
  if(x>0){
    A = (1/2)-(1/2)*sqrt(1-exp(-(2*(x^2))/pi))
  }
  return(A)
}


BlackScholes_Calls<-function(PVF,disc,K,sigma,time){
  
  d1 = (log(PVF)-log(K*disc))/(sigma*sqrt(time))+(sigma*sqrt(time))/2
  d2 = d1 - (sigma*sqrt(time))
  
  price = PVF*pnorm(d1)-K*disc*pnorm(d2)
  
  return(price)
}
BlackScholes_Calls_approx<-function(PVF,disc,K,sigma,time){
  
  d1 = (log(PVF)-log(K*disc))/(sigma*sqrt(time))+(sigma*sqrt(time))/2
  d2 = d1 - (sigma*sqrt(time))
  
  price = PVF*Af(d1)-K*disc*Af(d2)
  
  return(price)
}

BlackScholes_Puts<-function(PVF,disc,K,sigma,time){
  
  d1 = (log(PVF)-log(K*disc))/(sigma*sqrt(time))+(sigma*sqrt(time))/2
  d2 = d1 - (sigma*sqrt(time))
  price =  K*disc*pnorm(-d2)-PVF*pnorm(-d1)
  return(price)
}
BlackScholes_Puts_approx<-function(PVF,disc,K,sigma,time){
  
  d1 = (log(PVF)-log(K*disc))/(sigma*sqrt(time))+(sigma*sqrt(time))/2
  d2 = d1 - (sigma*sqrt(time))
  price =  K*disc*Af(-d2)-PVF*Af(-d1)
  return(price)
}
Vega<-function(PVF,disc,K,sigma,time){
  
  d1 = log(PVF/(K*disc))/(sigma*sqrt(time))+(sigma*sqrt(time))/2
  
  vega = PVF*sqrt(time/(2*pi))*exp(-(d1^2)/2)
  return(vega)
}

file = "C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/S&P500_ETF_Option_0917.xlsx"

data  = read.xlsx(file,1)
data = data[-c(1,2),]
Strikes = matrix(data = data$Calls, nrow(data),1)
Strikes = data.matrix(as.double(Strikes))
options(digits = 10)

askCalls = data.matrix(as.double(matrix(data = data$NA..2,nrow(data),1)))
bidCalls = data.matrix(as.double(matrix(data = data$NA..1,nrow(data),1)))

midCalls = data.matrix(askCalls +bidCalls)/2
volume_of_calls = data.matrix(as.double(matrix(data = data$NA..3,nrow(data),1)))

askPuts = data.matrix(as.double(matrix(data = data$NA..6,nrow(data),1)))
bidPuts = data.matrix(as.double(matrix(data = data$NA..5,nrow(data),1)))

midPuts = data.matrix(askPuts+bidPuts)/2
volume_of_puts =  data.matrix(as.double(matrix(data = data$NA..7,nrow(data),1)))


c1 = matrix(data =1, nrow(midCalls),1)
A = matrix(c(c1, -Strikes[,]),nrow(Strikes),2)
impliedVol_calls = matrix(data = 0 , nrow(midPuts),1)
impliedVol_puts = matrix(data = 0 , nrow(midPuts),1)
PVF_disc = solve(t(A)%*%A,t(A)%*%(midCalls-midPuts))
time = 139/252
k = 2381


for(i in 1:nrow(midCalls)) {
  xStart = .25
  tol = .00000001
 # tol = format(tol, scientific = FALSE)
  xnew = xStart
  xold = xStart -1 

  
  while(abs(xnew-xold) > tol){
    xold = xnew
    xnew = xold - (BlackScholes_Calls(PVF_disc[1,],PVF_disc[2,], Strikes[i,],xold,time)-midCalls[i,])/Vega(PVF_disc[1,],PVF_disc[2,], Strikes[i,],xold,time)
   
  
  }
    impliedVol_calls[i,] = xnew
}

for(i in 1:nrow(midCalls)) {
  xStart = .25
  tol = .0000001
  # tol = format(tol, scientific = FALSE)
  xnew = xStart
  xold = xStart -1 
  
  
  while(abs(xnew-xold) > tol){
    xold = xnew
    xnew = xold - (BlackScholes_Puts(PVF_disc[1,],PVF_disc[2,], Strikes[i,],xold,time)-midPuts[i,])/Vega(PVF_disc[1,],PVF_disc[2,], Strikes[i,],xold,time)
    
    
  }
  impliedVol_puts[i,] = xnew
}
 test = format(abs(impliedVol_calls - impliedVol_puts),scientific = FALSE)
 sigma_c = matrix(data = 0 , nrow(midPuts),1)
 sigma_p = matrix(data = 0 , nrow(midPuts),1)
 
 for(i in 1:nrow(midCalls)){
   
   r = PVF_disc[2,]
   r = log(PVF_disc[2,])*-252/139
   y = log(PVF_disc[1,]/(Strikes[i,]*PVF_disc[2,]))
   
   alpha_c = midCalls[i,]/(Strikes[i,]*exp(-(r)*time) )
   Rc = 2*alpha_c-exp(y)+1 
   
   A = (exp((1-(2/pi))*y)-exp(-(1-(2/pi))*y))^2
   Bc = 4*(exp((2/pi)*y)+exp((-2/pi)*y)) - 2*exp(-y)*(exp((1-(2/pi))*y)+exp(-(1-(2/pi))*y))*(exp(2*y)+1 -Rc^2)
   Cc = exp(-2*y)*(Rc^2 - (exp(y)-1)^2)*((exp(y)+1)^2-Rc^2) 
   
   beta = (2*Cc)/(Bc+sqrt(Bc^2+4*A*Cc))
   gamma = -(pi/2)*log(beta)
 
   if(y>=0){
     C_0 = Strikes[i, ]*exp(-r*time)*((exp(y))*A*(sqrt(2*y))-(1/2))
     if(midCalls[i,]<= C_0 ){
       sigma_c[i,] = (1/sqrt(time))*((sqrt(gamma+y))-(sqrt(gamma-y)))
     }
     if(midCalls[i,]> C_0 ){
       sigma_c[i,] = (1/sqrt(time))*((sqrt(gamma+y))+sqrt((gamma-y)))
     }
   }
   if(y<0){
     C_0 = Strikes[i, ]*exp(-r*time)*((exp(y)/2)-A*(-sqrt(-2*y)))
     if(midCalls[i,]<= C_0 ){
       sigma_c[i,] = (1/sqrt(time))*(-sqrt(gamma+y)+sqrt(gamma-y))
     }
     if(midCalls[i,]> C_0 ){
       sigma_c[i,] = (1/sqrt(time))*(sqrt(gamma+y)+sqrt(gamma-y))
     }
   }
   
   
 }
 
 
 
 
 
 
 for(i in 1:nrow(midPuts)){
   
   
   y = log(PVF_disc[1,]/(Strikes[i,]*PVF_disc[2,]))
   
   alpha_p = midPuts[i,]/(Strikes[i]*exp(-r*time))
   Rp = 2*alpha_p +exp(y)-1
   
   A = (exp((1-(2/pi))*y)-exp(-(1-(2/pi))*y))^2
   Bp = 4*(exp((2/pi)*y)+exp((-2/pi)*y)) - 2*exp(-y)*(exp((1-(2/pi))*y)+exp(-(1-(2/pi))*y))*(exp(2*y)+1 -Rp^2)
   Cp = exp(-2*y)*(Rp^2 - (exp(y)-1)^2)*((exp(y)+1)^2-Rp^2) 
   beta = 2*Cp/(Bp+sqrt(Bp^2+4*A*Cp))
   gamma = -(pi/2)*log(beta)
   
   if(y>=0){
     P_0 = Strikes[i, ]*exp(-(r*time))*((1/2)-(exp(y)*A*(-sqrt(2*y))))
     if(midPuts[i,]<= P_0 ){
       sigma_p[i,] = (1/sqrt(time))*(sqrt(gamma+y)-sqrt(gamma-y))
     }
     if(midPuts[i,]> P_0 ){
       sigma_p[i,] = (1/sqrt(time))*(sqrt(gamma+y)+sqrt(gamma-y))
     }
   }
   if(y<0){
     P_0 = Strikes[i, ]*exp(-r*time)*(A*(sqrt((-2*y))-(exp(y)/2)))
     if(midPuts[i,]<= P_0 ){
       sigma_p[i,] = (1/sqrt(time))*(-sqrt(gamma+y)+sqrt(gamma-y))
     }
     if(midPuts[i,]> P_0 ){
       sigma_p[i,] = (1/sqrt(time))*(sqrt(gamma+y)+sqrt(gamma-y))
     }
   }
 }
 
 Errors_c = matrix(data = 0 ,20,1)
 Errors_p = matrix(data = 0 ,20,1)
 

   Errors_c=(impliedVol_calls - sigma_c)/(impliedVol_calls)
   Errors_p= (impliedVol_puts - sigma_p)/(impliedVol_puts)
  write.xlsx(PVF_disc,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.xlsx", sheetName  = "PVF ")
 write.xlsx(impliedVol_calls ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.xlsx", sheetName  = "Calls", append = TRUE)
  write.xlsx(impliedVol_puts ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.xlsx", sheetName  = "puts", append = TRUE)
  write.xlsx(test ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.xlsx", sheetName  = "different", append = TRUE)
  write.xlsx(sigma_c ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.xlsx", sheetName  = "calls1", append = TRUE)
  write.xlsx(Errors_c ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.1.xlsx", sheetName  = "errorc", append = TRUE)
  write.xlsx(Errors_p ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question2.xlsx", sheetName  = "errorp", append = TRUE)
  
  
  
  
  
   
   
 