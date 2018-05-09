# cubic spline interpolation

library(rJava)
library(xlsxjars)
library(xlsx)

 ZeroRates<- function(DisFactor , times){
  zerorates = -(1/times)*log(DisFactor)
  return(zerorates)
}
 
values = matrix(c(.01,.9930,.9835,.9750,.9625),5,1)
nodes = matrix(c(0,6/12,12/12,18/12,24/12),5,1)

zRates = matrix(data = 0,nrow(values),1)
zRates[1,] = .01

for (i in 3:(nrow(nodes)+1)){
  zRates[i-1,] = ZeroRates(values[i-1,], nodes[i-1,])
}

Z = matrix(data = 0 , nrow(values)-2,1)
for(i in 1:(nrow(values)-1)){
 Z[i-1, ] = 6*((zRates[i+1,]-zRates[i,])/(nodes[i+1,]-nodes[i,])-((zRates[i,]-zRates[i-1,])/(nodes[i]- nodes[i-1])))
}

M = matrix(data = 0 , nrow(values)-2,nrow(values)-2)
 for (i in 2:nrow(values)-1){
   M[i-1,i-1] = 2*(nodes[i+1,]-nodes[i-1])
 }
for(i in 2:nrow(values)-2){
  M[i-1,i] = nodes[i+1,]-nodes[i,]
}
for(i in 3:nrow(values)-1){
  M[i-1,i-2] = nodes[i,]- nodes[i-1,]
}

x = solve(M,Z)
w = matrix(data = 0, nrow(values),1)
for (i in 1:nrow(x)){
  w[i+1]= x[i]
}


a = matrix(data= 0 , nrow(values)-1,1)
b = matrix(data= 0 , nrow(values)-1,1)
c = matrix(data= 0 , nrow(values)-1,1)
d = matrix(data= 0 , nrow(values)-1,1)
q = matrix(data= 0 , nrow(values)-1,1)
r = matrix(data= 0 , nrow(values)-1,1)

for(i in 2:nrow(values)){

  c[i-1,]= (w[i-1,]*nodes[i,]-w[i,]*nodes[i-1,])/(2*(nodes[i,]-nodes[i-1,]))
  d[i-1,]= (w[i,]-w[i-1,])/(6*(nodes[i,]-nodes[i-1,]))
}

for(i in 2:nrow(values)){
 q[i-1,] = zRates[i-1,]-(c[i-1,]*(nodes[i-1,])^2)- d[i-1,]*(nodes[i-1,]^3)
 r[i-1,] = zRates[i,]-c[i-1,]*nodes[i,]^2 - d[i-1,]*nodes[i,]^3
}

for(i in 2:nrow(values)){
  a[i-1,] = ((q[i-1,]*nodes[i,]-r[i-1,]*nodes[i-1,])/(nodes[i,] - nodes[i-1,]))
  b[i-1,] = (r[i-1,]-q[i-1,])/(nodes[i,]-nodes[i-1,])
}

date  = matrix(c(4/12,10/12,16/12,22/12),4,1)

z2 = matrix(data = 0,1,nrow(date))

for( j in 1:nrow(date)){  
  num = 1
  for( i in 2:nrow(values)){
   if(date[j,]>nodes[i,]){
    num = num+1
   }
  if(date[j,]<=nodes[i,]){
    z2[,j] = a[num,]+ b[num,]*date[j,] +c[num,]*date[j,]^2 +d[num,]*date[j,]^3
  }
  }
}
#print(num)


#100 + R/N*exp(-(date)*zero)
bond = 103.5 *exp(-(date[4,])*z2[,4])
for(i in 1:3){
  bond =  bond + 3.5*exp(-nodes[i+1,]*zRates[i+1,])
}
test = ZeroRates()
write.xlsx(M ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "M", append = TRUE)
write.xlsx(Z ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "z", append = TRUE)
write.xlsx(zRates ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "zero", append = TRUE)
write.xlsx(w ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "W", append = TRUE)
write.xlsx(a ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "a", append = TRUE)
write.xlsx(b ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "b", append = TRUE)
write.xlsx(c ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "c", append = TRUE)
write.xlsx(d ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question14Output.xlsx", sheetName  = "d", append = TRUE)

