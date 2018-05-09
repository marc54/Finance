library(matlib)
library(Matrix)
year2 = matrix(c(1.69 , 1.81, 1.81, 1.79, 1.79, 1.83, 1.81, 1.81, 1.83, 1.81, 1.82, 1.82, 1.80, 1.78 , 1.79),15,1)  
year3 = matrix(c(2.58, 2.71, 2.72, 2.78, 2.77, 2.75, 2.71, 2.72, 2.76, 2.73, 2.75, 2.75, 2.73, 2.71, 2.71),15,1)
year5 = matrix(c(3.57, 3.69, 3.70,3.77, 3.77, 3.73, 3.72,3.74,3.77,3.75,3.77,3.76,3.75,3.72,3.71),15,1)
year10 = matrix(c(4.63,4.73,4.74,4.81,4.80,4.79,4.76,4.77, 4.80, 4.77, 4.80,4.80,4.78,4.73,4.73),15,1)

c1 = matrix(c(data = 1 ), 15, 1)
A = matrix(c(c1,year2,year5,year10), 15, 4 )
transpa = t(A)%*%A

z= t(A)%*%year3

x = solve(transpa,z)
y = year3
T3 = matrix(data=0,15,1)
for(i in 1:nrow(year3)){
 T3[i,] = x[1,]+year2[i,]*x[2,]+year5[i,]*x[3,]+year10[i,]*x[4,] 
}
 
error =sqrt(t(y)%*%y -2*t(x)%*%t(A)%*%y +t(A%*%x)%*%A%*%x)
error2= (norm(y - T3,"f"))
T3int = matrix(data =0,15,1)

for(i in 1:nrow(T3)){
  
  T3int[i,] = (2/3)*year2[i,] + (1/3)*year5[i,]
  
}
error = 0 
error  = norm(y-T3int,"f")

error3 = norm(y - T3int2,"f")
 

write.xlsx(T3,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question1.xlsx", sheetName  = "LR Approx ")
write.xlsx(T3int ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question1.xlsx", sheetName  = "Spline Aprrox", append = TRUE)
write.xlsx(T3int2 ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question1.xlsx", sheetName  = "CSpline Aprrox", append = TRUE)






