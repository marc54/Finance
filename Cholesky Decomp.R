library(rJava)
library(xlsxjars)
library(xlsx)
library(Matrix)
library(matlib)
A = matrix(c(41,24,24,136),2,2,byrow = TRUE)
eigens = eigen(A)
Q = eigens$vectors
sqrtL =  diag(sqrt(eigens$values))
B = Q%*%sqrtL%*%t(Q)
test = B%*%B

write.xlsx(B, file = "C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 5/homework5_Question15.xlsx", sheetName = "Question 15p2" ,append = TRUE )  


U = chol(A)
t(U)%*%U
test2 = Q%*%solve(sqrtL)%*%t(Q)%*%B
write.xlsx(U, file = "C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 5/homework5_Question15.xlsx", sheetName = "Question 15p3" ,append = TRUE )
