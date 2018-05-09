
library(rJava)
library(xlsxjars)
library(xlsx)

#file = "C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/financials2012.xlsx"
#data = read.xlsx(file,1)
JPM = matrix(c(39.0,36.8,36.9,36.7,36.6,35.8,36.6,33.7),8,1)
GS = matrix(c(116.30,105.7,104.5,103.2,102.6,100.5,101.2,93.8),8,1)
MS = matrix(c(17.1,15.0,14.6,14.5,14.6,13.8,13.5,12.7),8,1)
BAC = matrix(c(8.8,8.0,8.2,8.0,7.7,7.4,7.3,7.1),8,1)
#data = data[complete.cases(data),]
#data = data.matrix(data)
NUMBER = 4
data2  = matrix(c(JPM,GS,MS,BAC),nrow(JPM),NUMBER)
#data2 = matrix(data =NA,41,10)

#revsre the order
#for(i in 1:nrow(data)){
 # data2[i,] = data[nrow(data)+1-i,]
#}


St2 = data2[-1,]
St1 = data2[-nrow(data2),]

weeklyPercent = St2/St1 -1
#weeklyPercent = weeklyPercent[,-1]

y = matrix(data = weeklyPercent[,1],nrow(weeklyPercent),1)
c = matrix(data =1, nrow(weeklyPercent),1)
c1 = matrix(data = weeklyPercent[,1],nrow(weeklyPercent),1)
c2 = matrix(data = weeklyPercent[,2],nrow(weeklyPercent),1)
c3 = matrix(data = weeklyPercent[,3],nrow(weeklyPercent) ,1)
c4 = matrix(data = weeklyPercent[,4],nrow(weeklyPercent),1)
#c5 = matrix(data = weeklyPercent[,5],nrow(weeklyPercent) ,1)
#c6 = matrix(data = weeklyPercent[,6],nrow(weeklyPercent) ,1)
#c7 = matrix(data = weeklyPercent[,7],nrow(weeklyPercent) ,1)
#c8 = matrix(data = weeklyPercent[,8],nrow(weeklyPercent) ,1)
#c9 = matrix(data = weeklyPercent[,9], nrow(weeklyPercent) ,1)
A = matrix(c(c,c2,c3,c4),nrow(weeklyPercent),4)
trana = t(A)
Z = trana%*%A
z = trana%*%y
x = solve(Z,z) 

#JPM = matrix(data  =0,40,1)
cov = cov(weeklyPercent)
cor = cor(weeklyPercent)
#yi =  
d  = (y-A%*%x)
error = sqrt(t(y)%*%y -2*t(x)%*%t(A)%*%y +t(A%*%x)%*%A%*%x)



JPM1 = A%*%x



A = matrix(c(c1,c2,c3,c4),40,4)
trana = t(A)
Z = trana%*%A
z = trana%*%y
x = solve(Z,z) 
JPM = matrix(data  =0,40,1)
 

JPM = 1*x[1,] + c2*x[2,] + x[3,]*c3 + c4*x[4,]
  abs(JPM-weeklyPercent[,1])/abs(JPM)
  error2 = sqrt(t(y)%*%y -2*t(x)%*%t(A)%*%y +t(A%*%x)%*%A%*%x)


data2 =data2[,-1]
y = matrix(data = data2[,1],41,1)
c1 = matrix(data =1, 41,1)
c2 = matrix(data = data2[,2],41,1)
c3 = matrix(data = data2[,3],41 ,1)
c4 = matrix(data = data2[,4],41,1)
c5 = matrix(data = data2[,5],41,1)
c6 = matrix(data = data2[,6],41 ,1)
c7 = matrix(data = data2[,7],41 ,1)
c8 = matrix(data = data2[,8],41 ,1)
c9 = matrix(data = data2[,9], 41 ,1)
A = matrix(c(c1,c2,c3,c4,c5,c6,c7,c8,c9),41,9)
trana = t(A)
Z = trana%*%A
z = trana%*%y
x = solve(Z,z) 

JPMprice = 1*x[1,] + c2*x[2,] + x[3,]*c3 + c4*x[4,]
error3  = sqrt(t(y)%*%y -2*t(x)%*%t(A)%*%y +t(A%*%x)%*%A%*%x)

write.xlsx(weeklyPercent ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "percent")
write.xlsx(JPM1 ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "JPM1", append = TRUE)
write.xlsx(JPM ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "JPM", append = TRUE)
write.xlsx(JPMprice ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "JPM Price", append = TRUE)


write.xlsx(error ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "error1", append = TRUE)
write.xlsx(error2 ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "error2", append = TRUE)
write.xlsx(error3 ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 7/Question3.xlsx", sheetName  = "error3", append = TRUE)

