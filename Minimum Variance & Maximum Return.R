library(Matrix)
library(matlib)
library(Matrix)


mu1 = .026
mu2 = .034
mu3 = .045
mu4 = .052
#mu5 = 0.079
rf = .0075
sigma1 = c(.04,-.015,0.015,-0.005)
sigma2 = c(-.015, .0625, -.02, -.01)
sigma3 = c(.015,-.02,.1024,.02)
sigma4 = c(-.005,-.01,.02,0.1225)
#sigma5 = c(-.01,0.005,-.005,0.025,0.1369)
cov12 =-.50
cov13 =-.15
cov23 = -.25

u = matrix(c(mu1-rf,mu2-rf,mu3 -rf,mu4-rf),4,1) 

covm = matrix(c(sigma1,sigma2,sigma3,sigma4),nrow(u),nrow(u))
#for all money invested let U = c1= matrix(data =1,nrow(u),1)
#u = c1= matrix(data =1,nrow(u),1)
x = solve(covm,u)

c1 = matrix(data =1,nrow(u),1)
w = as.numeric((1/(t(c1)%*%x)))*(x)
sigmap = sqrt(t(w)%*%covm%*%w)
return = rf + t(u)%*%w

Sharp = (return-rf)/sigmap

#minimum Variance portfolio tangency
mup = .04
wcash = as.numeric((1-(mup-rf)/(t(u)%*%w) ))
wmin = (1-wcash)*w
sigmap = sqrt(t(wmin)%*%covm%*%wmin)   

SharpMin = (mup -rf)/sigmap
#maximum return porfolio tangency
View(wcash)
sigmap = 0.28
Wmaxcash =as.numeric(1 - sigmap/sqrt(sqrt(t(w)%*%covm%*%w)))
sign = t(c1)%*%x
if (sign<0){
  Wmaxcash = -Wmaxcash
}
Wmax = (1 - Wmaxcash)*w
Wmax = data.matrix(Wmax)
return = rf + t(u)%*%Wmax
 
SharpMax = (return -rf)/sigmap


