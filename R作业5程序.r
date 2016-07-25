####################################
#R软件5程序(仅供参考)
####################################
#1.单个正态总体方差已知时对均值的检验的功效函数
#生成数据
mu0=2
sigma=2
n=100
x=rnorm(n,mu0,sigma)
alpha=0.05
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(1)
mu1=seq(0,2,0.1)
delt=(mu1-mu0)/(sigma/sqrt(n))
power=pnorm(qnorm(alpha)-delt) 
plot(mu1,power,type="b",xlab="mu")
abline(h=alpha,col=2,lty=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(2)
mu1=seq(2,4,0.1)
delt=(mu1-mu0)/(sigma/sqrt(n))
power=1-pnorm(qnorm(1-alpha)-delt)
plot(mu1,power,type="b",xlab="mu")
abline(h=alpha,col=2,lty=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(3)
mu1=seq(0,4,0.1)
delt=(mu1-mu0)/(sigma/sqrt(n))
power=1-pnorm(qnorm(1-alpha/2)-delt)+
  pnorm(qnorm(alpha/2)-delt)
plot(mu1,power,type="b",xlab="mu")
abline(h=alpha,col=2,lty=2)
#################################################
#2. 单个正态总体方差未知时对均值的检验的功效函数
#(1)
mu1=seq(0,2,0.1)
delt=(mu1-mu0)/(sd(x)/sqrt(n))
power=pt(qt(alpha,n-1)-delt,n-1,delt) #delt为t分布的非中心参数
plot(mu1,power,type="b",xlab="mu")
abline(h=alpha,col=2,lty=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(2)
mu1=seq(2,4,0.1)
delt=(mu1-mu0)/(sd(x)/sqrt(n))
power=1-pt(qt(1-alpha,n-1)-delt,n-1,delt)
plot(mu1,power,type="b",xlab="mu")
abline(h=alpha,col=2,lty=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(3)
mu1=seq(0,4,0.1)
delt=(mu1-mu0)/(sd(x)/sqrt(n))
power=1-pt(qt(1-alpha/2,n-1)-delt,n-1,delt)+
  pt(qt(alpha/2,n-1)-delt,n-1,delt)
plot(mu1,power,type="b",xlab="mu")
abline(h=alpha,col=2,lty=2)
#################################################
#3. 一元线性回归模型的统计推断

#加载数据
load("RABE5.Rdata")
P054
attach(P054)

#(1) 散点图
plot(Daily,Sunday)

该图显示二者有较好的线性关系

#(2) 作回归，添加回归直线，作预测

plot(Daily,Sunday)
lm.sol=lm(Sunday~Daily)
abline(lm.sol)
predict(lm.sol)

#(3) 求置信区间

beta.int=function(fm,alpha=0.05){
  A=summary(fm)$coefficients
  df=fm$df.residual
  left=A[,1]-A[,2]*qt(1-alpha/2,df)
  right=A[,1]+A[,2]*qt(1-alpha/2,df)
  rowname=dimnames(A)[[1]]
  colname=c("Estimate","Left","Right")
  matrix(c(A[,1],left,right),ncol=3,dimnames=list(rowname,colname))
}

beta.int(lm.sol)


#(4) 假设检验
summary(lm.sol)

结果显示对假设 H_0: beta_1=0,~~H_1: beta_1不等于 0， 检验统计量的
观测值为18.935，远远大于 t_{1-alpha/2}(32)=2.037， 检验的 p值小于 10^{-16}，极度显著。说明周日发行量与平日发行量之间有显著的线性关系。

##(5) 解释

由 summary 的结果可知，R^2=0.9181，故周日发行量的变化中能由平日发行量解释的比例为 0.9181。

##(6) Daily=500 时，周日发行量的置信区间


new=data.frame(Daily=500)
predict(lm.sol,new,interval="confidence",level=0.95)


##(7) Daily=500 时，周日发行量的预测区间


new=data.frame(Daily=500)
predict(lm.sol,new,interval="prediction",level=0.95)


该预测区间与 (6) 中的置信区间相比,区间长度要大很多.

#(8) Daily=2000 时，周日发行量的预测区间

new=data.frame(Daily=2000)
predict(lm.sol,new,interval="prediction",level=0.95)

该预测区间与(7)中的预测区间相比，要大很多，不如(7)中的精确。








