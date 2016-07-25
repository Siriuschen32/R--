####################################
R软件3程序(仅供参考)
####################################
#1.R中的颜色及点的类型
u=1:25
plot(u,pch=u,col=u,cex=3)

#2.正态态分布的密度随方差的变化
x=seq(-5,5,by=0.1)
plot(x,dnorm(x,0,1),type="l",ylim=c(0,0.8),ylab="density")
lines(x,dnorm(x,0,0.5),type="l",col="blue")
lines(x,dnorm(x,0,sqrt(1.25)),col="red")
legend(2,0.8,legend=c("N(0,1)","N(0,0.25)","N(0,1.25)"),
       col=c("black","blue","red"),lty=c(1,1,1),
       ncol=1,bty="n")
title("Normal density")

#3.正态分布的密度随均值的变化
x=seq(-5,5,by=0.1)
plot(x,dnorm(x,0,1),type="l",ylab="density")
lines(x,dnorm(x,1,1),type="l",col="blue")
lines(x,dnorm(x,-1,1),col="red")
legend(3,0.4,legend=c("N(0,1)","N(1,1)","N(-1,1)"),
       col=c("black","blue","red"),lty=c(1,1,1),
       ncol=1,bty="n")
title("Normal density")

#4.直方图与核密度估计
set.seed(123)
x=rnorm(1000)
hist(x,freq=F,main="")
lines(density(x))
w=seq(-3,3,by=0.1)
lines(w,dnorm(w),col=2)
lines(w,dnorm(w,mean(x),sd(x)),col="blue")
legend(1,0.4,legend=c("kenel density","N(0,1)","N(mean(x),var(x))"),
       col=c("black","red","blue"),lty=c(1,1,1))

#5.卡方分布
x=seq(0,10,by=0.1)
plot(x,dchisq(x,1),type="l",ylim=c(0,0.6),ylab="density",xlab="")
lines(x,dchisq(x,3),col=2)
lines(x,dchisq(x,5),col=3)
lines(x,dchisq(x,7),col=4)
legend("top",legend=c("df=1","df=3","df=5","df=7"),
       col=c(1,2,3,4),lty=1,ncol=2)
title("Chis square distributions",cex.main=0.8)

#6.二项分布与泊松分布
par(mfrow=c(1,2))
n=20;p=0.2
k=0:n
plot(k,dbinom(k,n,p),type="h",main="Binomial distribution B(20,0.2)",cex.main=0.8)
plot(k,dpois(k,4),type="h",main="Poisson distribution P(4)",cex.main=0.8)
dev.off()

#7.大数定律
#泊松
n=1000;lambda=4
x=rpois(n,lambda)
plot(cumsum(x)/(1:n),ylim=c(2,6),type="l",ylab="mean of samples",xlab="n",
     main="Poisson distribution with lambda=4")
abline(h=lambda,lty=2,col=2)
#二项
n=1000;m=20;p=0.4
x=rbinom(n,m,p)
plot(cumsum(x)/(1:n),ylim=c(6,10),type="l",ylab="mean of samples",xlab="n",
     main="Binomial distribution with m=20, p=0.4")
abline(h=m*p,lty=2,col=2)

#均匀
n=1000;a=2;b=4
x=runif(n,a,b)
plot(cumsum(x)/(1:n),ylim=c(1,5),type="l",ylab="mean of samples",xlab="n",
     main="Uniform distribution with a=2, b=4")
abline(h=(a+b)/2,lty=2,col=2)
##################################################################







