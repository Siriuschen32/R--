##################################
#R软件作业2参考程序
######################################
#1.读写数据文件
#(1)
write.table(cars,"carsdata.txt")
read.table("carsdata.txt")
#(2)
Air=AirPassengers;Titan=Titanic
save(Air,Titan,file="yourname.Rdata")
#(3)
rm(Air,Titan)
Air;Titan
#(4)
load("yourname.Rdata")
Air;Titan
#######################################
#2.回归分析
attach(cars)
x=cbind(1,speed);y=dist
n=length(y);p=dim(x)[2]
reg.analysis=function(x,y,alpha){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #估计
  beta.hat=solve(t(x)%*%x,t(x)%*%y) #最小二乘估计
  beta.hat=as.vector(beta.hat)  #转化为向量
  sigma2.hat=sum((y-x%*%beta.hat)^2)/(n-p) #sigma^2的最小二乘估计
  sigma.hat=sqrt(sigma2.hat)
  cov.matrix=sigma2.hat*solve(t(x)%*%x)  #估计的协方差矩阵
  beta.sd=sqrt(diag(cov.matrix))   #估计的beta.hat的标准差
  beta.sd=as.vector(beta.sd)  #转化为向量
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #置信区间
  beta.left=beta.hat-beta.sd*qt(1-alpha/2,n-p)  #区间左端点
  beta.right=beta.hat+beta.sd*qt(1-alpha/2,n-p) #区间右端点
  beta.interval=matrix(c(beta.left,beta.right),ncol=2,
                       dimnames=list(c("beta0","beta1"),c("left","right")))  #95%置信区间
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #残差
  y.hat=x%*%beta.hat
  res=y-y.hat                     #残差
  H=x%*%solve(t(x)%*%x)%*%t(x)    #帽子矩阵
  h=diag(H)
  s.res=res/(sigma.hat*sqrt(1-h)) #标准化残差
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #检验
  beta1.hat=beta.hat[2]
  sxx=sum((x[,2]-mean(x[,2]))^2)
  test.stat=beta1.hat*sqrt(sxx)/sigma.hat  #检验统计量的观测值
  p_value=2*(1-pt(abs(test.stat),n-p))     #检验的p_value
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #输出结果
  list(beta.hat=beta.hat,
       beta.interval=beta.interval,
       res=res,
       s.res=s.res,
       test.stat=test.stat,
       p_value=p_value
       )
}
reg.analysis(x,y,alpha=0.05)
detach(cars) #解除链接
##############################################
#3.利用蒲丰投针试验估计圆周率
l=1  #针长
d=2  #两平行线间距（l<d)
n=1000000
x=runif(n,0,pi)  
y=runif(n,0,d/2)
freq=mean(y<=l/2*sin(x)) #概率的估计
pi.hat=2*l/(d*freq)
pi.hat
###############################################
#4
den=function(x) 1/sqrt(2*pi)*exp(-x^2/2) #定义被积函数，即dnorm
a=1;b=5
m=100000
#(1)黎曼近似
d=(b-a)/m  #区间长度
x=seq(a-d,b+d,by=d)  #取点
I.hat1=sum(den(x)*d)
I.hat1
#(2)蒲丰投针
x=runif(m,a,b)
y=runif(m,0,den(a))
freq=mean(y<=den(x))
I.hat2=(b-a)*den(a)*freq
I.hat2
#(3)大数律
x=runif(m,a,b)
I.hat3=(b-a)*mean(den(x)) 
I.hat3
#(4)用pnorm
I=pnorm(b)-pnorm(a)
I
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#注：亦可将上述程序写成函数，参数为积分上下限、被积函数、模拟次数
#例：蒲丰投针
int=function(a,b,fun,m){
  x=runif(m,a,b)
  y=runif(m,0,den(a))
  freq=mean(y<=den(x))
  I.hat2=(b-a)*den(a)*freq
  I.hat2
}
int(a,b,den,m)
##################################################
#5.模拟马氏链
set.seed(12)
m=100
x=numeric(m)
x[1]=0
for (i in 2:m){
  if(x[i-1]==0)
    x[i]=rbinom(1,1,0.06)
  else
    x[i]=rbinom(1,1,0.9)
}
plot(1:m,x)
lines(1:m,x)
#######################################################
#6.模拟抛硬币试验
m=10000
n=1:m
h=rbinom(m,1,1/2) #or h=sample(c(0,1),m,replace=T)
y=cumsum(h)/n
plot(n,y,type="l",ylim=c(0,1))
abline(h=0.5,lty=2,col=2)
######################################################
#7.解一维非线性方程的牛顿算法
Newtons=function(fun,x,ep=1e-5,it_max=100){
  index=0;k=1
  while(k<=it_max){
     x1=x;obj=fun(x)
     x=x-obj$f/obj$J
     norm=abs(x-x1)
     if(norm<ep){
        index=1;break
     }
     k=k+1
  }
  obj=fun(x)
  list(root=x,it=k,index=index,Funval=obj$f)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~
#与方程有关的函数
funs=function(x){
   f=x^3-x-1
   J=3*x^2-1
   list(f=f,J=J)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#求解方程
Newtons(funs,0)
####################################################




#######################################################
