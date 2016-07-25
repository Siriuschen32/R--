#############################################
#R软件作业1参考解答（仅供参考）
##########################################
#1
#(1)
x=rnorm(100,2,3) 

#(2)
mean(x);var(x);sd(x);min(x);max(x);quantile(x,0.95);quantile(x,seq(0,1,0.1)) 

#(3)
hist(x)
hist(x,freq=F)  #后一图将覆盖前一图
#若要两图都显示
hist(x)
x11()
hist(x);hist(x,freq=F)
#一页多图
#排成一行
par(mfrow=c(1,2))
hist(x);hist(x,freq=F)
#关掉该作图窗口
dev.off()
#一页多图
#排成一列
par(mfrow=c(2,1))
hist(x);hist(x,freq=F)
dev.off()
#注：查看par的帮助，了解作图参数的含义
################################################
#2
#(1)
attach(cars)
plot(speed,dist)

#(2)
lm.sol=lm(dist~speed)
summary(lm.sol)
summary(lm.sol)$coefficients[,1]
#or
coefficients(lm.sol)
#or
lm.sol$coefficients
#添加估计的回归直线
abline(lm.sol)

#(3)
x=cbind(1,speed);y=dist
beta.hat=solve(t(x)%*%x,t(x)%*%y)

#(4)
n=length(y);p=dim(x)[2]
sigma2.hat=sum((y-x%*%beta.hat)^2)/(n-p)
sigma.hat=sqrt(sigma2.hat)
beta1.hat=beta.hat[2]
sxx=sum((x[,2]-mean(x[,2]))^2)
test.stat=beta1.hat*sqrt(sxx)/sigma.hat

#判断是否显著:检验统计量与临界值
alpha=0.05
critical.value=qt(1-alpha/2,n-p)  
abs(test.stat)>=critical.value
#判断是否显著:p-value与显著性水平
p_value=2*(1-pt(abs(test.stat),n-p))
p_value<alpha

#(5)
y.hat=x%*%beta.hat
res=y-y.hat
H=x%*%solve(t(x)%*%x)%*%t(x)
h=diag(H)
s.res=res/(sigma.hat*sqrt(1-h))
plot(y.hat,s.res)
abline(h=c(-2,2),col=2,lty=2)

##################################################
#3
#(1)
student=read.table("exam0203.txt",head=T)

#(2)
mean(student$Height);sum(student$Height)
#or
mean(student[,4]);sum(student[,4])

#(3)
tapply(student$Weight,student$Sex,mean)

#(4)
tapply(student$Weight,student[,2:3],mean)
####################################################
#4
#(1)
x=-10:10

#(2)
n=length(x)
y=numeric(n)
y[x>=0]=x[x>=0]^2+1
y[x<0]=sin(x[x<0])
###############################################
#5
P=matrix(c(0.1,0.2,0.3,0.4,0.4,0.1,0.2,0.3,0.3,0.4,0.1,0.2,0.2,0.3,0.4,0.1),4,4,byrow=T)

#(1)
apply(P,1,sum)

#(2) 取不同的n,可得P^n
A=P
n=2;i=1
while(i<=n-1){
  A=A%*%P
  i=i+1
}
A
#####################################################
#编写函数
matrix.power=function(P,n){
  A=P
  i=1
  while(i<=n-1){
    A=A%*%P
    i=i+1
  }
  A
}
matrix.power(P,n=13)
#尝试不同的n,当n>=13时，矩阵中的所有元素均为0.25
#########################################################
#方法二：利用矩阵对角化P=DBD^(-1),其中B为由特征值构成的对角阵,D为对应的特征向量构成的正交阵
#则P^n=DB^nD^(-1)
ev=eigen(P)
B=ev$values
D=ev$vectors
Pn=D%*%(diag(B))^3%*%solve(D)  #例：取n=3
Re(Pn)  #此处矩阵P的特征值和特征向量为复数，故取其实部
#######################################################
