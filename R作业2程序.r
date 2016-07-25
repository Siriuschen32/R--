##################################
#R�����ҵ2�ο�����
######################################
#1.��д�����ļ�
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
#2.�ع����
attach(cars)
x=cbind(1,speed);y=dist
n=length(y);p=dim(x)[2]
reg.analysis=function(x,y,alpha){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #����
  beta.hat=solve(t(x)%*%x,t(x)%*%y) #��С���˹���
  beta.hat=as.vector(beta.hat)  #ת��Ϊ����
  sigma2.hat=sum((y-x%*%beta.hat)^2)/(n-p) #sigma^2����С���˹���
  sigma.hat=sqrt(sigma2.hat)
  cov.matrix=sigma2.hat*solve(t(x)%*%x)  #���Ƶ�Э�������
  beta.sd=sqrt(diag(cov.matrix))   #���Ƶ�beta.hat�ı�׼��
  beta.sd=as.vector(beta.sd)  #ת��Ϊ����
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #��������
  beta.left=beta.hat-beta.sd*qt(1-alpha/2,n-p)  #������˵�
  beta.right=beta.hat+beta.sd*qt(1-alpha/2,n-p) #�����Ҷ˵�
  beta.interval=matrix(c(beta.left,beta.right),ncol=2,
                       dimnames=list(c("beta0","beta1"),c("left","right")))  #95%��������
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #�в�
  y.hat=x%*%beta.hat
  res=y-y.hat                     #�в�
  H=x%*%solve(t(x)%*%x)%*%t(x)    #ñ�Ӿ���
  h=diag(H)
  s.res=res/(sigma.hat*sqrt(1-h)) #��׼���в�
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #����
  beta1.hat=beta.hat[2]
  sxx=sum((x[,2]-mean(x[,2]))^2)
  test.stat=beta1.hat*sqrt(sxx)/sigma.hat  #����ͳ�����Ĺ۲�ֵ
  p_value=2*(1-pt(abs(test.stat),n-p))     #�����p_value
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #������
  list(beta.hat=beta.hat,
       beta.interval=beta.interval,
       res=res,
       s.res=s.res,
       test.stat=test.stat,
       p_value=p_value
       )
}
reg.analysis(x,y,alpha=0.05)
detach(cars) #�������
##############################################
#3.�����ѷ�Ͷ���������Բ����
l=1  #�볤
d=2  #��ƽ���߼�ࣨl<d)
n=1000000
x=runif(n,0,pi)  
y=runif(n,0,d/2)
freq=mean(y<=l/2*sin(x)) #���ʵĹ���
pi.hat=2*l/(d*freq)
pi.hat
###############################################
#4
den=function(x) 1/sqrt(2*pi)*exp(-x^2/2) #���屻����������dnorm
a=1;b=5
m=100000
#(1)��������
d=(b-a)/m  #���䳤��
x=seq(a-d,b+d,by=d)  #ȡ��
I.hat1=sum(den(x)*d)
I.hat1
#(2)�ѷ�Ͷ��
x=runif(m,a,b)
y=runif(m,0,den(a))
freq=mean(y<=den(x))
I.hat2=(b-a)*den(a)*freq
I.hat2
#(3)������
x=runif(m,a,b)
I.hat3=(b-a)*mean(den(x)) 
I.hat3
#(4)��pnorm
I=pnorm(b)-pnorm(a)
I
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ע����ɽ���������д�ɺ���������Ϊ���������ޡ�����������ģ�����
#�����ѷ�Ͷ��
int=function(a,b,fun,m){
  x=runif(m,a,b)
  y=runif(m,0,den(a))
  freq=mean(y<=den(x))
  I.hat2=(b-a)*den(a)*freq
  I.hat2
}
int(a,b,den,m)
##################################################
#5.ģ��������
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
#6.ģ����Ӳ������
m=10000
n=1:m
h=rbinom(m,1,1/2) #or h=sample(c(0,1),m,replace=T)
y=cumsum(h)/n
plot(n,y,type="l",ylim=c(0,1))
abline(h=0.5,lty=2,col=2)
######################################################
#7.��һά�����Է��̵�ţ���㷨
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
#�뷽���йصĺ���
funs=function(x){
   f=x^3-x-1
   J=3*x^2-1
   list(f=f,J=J)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#��ⷽ��
Newtons(funs,0)
####################################################




#######################################################
