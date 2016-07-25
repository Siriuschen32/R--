################################################
#函数的定义如下
#name=function(arg_1,arg_2,...) expression
#arg_1,arg_2:函数的参数
#expression:表达式，若很复杂，则用{}括起来
#放在程序最后的信息是函数的返回值，返回值可以是向量、矩阵、列表或数据框
############################################
#例子：计算一维正态分布的密度
normdensity=function(x,mu,sigma){
  normpdf=(2*pi*sigma^2)^(-1/2)*exp(-(x-mu)^2/(2*sigma^2))
  return(normpdf)
}
x=-3:3
normdensity(x,mu=0,sigma=1)
normdensity(x,0,1)
dnorm(x,0,1)                   
############################################
#例子：计算二维正态分布的密度
normdensity=function(x,mu,sigma){
  normpdf=(2*pi)^(-p/2)*det(Sigma)^(-1/2)*exp(-1/2*(x-mu)%*%solve(Sigma)%*%(x-mu))
  return(as.numeric(normpdf))
}
x=c(0,0);mu=c(0,0);Sigma=diag(c(1,1));p=2
normdensity(x,mu,sigma)
         
############################################
#例子：计算单样本的描述性统计量
data_outline=function(x){
  n=length(x)   
  m=mean(x)
  v=var(x)
  s=sd(x)
  me=median(x)
  cv=100*s/m        #变异系数
  css=sum((x-m)^2)  #样本校正平方和
  uss=sum(x^2)      
  R=max(x)-min(x)   #极差
  R1=quantile(x,0.75)-quantile(x,0.25)  #四分位极差
  sm=s/sqrt(n)      #标准误
  g1=n/((n-1)*(n-2))*sum((x-m)^3)/s^3   #偏度
  g2=((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4  
      -(3*(n-1)^2)/((n-2)*(n-3)))       #峰度
  data.frame(N=n,Mean=m,Var=v,std_dev=s,Median=me,std_mean=sm,
             CV=cv,CSS=css,USS=uss,R=R,R1=R1,Skewness=g1,Kurtosis=g2,row.names=1)
}
x=rnorm(100)
data_outline(x)
#############################################################
#例2.5计算两个样本的T统计量
twosamp=function(y1,y2){
    n1=length(y1);n2=length(y2)
    yb1=mean(y1);yb2=mean(y2)
    s1=var(y1);s2=var(y2)
    s=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
    (yb1-yb2)/sqrt(s*(1/n1+1/n2))
}
#or
twosamp=function(y1,y2){
    n1=length(y1);n2=length(y2)
    yb1=mean(y1);yb2=mean(y2)
    s1=var(y1);s2=var(y2);
    s=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
    t=(yb1-yb2)/sqrt(s*(1/n1+1/n2))
    return(t)
}
#输入两个样本数据
x=c(79.98,80.04,80.02,80.04,80.03,80.03,80.04,79.97,80.05,80.03,
80.02,80.00,80.02)
y=c(80.02,79.94,79.98,79.97,79.97,80.03,79.95,79.97)
#计算两个样本的T统计量
twosamp(x,y)
########################################################
#单个正态总体均值的置信区间
interval_estimate1=function(x,sigma=-1,alpha=.05){
  n=length(x);xb=mean(x)
  if(sigma>=0){    #方差sigma已知时
    tmp=sigma/sqrt(n)*qnorm(1-alpha/2);df=n
  }
  else{          #方差sigma未知时
    tmp=sd(x)/sqrt(n)*qt(1-alpha/2,n-1);df=n-1
  }
  data.frame(mean=xb,df=df,a=xb-tmp,b=xb+tmp)
}
#例4.14
x=c(14.6,15.1,14.9,14.8,15.2,15.1);interval_estimate1(x,sigma=0.2)

#例4.15
x=c(10.1,10,9.8,10.5,9.7,10.1,9.9,10.2,10.3,9.9);interval_estimate1(x)
############################################################################
#两个正态总体均值差的置信区间
interval_estimate2=function(x,y,sigma=c(-1,-1),var.equal=FALSE,alpha=.05){
  n1=length(x);n2=length(y)
  xb=mean(x);yb=mean(y)
  if(all(sigma>=0)){# 两方差sigma1和sigma2已知
    tmp=qnorm(1-alpha/2)*sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
    df=n1+n2
  }
  else{#两方差sigma1和sigma2未知
    if(var.equal==TRUE){#两方差sigma1和sigma2未知，且相等
      Sw=((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2)
      tmp=sqrt(Sw*(1/n1+1/n2))*qt(1-alpha/2,n1+n2-2)
      df=n1+n2-2
    }
    else{#两方差sigma1和sigma2未知，且不等
      S1=var(x);S2=var(y)
      nu=(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
      tmp=qt(1-alpha/2,nu)*sqrt(S1/n1+S2/n2)
      df=nu
    }
  }
  data.frame(mean=xb-yb,df=df,a=xb-yb-tmp,b=xb-yb+tmp)
}


#例4.17 两总体方差已知时，均值差的置信区间
x=rnorm(100,5.32,2.18);y=rnorm(100,5.32,1.76)
interval_estimate2(x,y,sigma=c(2.18,1.76))

#例4.18 两总体方差未知时，均值差的置信区间
x=rnorm(12,501.1,2.4);y=rnorm(17,499.7,4.7)
interval_estimate2(x,y,var.equal=T) #方差相等时
interval_estimate2(x,y) #方差不等时     

#利用R函数      
t.test(x,y) #不等方差时，同interval_estimate2(x,y) 
t.test(x,y,var.equal=T) #等方差时，同interval_estimate2(x,y,var.equal=T)
