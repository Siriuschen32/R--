####################################
#R���5����(�����ο�)
####################################
#1.������̬���巽����֪ʱ�Ծ�ֵ�ļ���Ĺ�Ч����
#��������
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
#2. ������̬���巽��δ֪ʱ�Ծ�ֵ�ļ���Ĺ�Ч����
#(1)
mu1=seq(0,2,0.1)
delt=(mu1-mu0)/(sd(x)/sqrt(n))
power=pt(qt(alpha,n-1)-delt,n-1,delt) #deltΪt�ֲ��ķ����Ĳ���
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
#3. һԪ���Իع�ģ�͵�ͳ���ƶ�

#��������
load("RABE5.Rdata")
P054
attach(P054)

#(1) ɢ��ͼ
plot(Daily,Sunday)

��ͼ��ʾ�����нϺõ����Թ�ϵ

#(2) ���ع飬��ӻع�ֱ�ߣ���Ԥ��

plot(Daily,Sunday)
lm.sol=lm(Sunday~Daily)
abline(lm.sol)
predict(lm.sol)

#(3) ����������

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


#(4) �������
summary(lm.sol)

�����ʾ�Լ��� H_0: beta_1=0,~~H_1: beta_1������ 0�� ����ͳ������
�۲�ֵΪ18.935��ԶԶ���� t_{1-alpha/2}(32)=2.037�� ����� pֵС�� 10^{-16}������������˵�����շ�������ƽ�շ�����֮�������������Թ�ϵ��

##(5) ����

�� summary �Ľ����֪��R^2=0.9181�������շ������ı仯������ƽ�շ��������͵ı���Ϊ 0.9181��

##(6) Daily=500 ʱ�����շ���������������


new=data.frame(Daily=500)
predict(lm.sol,new,interval="confidence",level=0.95)


##(7) Daily=500 ʱ�����շ�������Ԥ������


new=data.frame(Daily=500)
predict(lm.sol,new,interval="prediction",level=0.95)


��Ԥ�������� (6) �е������������,���䳤��Ҫ��ܶ�.

#(8) Daily=2000 ʱ�����շ�������Ԥ������

new=data.frame(Daily=2000)
predict(lm.sol,new,interval="prediction",level=0.95)

��Ԥ��������(7)�е�Ԥ��������ȣ�Ҫ��ܶ࣬����(7)�еľ�ȷ��








