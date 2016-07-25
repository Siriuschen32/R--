#2.7读写数据文件
#########################################
#2.7.1读纯文本文件
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.read.table()函数读取表格文件和read.delim()函数
rt=read.table("houses.txt")  #第一列为数据号
is.data.frame(rt)
rt=read.table("house.txt",header=T)  #第一列无数据号
rt=read.table("house.txt") 
#扩展名为.dat的文本文件
rt=read.table("houses.dat") 
read.table("ORF_names.dat")

#读取网站数据
website="http://projecteuler.net/project/base_exp.txt"
read.table(website,sep=",")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.scan()函数
scan("weight.data.txt")
scan("weight.dat")
scan("h_w.data.txt")
scan("h_w.dat")

#数据中有不同属性,可以指定变量的类型,生成列表
scan("h_w.dat",list(height=0,weight=0))
scan("h_w.dat",what=list(height=0,weight=0))
inp=scan("h_w.dat",list(height="",weight=""));inp
is.list(inp)
#尝试
inp=scan("h_w.dat",list(height=0,weight=0,score=0));inp
inp=scan("h_w.dat",list(height=0,weight=0,score=0,money=0));inp

#例子
read.table("new.txt")
a=scan("new.txt",list(Name="",Sex="",Age=0,Height=0,Weight=0))
as.data.frame(a)

#转化为矩阵
x=matrix(scan("weight.dat",0),nrow=3,byrow=T)
x=matrix(scan("weight.dat"),nrow=3,byrow=T)

x=matrix(scan("h_w.dat",0),nrow=3,byrow=T)
x=matrix(scan("h_w.dat"),nrow=3,byrow=T)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################
#2.7.2读其他各式的数据文件
#1.读SPSS,SAS,S-PLUS,Stata数据文件
library(foreign)
rs=read.spss("educ_scores.sav")   #结果为列表
rs=read.spss("educ_scores.sav",to.data.frame=T) #结果为数据框
rs=read.spss("accidents.sav",to.data.frame=T)
rs=read.spss("births.sav",to.data.frame=T)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.读Excel数据文件
read.table("edut.xls",head=T)  #不能直接读取

#转化为文本文件再读取
read.table("educ.txt",head=T)
read.delim("educ.txt",head=T) 

#转化为CSV文件再读 
read.csv("educ.csv")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.7.3链接嵌入的数据库
#base中的数据
data()
Orange
#或者
data(Orange);Orange

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#查看其他软件包中的数据
#方法一
library()  #查看R有哪些安装好的软件包
data(package="cluster") #浏览软件包cluster中有哪些数据集
data(agriculture,package="cluster") #加载软件包cluster中的数据集argriculture
agriculture 
data(animals,package="cluster");animals

~~~~~~~~~~~~~~~~~~~~~~~~~~
#方法二
library("lattice")  #加载软件包lattice
data(package="lattice")
environmental
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.7.4写数据文件
#1.write()函数
A=matrix(c(1:20),nrow=10)
write(A,file="yang.txt",ncolumns=2)
write(A,file="wang.txt",ncolumns=2,sep="\t")

#2.write.table()和write.csv()
df=data.frame(
   Name=c("Alice","Becka","James","Jeffrey","John"),
   Sex=c("F","F","M","M","M"),
   Age=c(13,13,12,13,12),
   Height=c(56.5,65.3,57.3,62.5,59.0),
   Weight=c(84.0,98.0,83.0,84.0,99.5)
   )
write.table(df,file="foo.txt")
write.table(df,file="foo.txt",sep="\t")

read.table("foo.txt")
write.csv(df,file="foo.csv")
########################################
#R格式的数据
mtcars
mtcars2=data.frame(mtcars[,c(1,4)])
save(mtcars2,file="save.Rdata")
load("save.Rdata")
#保存多个数据集
car=cars
dataf=df
save(car,dataf,file="save.Rdata")
rm(car,dataf)
load("save.Rdata")
########################################


