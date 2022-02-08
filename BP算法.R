library(openxlsx)
##sigmoid激活函数
fun_stim<-function(x,fun_stim_type)
{
  if(fun_stim_type=='sigmoid')
  {
    return(1/(1+exp(x)^(-1)))
  }
  if(fun_stim_type=='relu')
  {
    return(x*(sign(x)+1)/2)
  }
}
##sigmoid激活函数的导数
fun_partial_stim<-function(x,fun_stim_type)
{
  if(fun_stim_type=='sigmoid')
  {
    return(1/(1+exp(x)^(-1))*(1-1/(1+exp(x)^(-1))))
  }
  if(fun_stim_type=='relu')
  {
    return((sign(x)+1)/2)
  }
}
##损失函数
fun_err<-function(x,y)
{
  step1<-x-y
  step2<-step1^2
  result<-step2%*%matrix(1,nrow=ncol(x))/ncol(x)
  return(list(sum(result)/nrow(x),result))
}
##损失函数的导数
fun_partial_err<-function(x,y)
{
  # x=Y[[4]]
  # y=Y_out
  step3<-(x-y)*2/nrow(x)
  result<-step3%*%matrix(1,nrow=ncol(x))/ncol(x)
  return(list(sum(result)/nrow(x),result))
}
##特殊的矩阵乘法
spe_multi0<-function(x,y)
{
  result<-matrix(0,nrow=length(x))
  for (i in c(1:length(x)))
  {
    result[i]=x[i]*y[i]
  }
  return(result)
}
##对应位置相乘的矩阵乘法
spe_multi<-function(x,y)
{
  result<-matrix(0,nrow=nrow(y),ncol=ncol(y))
  for(i in c(1:ncol(y)))
  {
    result[,i]=spe_multi0(x,y[,i])
  }
  return(result)
}
##权重、偏置初始化函数
W_B_initialization<-function(n_code)
{
  W=list()
  B=list()
  for(i in c(1:(length(n_code)-1)))
  {
    W[[i]]=matrix(runif(n_code[i+1]*n_code[i],min=-sqrt(1/n_code[i]),max=sqrt(1/n_code[i])),nrow=n_code[i+1])
    B[[i]]=matrix(runif(n_code[i+1],min=-sqrt(1/n_code[i]),max=sqrt(1/n_code[i])),nrow=n_code[i+1])
  }
  return(list(W,B))
}
##正确率计算
acc<-function(x,y)
{
  return(sum(x==y))
}
##前反馈
FP<-function(Z_input,n_code,W,B,Y_out,stim_type)
{
  t<-is(ncol(Z_input))
  if(t[1]=='NULL')
  {
    ncol_Z_input=1
  }else{
    ncol_Z_input=ncol(Z_input)
  }
  ##Z_input是样本的输入层，Y_out是对应输入的输出标签，n_code是层数，第一层是输入层，最后一层是输出层
  for(i in c(1:length(n_code)))
  {
    if(i==1)
    {
      Z=list()
      Y=list()
      Z[[1]]=Z_input
      Y[[1]]=Z[[1]]
    }else{
      Z[[i]]=W[[i-1]]%*%Y[[i-1]]+matrix(rep(B[[i-1]],ncol_Z_input),ncol=ncol_Z_input)
      Y[[i]]=fun_stim(Z[[i]],stim_type[i-1])
    }
  }
  #Y_final<-(sign(Y[[length(n_code)]]-0.5)+1)/2
  #acc_result<-sum(Y_final==Y_out)/ncol(Z_input)
  err<-fun_err(Y[[length(Y)]],Y_out)[[1]]
  #return(list(Z,Y,err,acc_result))
  return(list(Z,Y,err))
}
##梯度下降
BP<-function(Z,Y,W,B,n_code,eta,Y_out,stim_type)
{
  
  partial_total_y<-list()
  new_W<-list()
  new_B<-list()
  for(i in c(length(n_code):2))
  {
    ##偏导参数更新
    partial_total0<-matrix(0,nrow=n_code[i])
    if(i==length(n_code)){
      partial_total0<-fun_partial_err(Y[[i]],Y_out)[[2]]
      partial_total_y[[1]]<-partial_total0
    }else{
      partial_total_y[[2]]<-partial_total_y[[1]]
      partial_total0<-t(W[[i]])%*%spe_multi(partial_total_y[[2]],fun_partial_stim(Z[[i+1]],stim_type[i-1]))%*%matrix(1,nrow=ncol(Z[[i+1]]))/ncol(Z[[i+1]])
      partial_total_y[[1]]<-partial_total0
    }
    ##参数更新
    partial_W<-spe_multi(partial_total_y[[1]],fun_partial_stim(Z[[i]],stim_type[i-1]))%*%t(Y[[i-1]])/ncol(Z[[i]])
    new_W[[i-1]]<-W[[i-1]]-eta*partial_W
    partial_B<-spe_multi(partial_total_y[[1]],fun_partial_stim(Z[[i]],stim_type[i-1]))%*%matrix(1,nrow=ncol(Z[[i]]))/ncol(Z[[i]])
    new_B[[i-1]]<-B[[i-1]]-eta*partial_B
  }
  W<-new_W
  B<-new_B
  return(list(W,B))
}


#####
iris <- datasets::iris
type_flower<-levels(iris[,5])
Y_out<-matrix(0,nrow=3,ncol=nrow(iris))
for(i in c(1:nrow(iris)))
{
  Y_out[which(type_flower==as.character(iris[i,5])),i]=1
}
Z_input<-t(iris[,-5])
order=sample(1:150,150)
Z_input=Z_input[,order]
Y_out=Y_out[,order]
#区分训练集和测试集
Z_input_train<-Z_input[,1:120]
Z_input_test<-Z_input[,121:150]
Y_out_train<-Y_out[,1:120]
Y_out_test<-Y_out[121:150]
###
n_code<-c(4,8,6,4,3)
n_layers<-length(n_code)
stim_type=c('sigmoid','sigmoid','sigmoid','sigmoid')
##生成权值
W<-W_B_initialization(n_code)[[1]]
B<-W_B_initialization(n_code)[[2]]
###
true_type<-c()
for(i in c(1:ncol(Y_out)))
{
  true_type[i]<-which(Y_out[,i]==1)
}
test_type<-c()
###
n=0
batch_size=5
epoches=5000
eta=0.5
while(n<=epoches)
{
  for(i in c(1:ceiling(ncol(Z_input)/batch_size)))
  {
    Z_input0=Z_input[,((i-1)*batch_size+1):min(ncol(Z_input),i*batch_size)]
    Y_out0=Y_out[,((i-1)*batch_size+1):min(ncol(Z_input),i*batch_size)]
    step1<-FP(Z_input0,n_code,W,B,Y_out0,stim_type)
    Z=step1[[1]]
    Y=step1[[2]]
    step2<-BP(Z,Y,W,B,n_code,eta,Y_out0,stim_type)
    W=step2[[1]]
    B=step2[[2]]
  }
  n=n+1
  the_result<-FP(Z_input,n_code,W,B,Y_out,stim_type)
  for(i in c(1:ncol(the_result[[2]][[length(n_code)]])))
  {
    test_type[i]<-which(the_result[[2]][[length(n_code)]][,i]==max(the_result[[2]][[length(n_code)]][,i]))
  }
  acc=sum(true_type==test_type)/length(true_type)
  print(paste(n,'the acc is ',acc))
}
final_result<-FP(Z_input,n_code,W,B,Y_out)
##

#区分训练集和测试集
Z_input_train<-Z_input[,1:120]
Z_input_test<-Z_input[,121:150]
Y_out_train<-Y_out[,1:120]
Y_out_test<-Y_out[121:150]
#
n=0
batch_size=5
epoches=5000
eta=0.5
while(n<=epoches)
{
  for(i in c(1:ceiling(ncol(Z_input_train)/batch_size)))
  {
    Z_input0=Z_input_train[,((i-1)*batch_size+1):min(ncol(Z_input_train),i*batch_size)]
    Y_out0=Y_out_train[,((i-1)*batch_size+1):min(ncol(Z_input_train),i*batch_size)]
    step1<-FP(Z_input0,n_code,W,B,Y_out0,stim_type)
    Z=step1[[1]]
    Y=step1[[2]]
    step2<-BP(Z,Y,W,B,n_code,eta,Y_out0,stim_type)
    W=step2[[1]]
    B=step2[[2]]
  }
  n=n+1
  the_result<-FP(Z_input_test,n_code,W,B,Y_out_test,stim_type)
  for(i in c(1:ncol(the_result[[2]][[length(n_code)]])))
  {
    test_type[i]<-which(the_result[[2]][[length(n_code)]][,i]==max(the_result[[2]][[length(n_code)]][,i]))
  }
  acc=sum(true_type[121:150]==test_type)/length(test_type)
  print(paste(n,'the acc is ',acc))
}


##基本参数
n_code<-c(10,20,1)
n_layers<-length(n_code)
eta=0.5
##生成权值
W=list()
B=list()
for(i in c(1:(length(n_code)-1)))
{
  W[[i]]=matrix(runif(n_code[i+1]*n_code[i],min=-sqrt(1/n_code[i]),max=sqrt(1/n_code[i])),nrow=n_code[i+1])
  B[[i]]=matrix(runif(n_code[i+1],min=-sqrt(1/n_code[i]),max=sqrt(1/n_code[i])),nrow=n_code[i+1])
}
##网页给出的标准数据
W[[1]]=matrix(c(0.15,0.25,0.20,0.30),nrow=2)
W[[2]]=matrix(c(0.40,0.50,0.45,0.55),nrow=2)
B[[1]]=matrix(c(0.35,0.35),nrow=2)
B[[2]]=matrix(c(0.6,0.6),nrow=2)
Z_input=matrix(c(0.05,0.10,0.10,0.12,0.12,0.35,0.04,0.98),nrow=2)
Y_out=matrix(c(0.01,0.99,0.05,0.95,0.34,0.89,0.12,0.78),nrow=2)
##肺癌数据
data<-read.csv('/Users/xulongtao/Documents/论文代码整理/数据集/test_finaldata_fc.csv',header = T)
for(i in c(1:nrow(data)))
{
 if(data[i,21]==1)
 {
   data[i,21]=0
 }
 if(data[i,21]==2)
 {
   data[i,21]=1
 }
}
Z_input<-t(as.matrix(rbind(data[1:59,1:20],data[514:572,1:20])))
Y_out<-t(c(data[1:59,21],data[514:572,21]))
order=sample(1:118,118)
Z_input=Z_input[,order]
Y_out=Y_out[order]
##
n_code<-c(20,30,30,1)
n_layers<-length(n_code)
stim_type=c('sigmoid','sigmoid','sigmoid')
##生成权值
W=list()
B=list()
for(i in c(1:(length(n_code)-1)))
{
  W[[i]]=matrix(runif(n_code[i+1]*n_code[i],min=-sqrt(1/n_code[i]),max=sqrt(1/n_code[i])),nrow=n_code[i+1])
  B[[i]]=matrix(runif(n_code[i+1],min=-sqrt(1/n_code[i]),max=sqrt(1/n_code[i])),nrow=n_code[i+1])
}
###
##
n=0
batch_size=5
epoches=5000
eta=0.5
while(n<=epoches)
{
  for(i in c(1:ceiling(ncol(Z_input)/batch_size)))
  {
    Z_input0=Z_input[,((i-1)*batch_size+1):min(ncol(Z_input),i*batch_size)]
    Y_out0=Y_out[((i-1)*batch_size+1):min(ncol(Z_input),i*batch_size)]
    step1<-FP(Z_input0,n_code,W,B,Y_out0,stim_type)
    Z=step1[[1]]
    Y=step1[[2]]
    step2<-BP(Z,Y,W,B,n_code,eta,Y_out0,stim_type)
    W=step2[[1]]
    B=step2[[2]]
  }
  n=n+1
  the_result<-FP(Z_input,n_code,W,B,Y_out,stim_type)
  test_type<-(sign(the_result[[2]][[length(n_code)]]-0.5)+1)/2
  acc=sum(Y_out==as.numeric(test_type))/length(Y_out)
  print(paste(n,'the acc is ',acc))
}






  