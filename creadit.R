xdata=read.csv("creditdata.csv",head=T,stringsAsFactors=F)

xdata=creditdata

n = nrow(xdata)
#------------------------------1)logit---------------------------------
trdata = xdata[1:(n/2),]
tsdata = xdata[(n/2+1):n,]

reg1 = glm(default~.,data = trdata,family = binomial(link = "logit"))

reg2 = glm(default~age+gender+marriage1+marriage2+counttime+consume12+consume1,
           data = trdata,family = binomial(link = "logit"))


predi1 = predict(reg1,tsdata,type="response")
predi2 = predict(reg2,tsdata,type="response")

theta = sum(trdata$default)/nrow(trdata)

#用多个theta则可以搞成ROC图
ntheta=seq(min(predi1),max(predi1),(max(predi1)-min(predi1))/99)
ntheta0=quantile(predi1,seq(0.02,0.98,0.02))


#检测哪个样本比较好
y = tsdata$default
y_hat1 = (predi1>theta)
y_hat2=(predi2>theta)
#值越大越好
cpnp = function(y,y_hat)
{
  #
  alpha=NA
  for(i in 1:length(y))
  {
    alpha = sum(y==0 & y_hat==1)/sum(y==0)
    beta = sum(y==1 & y_hat==0)/sum(y==1)
  }
    NP = (1-alpha)/beta
  return(data.frame(NP,alpha,beta))
}

#-------------------------------2)KNN----------------------------------
library(class)
pred.knn=knn(trdata,tsdata,cl=trdata$default,k=1)
np.knn=cpnp(y,pred.knn)

#-------------------------3)Support vector machine---------------------
library(e1071)
reg.svm = svm(default~.,trdata)
pre.svm= predict(reg.svm,tsdata)

NP.svm=cpnp(y,pre.svm>theta)

#-------------------------4)Neural networks-----------------------------
library(nnet)
 

#--------------------------5)decision tree------------------------
library(rpart)

