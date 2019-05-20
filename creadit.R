setwd("C:/Users/Administrator/Desktop/credit")

xdata=read.csv('creditdata.csv',head=T,stringsAsFactors=F)

n=nrow(xdata)

trdata=xdata[1:(n/2),]
tsdata=xdata[(n/2+1):n,]

reg1=glm(default~.,data=trdata,family=binomial(link = "logit"))

reg2=glm(default~age+gender+marriage1+marriage2+counttime+consume12+consume1,
         data=trdata,family=
           binomial(link = "logit"))

pred1=predict(reg1,tsdata,type='response')

pred2=predict(reg2,tsdata,type='response')

theta=sum(trdata$default)/nrow(trdata)

y=tsdata$default

y_hat1=(pred1>theta)
  y_hat2=(pred2>theta)

cpNP=function(y,y_hat){
  
  return(NP)
}

