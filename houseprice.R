#library packages
library(fpp2)
library(forecast)
library(TSEntropies)
library(ggplot2)
library(nonlinearTseries)
library(TSclust)

#import dataset
data=tenyearclean
data=read.csv('tenyearclean.csv')
ts_data=ts(data[-1],start=c(2008,1),frequency = 12)

#compare all the data
autoplot(ts_data,facets=T)+ggtitle("Comparison of time series of different property sales")

#doing cluster
dissimilarity=diss(ts_data,METHOD="COR")
hc.dpred <- hclust(dissimilarity)
plot(hc.dpred)

#so from the cluster we can see that unit 1 and unit 2 are similar,
#house3, house4, house5 are similar
#house1, house2 are similar
#we can choose unit 1, house 3 house 1 and house 5 as representative 
#general trend of the four time series
unit1=ts(data$unit1,start=c(2008,1),frequency = 12)
house3=ts(data$house3,start=c(2008,1),frequency = 12)
house2=ts(data$house2,start=c(2008,1),frequency = 12)
house5=ts(data$house5,start=c(2008,1),frequency = 12)

#check if there is outliers
tsoutliers(unit1)
tsoutliers(house2)
tsoutliers(house3)
tsoutliers(house5)
house2=tsclean(house2,replace.missing = F,lambda = NULL)
house5=tsclean(house5,replace.missing = F,lambda = NULL)

repre_data=cbind(unit1,house2,house3,house5)

#check the outliers
#general plot of trend 
autoplot(repre_data)+ggtitle('the general trend plot of four category data')

autoplot(repre_data,facets=T)+geom_smooth()+
  ggtitle("Comparison of time series of different property sales")


#---------------------------unit1------------------------------#
#general trend
autoplot(unit1)+ggtitle('general trend when unit with one bedroom')
#seasonality
#unit1
ggseasonplot(unit1, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
  ggtitle("Seasonality for unit with 1 bedroom")

unit_1=data$unit1
Time.Stamp=seq(1,132,1)
ggplot(unit_1,aes(x = Time.Stamp, y = 1)) +
  geom_tile(aes(fill = unit1)) +
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red", midpoint=28) + ggtitle("Seasonality through heatmap for unit with 1 bedrooms")+
  
cla_unit1=decompose(unit1) 
autoplot(cla_unit1)
unit1_trend=var(cla_unit1$trend,na.rm = TRUE)
unit1_random=var(cla_unit1$random,na.rm = TRUE)

FT_unit1=1-(unit1_random/var((cla_unit1$trend+unit1_random),na.rm = TRUE))
FS_unit1=1-(unit1_random/var((cla_unit1$seasonal+unit1_random),na.rm = TRUE))

#sample entropy
SampEn(unit1)

#split train and test 
#we do think we need transformation
#reason 1 stablize the variance
#reason 2 keep prediction over 0
#we try to use more observations to train the model so we only use one year
#as test set 
unit1=log(unit1)
unit1_train=window(unit1,end=end(unit1)-c(1,0))

autoplot(unit1_train)

#nonlinearty test
#p-value is big 
#we don't need nnar model here 
nonlinearityTest(unit1_train)

#best ets for unit 1
#best ets for unit 1 is ann which make sense 

best.ets.unit1=ets(unit1_train)
best.ets.unit1


#auto arima
#best arima for unit 1 is arima(1,0,2) but the ma2 not so significant 
#so we try to force arima(1,0,1) here 
best.arima.unit1=auto.arima(unit1_train,trace=T,stepwise = F)
best.arima.unit1

arima.unit1.101=arima(unit1_train,order=c(1,0,1))
arima.unit1.101

#we check the accuracy of three model 
accuracy(forecast(best.ets.unit1),unit1)
accuracy(forecast(best.arima.unit1),unit1)
accuracy(forecast(arima.unit1.101),unit1)

#check residual
#all the residuals are independent 
checkresiduals(best.ets.unit1)
checkresiduals(best.arima.unit1)
checkresiduals(arima.unit1.101)

#forecast
#we do find arima.unit1.101 have smaller interval 
forecast(best.ets.unit1)
forecast(best.arima.unit1)
forecast(arima.unit1.101)

#rolling window 
#compare two arima model to choose the best one 
f.ets.ann<-function(x,h){
  forecast(ets(x,model = "ANN"),h=h)
}

f.ets.arima102<-function(x,h){
  forecast(Arima(x,order =  c(1,0,2)),h=h)
}

f.ets.arima101<-function(x,h){
  forecast(Arima(x,order =  c(1,0,1)),h=h)
}


d=unit1_train
rollCVannerrors=tsCV(d,f.ets.ann,h=1)
rollCVbestarimaerrors=tsCV(d,f.ets.arima102,h=1)
rollCVariarima101=tsCV(d,f.ets.arima101,h=1)

plot(seq(1,length(rollCVannerrors),1),rollCVannerrors,"p",pch=19,col='blue',cex=0.6,main="Rolling window CV on unit1",xlab="Size of training set",ylab="Rolling window errors")
points(seq(1,length(rollCVannerrors),1),rollCVbestarimaerrors,col="red",pch=19,cex=0.6)
points(seq(1,length(rollCVannerrors),1),rollCVariarima101,col="green",pch=19,cex=0.6)
legend('bottomright',c("BestETS","BestARIMA",'arima101'),col=c('blue','red','green'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


MSE.ann.unit1=mean(rollCVannerrors^2,na.rm = T)
MSE.best.arima.unit1=mean(rollCVbestarimaerrors,na.rm = T)
MSE.arima101.unit1=mean(rollCVariarima101,na.rm = T)

#retrospective
#retrospective analysis

tim.ser=unit1_train

e.ann.unit1=rep(0,length(tim.ser)-2)
e.arima101.unit1=rep(0,length(tim.ser)-2)


for (s in 20:(length(tim.ser)-1))
{
  data=tim.ser[1:s]
  p=length(forecast(ets(data),h=(length(tim.ser)-s))$mean)
  f.ann=forecast(ets(data,model='ANN'),h=(length(tim.ser)-s))$mean[p]
  e.ann.unit1[s]=abs(tim.ser[length(tim.ser)]-f.ann)
  
  f.arima101=forecast(Arima(data,c(1,0,1)),h=(length(tim.ser)-s))$mean[p]
  e.arima101.unit1[s]=abs(tim.ser[length(tim.ser)]-f.arima101)
  
}

e.ann.unit1
e.arima101.unit1

plot(seq(1,length(e.ann.unit1),1),e.ann.unit1,"p",pch=19,col='darkred',cex=0.6,main="Retrospective accuracy analysis",xlab="Size of training set",ylab="Retrospective absolure errors")
points(seq(1,length(e.ann.unit1),1),e.arima101.unit1,col="lightblue",pch=19,cex=0.6)

legend('topright',c("BestETS","ARIMA101"),col=c('darkred','lightblue'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


#bagging

bm.unit1=baggedModel(unit1_train,bootstrapped_series = bld.mbb.bootstrap(unit1_train,15,block_size = NULL))
accuracy(forecast(bm.unit1))

bootstrapped_versions=bld.mbb.bootstrap(unit1_train,15)
boot.ts=ts(as.data.frame(bootstrapped_versions),start = c(2008,1),frequency = 12)
autoplot(unit1_train) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(unit1_train, colour=FALSE)+  ylab("Bootstrapped_versions")+guides(colour="none")+
  ggtitle("15 bootstrapped versions of the unit1 train")

forecast(bm.unit1)


#-----------------------house2---------------------#
#general trend 
autoplot(house2)
#seasonality
ggseasonplot(house2, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
  ggtitle("Seasonality for house with 2 bedrooms")

house_2=data[3]
Time.Stamp=seq(1,132,1)
ggplot(data[3],aes(x = Time.Stamp, y = 1)) +
  geom_tile(aes(fill = house1)) +
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red", midpoint=28) + ggtitle("Seasonality through heatmap for house with 2 bedrooms")+
  ylab("") + scale_y_discrete(expand=c(0,0))

cla_house2=decompose(house2) 
autoplot(cla_house2)
house2_trend=var(cla_house2$trend,na.rm = TRUE)
house2_random=var(cla_house2$random,na.rm = TRUE)

FT_house2=1-(house2_random/var((cla_house2$trend+house2_random),na.rm = TRUE))
FS_house2=1-(house2_random/var((cla_house2$seasonal+house2_random),na.rm = TRUE))
#entropy
SampEn(house2)

#split train and test 
house2=log(house2)
autoplot(house2)

house2_train=window(house2,end=end(house2)-c(1,0))
autoplot(house2_train)

#non linearity test
nonlinearityTest(house2)

#best ets
#best ets model of house 1 is ann
best.ets.house2=ets(house2_train)
best.ets.house2


#best arima
#best arima model for house 1 is arima(1,0,0)
best.arima.house2=auto.arima(house2_train,stepwise=F,trace=T,seasonal = FALSE)
best.arima.house2

#accuracy
accuracy(forecast(best.ets.house2),house2)
accuracy(forecast(best.arima.house2),house2)

#check residual
checkresiduals(best.ets.house2)
checkresiduals(best.arima.house2)

#forecast
forecast(best.ets.house2)
forecast(best.arima.house2)


#rolling window 
#compare two arima model to choose the best one 
f.ets.ann<-function(x,h){
  forecast(ets(x,model = "ANN"),h=h)
}

f.ets.arima100<-function(x,h){
  forecast(Arima(x,order =  c(0,0,0)),h=h)
}


d=house2_train
rollCVannerrors=tsCV(d,f.ets.ann,h=1)
rollCVbestarimaerrors=tsCV(d,f.ets.arima100,h=1)

plot(seq(1,length(rollCVannerrors),1),rollCVannerrors,"p",pch=19,col='darkblue',cex=0.6,main="Rolling window CV on house2",xlab="Size of training set",ylab="Rolling window errors")
points(seq(1,length(rollCVannerrors),1),rollCVbestarimaerrors,col="darkred",pch=19,cex=0.6)
legend('bottomright',c("BestETS","BestARIMA"),col=c('darkblue','darkred'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


MSE.ann.house2=mean(rollCVannerrors^2,na.rm = T)
MSE.best.arima.house2=mean(rollCVbestarimaerrors,na.rm = T)


#retrospective
#retrospective analysis

tim.ser=house2_train

e.ann.house1=rep(0,length(tim.ser)-2)
e.arima100.house1=rep(0,length(tim.ser)-2)


for (s in 20:(length(tim.ser)-1))
{
  data=tim.ser[1:s]
  p=length(forecast(ets(data),h=(length(tim.ser)-s))$mean)
  f.ann=forecast(ets(data,model='ANN'),h=(length(tim.ser)-s))$mean[p]
  e.ann.house1[s]=abs(tim.ser[length(tim.ser)]-f.ann)
  
  f.arima100=forecast(Arima(data,c(0,0,0)),h=(length(tim.ser)-s))$mean[p]
  e.arima100.house1[s]=abs(tim.ser[length(tim.ser)]-f.arima100)
  
}

e.ann.house1
e.arima100.house1

plot(seq(1,length(e.ann.house1),1),e.ann.house1,"p",pch=19,col='darkred',cex=0.6,main="Retrospective accuracy analysis",xlab="Size of training set",ylab="Retrospective absolure errors")
points(seq(1,length(e.ann.house1),1),e.arima100.house1,col="darkblue",pch=19,cex=0.6)

legend('topright',c("BestETS","ARIMA000"),col=c('darkred','darkblue'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


#bagging

bm.house2=baggedModel(house2_train,bootstrapped_series = bld.mbb.bootstrap(house2_train,15,block_size = NULL))
accuracy(forecast(bm.house2))

bootstrapped_versions=bld.mbb.bootstrap(house2_train,15)
boot.ts=ts(as.data.frame(bootstrapped_versions),start = c(2008,1),frequency = 12)
autoplot(house2_train) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(house2_train, colour=FALSE)+  ylab("Bootstrapped_versions")+guides(colour="none")+
  ggtitle("15 bootstrapped versions of the house2 train")

forecast(bm.house2)



#----------------------------------house3-----------------------------------------#
#entropy
SampEn(house3)
#nonlineartest
nonlinearityTest(house3)

#trend
autoplot(house3)
#seasonality
ggseasonplot(house3, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
  ggtitle("Seasonality for house with 3 bedrooms")

house_3=data[4]
Time.Stamp=seq(1,132,1)
ggplot(house_3,aes(x = Time.Stamp, y = 1)) +
  geom_tile(aes(fill = house3)) +
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red", midpoint=28) + ggtitle("Seasonality through heatmap for house with three bedrooms")+
  ylab("") + scale_y_discrete(expand=c(0,0))

#trend and seasonality
#house3
cla_house3=decompose(house3) 
autoplot(cla_house3)
house3_trend=var(cla_house3$trend,na.rm = TRUE)
house3_random=var(cla_house3$random,na.rm = TRUE)

FT_house3=1-(house3_random/var((cla_house3$trend+house3_random),na.rm = TRUE))
FS_house3=1-(house3_random/var((cla_house3$seasonal+house3_random),na.rm = TRUE))

#split train and test 
house3=log(house3)
house3_train=window(house3,end=end(house3)-c(1,0))
autoplot(house3_train)

#best ets
#best ets is ann which doeen't make sense since we have strong trend 
#which may due to the outlier on 2008 
best.ets.house3=ets(house3_train)
best.ets.house3
#if we only use time start from 2009 

house3_train_2009=window(house3,start=2009,end=2017)
autoplot(house3_train_2009)
#the trend be more clear and we do get a aan model 
#which fits logical 
ets.2009.house3=ets(house3_train_2009)
ets.2009.house3

#auto arima
best.arima.house3=auto.arima(house3_train_2009,trace = T,stepwise = F)
best.arima.house3


#accuracy
accuracy(forecast(best.ets.house3),house3)
accuracy(forecast(best.arima.house3),house3)

#check residual
checkresiduals(best.ets.house3)
checkresiduals(best.arima.house3)

#forecast
forecast(best.ets.house3)
forecast(best.arima.house3)

#rolling window 
#compare two arima model to choose the best one 
f.ets.ann<-function(x,h){
  forecast(ets(x,model = "AAN",damped = T),h=h)
}

f.ets.arima015<-function(x,h){
  forecast(Arima(x,order =  c(0,1,5)),h=h)
}


d=house3_train_2009
rollCVannerrors=tsCV(d,f.ets.ann,h=1)
rollCVbestarimaerrors=tsCV(d,f.ets.arima015,h=1)

plot(seq(1,length(rollCVannerrors),1),rollCVannerrors,"p",pch=19,col='darkblue',cex=0.6,main="Rolling window CV on house3",xlab="Size of training set",ylab="Rolling window errors")
points(seq(1,length(rollCVannerrors),1),rollCVbestarimaerrors,col="darkred",pch=19,cex=0.6)
legend('bottomright',c("BestETS","BestARIMA"),col=c('darkblue','darkred'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


MSE.ann.house3=mean(rollCVannerrors^2,na.rm = T)
MSE.best.arima.house3=mean(rollCVbestarimaerrors,na.rm = T)


#retrospective
#retrospective analysis

tim.ser=house3_train_2009

e.ann.house3=rep(0,length(tim.ser)-2)
e.arima015.house3=rep(0,length(tim.ser)-2)


for (s in 20:(length(tim.ser)-1))
{
  data=tim.ser[1:s]
  p=length(forecast(ets(data),h=(length(tim.ser)-s))$mean)
  f.ann=forecast(ets(data,model='AAN',damped = T),h=(length(tim.ser)-s))$mean[p]
  e.ann.house3[s]=abs(tim.ser[length(tim.ser)]-f.ann)
  
  f.arima015=forecast(Arima(data,c(0,1,5)),h=(length(tim.ser)-s))$mean[p]
  e.arima015.house3[s]=abs(tim.ser[length(tim.ser)]-f.arima015)
  
}

e.ann.house3
e.arima015.house3

plot(seq(1,length(e.ann.house3),1),e.ann.house3,"p",pch=19,col='darkred',cex=0.6,main="Retrospective accuracy analysis",xlab="Size of training set",ylab="Retrospective absolure errors")
points(seq(1,length(e.ann.house3),1),e.arima015.house3,col="darkblue",pch=19,cex=0.6)

legend('topright',c("BestETS","ARIMA015"),col=c('darkred','darkblue'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


#bagging

bm.house3=baggedModel(house3_train_2009,bootstrapped_series = bld.mbb.bootstrap(house3_train_2009,15,block_size = NULL))
accuracy(forecast(bm.house3))

bootstrapped_versions=bld.mbb.bootstrap(house3_train_2009,15)
boot.ts=ts(as.data.frame(bootstrapped_versions),start = c(2009,1),frequency = 12)
autoplot(house3_train_2009) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(house3_train_2009, colour=FALSE)+  ylab("Bootstrapped_versions")+guides(colour="none")+
  ggtitle("15 bootstrapped versions of the house3 train")

forecast(bm.house3)





#---------------------house 5------------------------------#
#general trend
autoplot(house5)

#seasonality
ggseasonplot(house5, labels=FALSE, continuous = TRUE, polar=TRUE)+
  ggtitle("Seasonality of 5 BR House")

#entropy
SampEn(house5)
#outliers

#nonlinearity test 
nonlinearityTest(house5)

cla_house5=decompose(house5) 
autoplot(cla_house5)
house5_trend=var(cla_house5$trend,na.rm = TRUE)
house5_random=var(cla_house5$random,na.rm = TRUE)

FT_house5=1-(house5_random/var((cla_house5$trend+house5_random),na.rm = TRUE))
FS_house5=1-(house5_random/var((cla_house5$seasonal+house5_random),na.rm = TRUE))

#split train and test 
house5=log(house5)
autoplot(house5)

house5_train=window(house5,end=end(house5)-c(1,0))
autoplot(house5_train)


#best ets
#best ets is ann which doeen't make sense since we have strong trend 
#which may due to the outlier on 2008 
best.ets.house5=ets(house5_train)
best.ets.house5
#if we only use time start from 2009 
house5_train_2009=window(house5,start=2009,end=2017)
autoplot(house5_train_2009)

#the trend be more clear and we do get a aan model 
#which fits logical 
ets.2009.house5=ets(house5_train_2009)
ets.2009.house5

#auto arima
best.arima.house5=auto.arima(house5_train_2009,trace = T,stepwise = F)
best.arima.house5


#accuracy
accuracy(forecast(best.ets.house5),house5_train)
accuracy(forecast(ets.2009.house5),house5_train)
accuracy(forecast(best.arima.house5),house5_train)

#check residual
checkresiduals(best.ets.house5)
checkresiduals(ets.2009.house5)
checkresiduals(best.arima.house5)

#forecast
forecast(best.ets.house5)
forecast(best.arima.house5)
forecast(ets.2009.house5)


#rolling window 
#compare two arima model to choose the best one 
f.ets.aan<-function(x,h){
  forecast(ets(x,model = "AAN",damped = F),h=h)
}

f.ets.arima011<-function(x,h){
  forecast(Arima(x,order =  c(0,1,1)),h=h)
}


d=house5_train_2009
rollCVannerrors=tsCV(d,f.ets.ann,h=1)
rollCVbestarimaerrors=tsCV(d,f.ets.arima011,h=1)

plot(seq(1,length(rollCVannerrors),1),rollCVannerrors,"p",pch=19,col='darkblue',cex=0.6,main="Rolling window CV on house5",xlab="Size of training set",ylab="Rolling window errors")
points(seq(1,length(rollCVannerrors),1),rollCVbestarimaerrors,col="darkred",pch=19,cex=0.6)
legend('bottomright',c("BestETS","BestARIMA"),col=c('darkblue','darkred'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


MSE.ann.house5=mean(rollCVannerrors^2,na.rm = T)
MSE.best.arima.house5=mean(rollCVbestarimaerrors,na.rm = T)


#retrospective
#retrospective analysis

tim.ser=house5_train_2009

e.ann.house5=rep(0,length(tim.ser)-2)
e.arima011.house5=rep(0,length(tim.ser)-2)


for (s in 20:(length(tim.ser)-1))
{
  data=tim.ser[1:s]
  p=length(forecast(ets(data),h=(length(tim.ser)-s))$mean)
  f.ann=forecast(ets(data,model='AAN',damped = F),h=(length(tim.ser)-s))$mean[p]
  e.ann.house5[s]=abs(tim.ser[length(tim.ser)]-f.ann)
  
  f.arima011=forecast(Arima(data,c(0,1,1)),h=(length(tim.ser)-s))$mean[p]
  e.arima011.house5[s]=abs(tim.ser[length(tim.ser)]-f.arima011)
  
}

e.ann.house5
e.arima011.house5

plot(seq(1,length(e.ann.house5),1),e.ann.house3,"p",pch=19,col='darkred',cex=0.6,main="Retrospective accuracy analysis",xlab="Size of training set",ylab="Retrospective absolure errors")
points(seq(1,length(e.ann.house5),1),e.arima011.house5,col="darkblue",pch=19,cex=0.6)

legend('topright',c("BestETS","ARIMA011"),col=c('darkred','darkblue'),lty=rep(2,3),x.intersp=0.3,y.intersp=0.3,cex=0.8)


#bagging

bm.house5=baggedModel(house5_train_2009,bootstrapped_series = bld.mbb.bootstrap(house5_train_2009,15,block_size = NULL))
accuracy(forecast(bm.house5))

bootstrapped_versions=bld.mbb.bootstrap(house5_train_2009,15)
boot.ts=ts(as.data.frame(bootstrapped_versions),start = c(2009,1),frequency = 12)
autoplot(house5_train_2009) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(house5_train_2009, colour=FALSE)+  ylab("Bootstrapped_versions")+guides(colour="none")+
  ggtitle("15 bootstrapped versions of the house5")

forecast(bm.house5)


