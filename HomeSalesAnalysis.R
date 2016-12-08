HomeSales<-read.csv("DATA/kc_house_data.csv")
write.csv(HomeSales, "Output.csv")
                    
library(ggplot2)
library(GGally)
library(pls)
library(ggplot2)

cor(HomeSales[c(3:8)])
cor(HomeSales[c(3, 11:16)])


ggpairs(HomeSales[c(4:8,3)])
ggpairs(HomeSales[c(3,11:15)])

ggplot(HomeSales, aes(x=LogLotSqFt))+geom_histogram()
ggplot(HomeSales, aes(x=bedrooms))+geom_histogram()
ggplot(HomeSales, aes(x=bathrooms))+geom_histogram()
ggplot(HomeSales, aes(x=sqft_living))+geom_histogram()
ggplot(HomeSales, aes(x=log(sqft_living)))+geom_histogram()
ggplot(HomeSales, aes(x=scale(sqft_living)))+geom_histogram()
ggplot(HomeSales, aes(x=scale(sqft_living)))+geom_histogram()
ggplot(HomeSales, aes(x=scale(sqft_above)))+geom_histogram()



HomeSales$LogLotSqFt<-log(HomeSales$sqft_lot)
HomeSales$LogSalePrice<-log(HomeSales$price)
HomeSales$ScaledLogPrice<-scale(HomeSales$LogSalePrice, scale=TRUE, center = TRUE)
HomeSales$LogSqFtLiving<-log(HomeSales$sqft_living)
HomeSales$LogSqFtAbove<-log(HomeSales$sqft_above)
HomeSales$LogSqFtBasement<-log(HomeSales$sqft_basement+1)
HomeSales$SqRtSalesPrice<-sqrt(HomeSales$price)

PCM<-prcomp(HomeSales[c(4:5,8,11:12,15:16,18:19,22,25,26,27)], center=TRUE, scale=TRUE)
PCM
summary(PCM)
plot(PCM, type="l")

crossval(pcr_model)

traindata<-as.data.frame(predict(PCM,HomeSales[c(4:5,8,11:12,15:16,18:19,22,25,26,27)]))

model<-lm(HomeSales$SqRtSalesPrice ~ traindata$PC1 + traindata$PC2 + traindata$PC3 + traindata$PC4 + traindata$PC5 + traindata$PC6 + traindata$PC7+traindata$PC8 + traindata$PC9 + traindata$PC10)
summary(model)

model2<-lm(HomeSales$LogSalePrice ~ traindata$PC1 + traindata$PC2 + traindata$PC3 + traindata$PC4 + traindata$PC5 + traindata$PC6 + traindata$PC7+traindata$PC8 + traindata$PC9 + traindata$PC10)
summary(model2)

#remove obs 15871...33 bedrooms in a 1620 square foot house?

plot(model)
plot(model2)



PCM
summary(PCM)
plot(PCM, type="l")

hist(HomeSales$sqft_basement)


hist(log(HomeSales$price))

#Here's the PCR part - it will run dog ass slow and long

pcrdata<-HomeSales[c(4:6,8,11:16,18:19,22:23)]

pcr_model<-pcr(SqRtSalesPrice~., data = pcrdata, scale=TRUE, center=TRUE, validation="LOO")
summary(pcr_model)
pcr_model

validationplot(pcr_model)
validationplot(pcr_model, val.type = "MSEP")
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)
coefficients(pcr_model)



pcr_model$loadings
pcr_model$Xmeans

pcr_model$model


#Let's create a random sample of data to build the model, a second set for test data, perform PCA, regress, and test

#decide the size of sample data as 1/2 of the data

SampleCount<-round(length(HomeSales$id)/3.5)



#subdivide the data

TrainDataSample<-sample(nrow(HomeSales), SampleCount, replace=FALSE)

#Here's my training and regression set
TrainDataSample2<-HomeSales[TrainDataSample,]

#here's the set I'll test it against
TestData<-HomeSales[-TrainDataSample,]

PCM<-prcomp(TrainDataSample2[c(4:5,8,11:12,15,18:19,22,25,26,27)], center=TRUE, scale=TRUE)
PCM
summary(PCM)
plot(PCM, type="l")

traindata<-as.data.frame(predict(PCM,TrainDataSample2[c(4:5,8,11:12,15,18:19,22,25,26,27)]))


model<-lm(TrainDataSample2$SqRtSalesPrice ~ traindata$PC1 + traindata$PC2 + traindata$PC3 + traindata$PC4 + traindata$PC5)

summary(model)

#build matrix from traindatasample3 to test with

traindata<-as.data.frame(predict(PCM,TestData[c(4:5,8,11:12,15:16,18:19,22,25,26,27)]))

results<-predict(model,TestData, interval='predict')
results<-cbind(results, TestData$SqRtSalesPrice)
results<-as.data.frame(results)

results$Price<-(results$V4)^2
results$Predicted<-(results$fit)^2
results$difference<-(results$Price-results$Predicted)

plot(results$Predicted, results$difference)

ggplot(results, aes(x=Predicted, y=difference))+geom_point()+xlim(0,3E6)
ggplot(results, aes(x=Predicted, y=(difference/Predicted)))+geom_point()+xlim(0,1E6)
