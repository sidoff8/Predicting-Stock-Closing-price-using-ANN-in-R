install.packages("neuralnet")
install.packages("nnet")


#calling included libraries
library(quantmod)
library(dplyr)
library(forecast)
library(neuralnet)
library(nnet)
library(devtools)

getSymbols("TCS",src="yahoo", from="2018-05-05", periodicity = "daily"  )

#Storing data in data frames
x<-data.frame(TCS)
write.table(x, file = "TCS$Close.csv", row.names=F, sep = ",")

x$date<-rownames(x)
rownames(x)<-NULL
x <- subset(x, select = c(TCS.Open, TCS.High , TCS.Low, TCS.Close, TCS.Volume, TCS.Adjusted))

x2 <- subset(x, select = c(TCS.Open, TCS.High , TCS.Low, TCS.Close, TCS.Volume, TCS.Adjusted))

str(x)

#for histogram https://www.datamentor.io/r-programming/histogram/
hist(x$TCS.Close,
     main=" TCS closing prices Histogram",
     xlab="x$TCS.Close",
     col="red"
)


plot(x$TCS.Close,type = "o", col = "red", xlab = "Index", ylab = "x$TCS.Close",
     main = "TCS closing prices Line Graph")

#Shows dimension of data frame x
dim(x)

#Head Function in R: returns the first n rows of a matrix or data frame in R.Here n=2

head(x,2,range)

#Apply Function in R: returns the n output in for the given funtion
#Here apply Returns range value
apply(x,2,range)

#Here apply Returns minimum value
minValue <- apply(x,2,min)
minValue

#Here apply Returns maximum value
maxValue <- apply(x,2,max)
maxValue

#dividing training data from our x data frame
t1<- sample(1:nrow(x),150)
tr1<-x[t1,]
dim(tr1)

#dividing test data randomly from our x data frame
te1<- x[-t1,]
dim(te1)

#scale is generic function whose default method centers and/or scales the columns of a numeric matrix
#https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/scale
x<-as.data.frame(scale(x,center = minValue, scale = maxValue-minValue))
ind<-sample(1:nrow(x),150)
trainDF<-x[ind,]
testDF<-x[-ind,]
dim(trainDF)
dim(testDF)


allVars<-colnames(x)
allVars

predictorVarss<- allVars[!allVars%in%"TCS.Close"]
predictorVarss
predictorVarss<-paste(predictorVarss,collapse="+")
predictorVarss

form=as.formula(paste("TCS.Close~", predictorVarss,collapse = "+"))
neuralModel<- neuralnet(formula =form, hidden = c(4,2),linear.output=T,data=trainDF)

plot(neuralModel,col.hidden='black',col.entry = 'red',col.hidden.synapse = 'black',col.intercept = 'blue',col.entry.synapse = 'black',col.out = 'red',fontsize = 16)


predictions<-compute(neuralModel,testDF[,1:5])
str(predictions)

pred<-predictions$net.result
pred
predcsv<-data.frame(pred)
predcsv
predictions<- pred*(max(testDF$TCS.Close)-min(testDF$TCS.Close))+min(testDF$TCS.Close)
predictions

actualValues <- (testDF$TCS.Close)*(max(testDF$TCS.Close)-min(testDF$TCS.Close))
actualValues<-data.frame(actualValues)
write.table(actualValues, file = "actualValues.csv", row.names=F, sep = ",")



plot(testDF$TCS.Close,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(predictions,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Real','Predicted'),pch=18,col=c('red','blue'))

plot(testDF$TCS.Close,type = "o",col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(predictions,type = "o",col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Real','Predicted'),pch=18,col=c('red','blue'))


predictions
write.table(predictions, file = "predictions.csv", row.names=F, sep = ",")
testDF$TCS.Close
pred
final<-(testDF$TCS.Close-predictions)
result<-(te1$TCS.Close-final)



#mean square error
MeanSquareError<-sum((predictions - actualValues)^2)/nrow(testDF)
MeanSquareError=MeanSquareError*100
MeanSquareError
MeanSquareAccuracy=100-MeanSquareError
MeanSquareAccuracy
Err=sum((actualValues - predictions))/nrow(testDF)*100
Accuracy=100-Err
Accuracy

