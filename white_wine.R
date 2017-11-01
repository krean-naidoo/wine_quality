#WHO --> Client is a distributor of various selections of wine
#What --> Client needs to an effective way to assess wine quality due to high volume & cost of a somilier
#How --> Client wishes to utlize industry standard chemical atributes to predict quality 
#OBJECTIVE# By being able to predict wine quality based on chemical attributes, the client is able to enable
#consistency in rating wines, as well as creacte a efficient method to rate wines. 

#Loading white wine quality data 
data_white<-read.csv("C:/Users/Krean Naidoo/Downloads/Wine Quality/winequality-white.csv", header = TRUE, sep = ";")

#Lets start by checking data characteristics
head(data_white)
tail(data_white)
str(data_white)

#hmmm all the atributes are of class "num" except for quality, which is of class "int"
#We will coerce "quality" atribute into numeric class (even though it'll use more storage)
data_white[,12]<-sapply(data_white[,12],as.numeric)
str(data_white)

#We will work with the assumption that all our variables are ordinal and of the numeric class 

summary(data_white)
w.sum <- do.call(data.frame, 
               list(mean = apply(data_white, 2, mean),
                    sd = apply(data_white, 2, sd),
                    median = apply(data_white, 2, median),
                    min = apply(data_white, 2, min),
                    max = apply(data_white, 2, max)))
w.sum
write.csv(w.sum, "C:/Users/Krean Naidoo/Desktop/Tech Test/white_datasummary.csv")

dim(data_white)
#Wow! There's a total of 4898 different types of wine

#Are there any missing values
sum(is.na(data_white))
#Phew, no chickens flew the coop. No data is missing! 

#We will eliminate identical rows. This is because the distributor can carry the exact same wine from the 
#same winery bottled under different brands. This can cause overfitting of models that will be produced
data_white1<-unique(data_white)
dim(data_white1)
#OKay! We are now working with 3961 unique types of wines

#Lets create histograms to visualize the spread of the data
attach(data_white1)
par(mfrow=c(3,4), mar = c(2,2,1,1))
boxplot(fixed.acidity,col="blue",main = "Fixed Acidity", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(volatile.acidity,col="blue",main = "Volatile Acidity", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(citric.acid,col="blue",main = "Citric Acid", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(residual.sugar,col="blue",main = "Residual Sugar", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(chlorides,col="blue",main = "Chlorides", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(free.sulfur.dioxide,col="blue",main = "Free Sulfur Dioxide", cex.main = .9, ylab = NULL, xlab = NULL)
boxplot(total.sulfur.dioxide,col="blue",main = "Total Sulfur Dioxide", cex.main = .9, ylab = NULL, xlab = NULL)
boxplot(density,col="blue",main = "Density", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(pH,col="blue",main = "pH", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(sulphates,col="blue",main = "Sulphates", cex.main = 1, ylab = NULL, xlab = NULL)
boxplot(alcohol,col="blue",main = "Alcohol", cex.main = 1, ylab = NULL, xlab = NULL)
barplot(table(quality),col="blue",main = "Quality", cex.main = 1, ylab = NULL, xlab = NULL)
detach(data_white1)

#It was decided that even with outliers occuring in the data, given the sitution where certain extreme
#characteristics could vastly change the quality of the wine, it was decided to keep outliers within
#the data set

#Is there correlation between any of the attributes
library(corrplot)
par(mfrow=c(1,1), xpd=TRUE)
corrplot(cor(data_white1),mar =c(0,0,0,0),type = "lower")

#Time to normalize the data in order to use classification techniques! But first, lets create a copy of our data
data_white2<-data_white1
#Rescale each column(1:12 b/c 13 is char) to range between 0 and 1 (ie normalize)
data_white2[,1:12]<-apply(data_white2[,1:12], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

#Lets look at information gain using regression analysis 
wfit1<-lm(quality ~ ., data=data_white2)
summary(wfit1)

#Lets remove "density" because the estimate causes a variance in quality that is non-sensical 
wfit2<-lm(quality ~ alcohol+sulphates+pH+total.sulfur.dioxide+free.sulfur.dioxide+
            chlorides+residual.sugar+citric.acid+volatile.acidity+fixed.acidity, data=data_white2)
summary(wfit2)

#Lets remove "citric.acid" because it is not significant at a 95% CI & has little effect 
wfit3<-lm(quality ~ alcohol+sulphates+pH+total.sulfur.dioxide+free.sulfur.dioxide+
            chlorides+residual.sugar+volatile.acidity+fixed.acidity, data=data_white2)
summary(wfit3)

#Lets remove "chlorides" because it is not significant at a 95% CI now & has little effect
wfit4<-lm(quality ~ alcohol+sulphates+pH+total.sulfur.dioxide+free.sulfur.dioxide+
            residual.sugar+volatile.acidity+fixed.acidity, data=data_white2)
summary(wfit4)

#Checking percentage distribution of quality atribute 
white_prop<-prop.table(table(data_white1$quality))
white_prop
write.csv(white_prop, "C:/Users/Krean Naidoo/Desktop/Tech Test/white_prop.csv")

#To help simplify the classification model, we will assign a rating to each quality level.
#We will have to do this to our unnormalized data set then renormalize it. First, lets create a copy and
#remove the attributes we deemed to have little effect ("citric.acid","chlorides","density")
data_white3<-data_white1
data_white3<-data_white3[,-3][,-4][,-6]

#Now lets assign the rating!
data_white3$rating<-ifelse(data_white3$quality==3 | data_white3$qualit ==4 | data_white3$qualit == 5,"low",
                          (ifelse(data_white3$qualit == 6,"medium","high")))
#Lets check the class of our new rating varibale
class(data_white3$rating)
#Whoops lets switch this into a factor!
data_white3$rating<-as.factor(data_white3$rating)
#Lets check that
class(data_white3$rating)

#Lets create a copy and normalize!
data_white4<-data_white3
data_white4[,1:9]<-apply(data_white4[,1:9], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

#Divide data_white into training and testing sets
library('caret')
set.seed(3456)
w.trainIndex <- sample(1:nrow(data_white4), 0.7*nrow(data_white4))
w.train.set<-data_white4[w.trainIndex,]
w.test.set<-data_white4[-w.trainIndex,]

#Remove the Ratings & Quality column from training/ test dataset
w.train.set_new<-w.train.set[-(9:10)]
w.test.set_new<-w.test.set[-(9:10)]

#Store labels from training/test dataset
w.train_labels<-w.train.set$rating
w.test_labels<-w.test.set$rating

#We will use the KNN algorithm to predict the rating of wines using its attributes 
library(class)
library(gmodels)
set.seed(3456)
w.knn_prediction.5 <- knn(train = w.train.set_new, test = w.test.set_new, cl=w.train_labels, k = 5)
CrossTable(x=w.test_labels, y=w.knn_prediction.5, prop.chisq=FALSE)
accuracy.5<-((107+228+298)/length(w.test_labels))
accuracy.5
#Wow only 53.23% accuracy in our model with k=5. 

set.seed(3456)
w.knn_prediction.3 <- knn(train = w.train.set_new, test = w.test.set_new, cl=w.train_labels, k = 3)
CrossTable(x=w.test_labels, y=w.knn_prediction.3, prop.chisq=FALSE)
accuracy.3<-((109+233+306)/length(w.test_labels))
accuracy.3
#Wow only 54.50% accuracy in our model with k=5. 

set.seed(3456)
w.knn_prediction.7 <- knn(train = w.train.set_new, test = w.test.set_new, cl=w.train_labels, k = 7)
CrossTable(x=w.test_labels, y=w.knn_prediction.7, prop.chisq=FALSE)
accuracy.7<-((102+222+310)/length(w.test_labels))
accuracy.7
#Wow only 53.32% accuracy in our model with k=5. 

#Lets try using the random forest algorithim to see if we can create a better prediction model
library(randomForest)
set.seed(3456)
w.rf<-randomForest(x=w.train.set_new,y=w.train_labels,xtest = w.test.set_new,ytest = w.test_labels)
w.rf
accuracy.rf<-((106+258+353)/length(w.test_labels))
accuracy.rf
plot(w.rf)


