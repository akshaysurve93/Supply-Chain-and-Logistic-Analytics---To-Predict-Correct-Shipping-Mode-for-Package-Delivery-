
setwd("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Asssignment SCLA")
getwd()

library(readxl)
datafile <- read.csv("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Asssignment SCLA/Inventory CSV.csv")

#no of Obs. and attributes in Data set
dim(datafile)

# Missing value check
sum(is.na(datafile)) 

# EDA and Descriptive Analytics
# Checking attribute types of data set 
str(datafile)

#Variables, Order Date, Order ID and Product Name are not relevant in data analysis
#model building and can be removed.
newdata <- subset(datafile, select = (-c(1,2,5)))
dim(newdata)
                
# Target variable Ship mode and independent variables, 
# Product Container and Product Sub-Category are categorical variables .
# Declare these variables as factor.
newdata$Product.Container <- as.factor(newdata$Product.Container)
newdata$Product.Sub.Category <- as.factor(newdata$Product.Sub.Category)
newdata$Ship.Mode <- as.factor(newdata$Ship.Mode)

# Rechecking variable types
str(newdata)

# taking summary of the data to see if there are any outliers
summary(newdata)

# Boxplot
boxplot(newdata$Order.Quantity, col = "light blue", main = "Boxplot of Order Quantity")
boxplot(newdata$Sales, col = "red", main = "Boxplot of sales")


#We have used a common definition of an outlier that will use
#Anything less the first quartile - 1.5 * the interquartile range
#or above the third quartile + 1.5 * the interquartile range is considered an outlier.
#The interquartile range(IQR)
#is the range between the first quartile and the third quartile (the middle 50% of the data).
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
newdata$Sales = capOutlier(newdata$Sales)
summary(newdata$Sales)
summary(newdata)
boxplot(newdata$Sales, col = "orange", main = "Boxplot of newdata$sales")
#quantile(newdata$Sales = 0.95)

#########################################################
# Spliting data into train and test
#########################################################
library(caret)
set.seed(123)
data_split<-createDataPartition(newdata$Ship.Mode, p=0.7,list = FALSE,times = 1)
train<-newdata[data_split,]
test<-newdata[-data_split,]
dim(train)
dim(test)

#Check Class Imbalance
table(train$Ship.Mode) 
# percent distribution of data
distribution = round(prop.table(table(train$Ship.Mode))*100,2)
distribution
bp = barplot(distribution)
#Clearly there is a class imbalance in the dataset 
#Regular Air more than 75% against Express Air at around 10% 
#and Delivery Truck at around 15%.
#with this data imbalance,we are not going to get accurate model.

#To address data imbalance, let's down sample 
#majority class to be the same size of smallest class
library(caret)
set.seed(123)
'%ni%' <- Negate('%in%')
train_ds <- downSample(x=train[,colnames(train) %ni% "Ship.Mode"],
                       y = train$Ship.Mode, yname = "Ship.Mode")
table(train_ds$Ship.Mode)

#Now we have equal proportion of response classes.
#Let's build model using this down sample data set.

#############################################################
# Model Building
#############################################################
#Response variable, Ship mode is Multinomial categorical variables with 3 class values. 
#This is a multinomial logistic regression problem and we are going to use ML algo neural net.
ml_model <- nnet::multinom(train_ds$Ship.Mode ~., data = train_ds)
summary(ml_model)

# predicting the values for train dataset
train_accuracy <- predict(ml_model, newdata = train_ds, "class")
# Building classification table
train_table = table(train_ds$Ship.Mode, train_accuracy)
train_table

#Train data accuracy
round((sum(diag(train_table))/sum(train_table))*100,2)

# predicting the values for test dataset
test_accuracy <- predict(ml_model, newdata = test, "class")
# Building classification table
test_table <- table(test$Ship.Mode, test_accuracy)
test_table

#Test data accuracy
round((sum(diag(test_table))/sum(test_table))*100,2)
#The accuracy of the test data set is 56.97 % which is less compared to train accuracy 69.82 % .

