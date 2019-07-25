#logistf Function --- for Lowfrequency and rare event data for potential likely hood estimate
#Biases is estimate of population parameters
#package info : https://cran.r-project.org/web/packages/logistf/logistf.pdf

#setting up the working directory

getwd()
#C:\Users\Avi\Desktop\Francis Kim ISB

#installing the required package and library 
install.packages("logistf")
library(logistf)

#loading the panel data 

df <- read.csv("mediateR.csv",header = TRUE,sep = ",")
head(df)
str(df)

#standard logisitc regression
fit1 <- logistf(data=df,AchC~gender+MGoal+Anxiety+SMint,firth=FALSE,pl=FALSE)
fit1
summary(fit1)
exp(coef(fit1))

#

# ############## ----------------------------------------------------------

#SMOTE -- Synthetic minority oversampling technique

install.packages("DMwR")
library(DMwR)

#it oversamples the rare events (<15% -- bootstrapping and KNN algorthims create more observations from that rare events)
#we can even undersample the -ve targets by randomly removing the rows with target zero
#download the datasets 
df <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F)
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]]
names
#clean the col names
names <- gsub(pattern =":|[.]",x = names, replacement="")
colnames(df) <- names
colnames(df) <-c("target", "age", "sex", "on_thyroxine", "query_on_thyroxine",
                    "on_antithyroid_medication", "thyroid_surgery", "query_hypothyroid",
                    "query_dfthyroid", "pregnant", "sick", "tumor", "lithium",
                    "goitre", "TSH_measured", "TSH", "T3_measured", "T3", "TT4_measured",
                    "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured",
                    "TBG")
head(df,5)
#clean the obervations 
df$target <- ifelse(df$target=='negative',0,1)
head(df,5)
table(df$target)

#cleaning and converting factors to numeric 
prop.table(table(df$target))
str(df)

factindex <- sapply(df,is.factor)
df[factindex] <- lapply(df[factindex], as.character)

df[ df == "?" ] = NA
df[ df == "f" ] = 0
df[ df == "t" ] = 1
df[ df == "n" ] = 0
df[ df == "y" ] = 1
df[ df == "M" ] = 0
df[ df == "F" ] = 1

df[factindex]<- lapply(df[factindex],as.numeric)
str(df)

#replace NA's with mean of that columsn
repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
df <- repalceNAsWithMean(df)

library(caret)
#doing the data partition 
set.seed(1234)
test_trainsplit <- createDataPartition(df$target,p=0.50,list = FALSE, times=1) 
trainsplit <- df[test_trainsplit,]
testsplit <- df[-test_trainsplit,]

table(trainsplit$target)
prop.table(table(trainsplit$target))
table(testsplit$target)
prop.table(table(testsplit$target))

#Treebag model - caret pass a train to model and tune it by using cross validation for 5 times 
ctrl <- trainControl(method= "cv",number = 5)

tmodel <- train(target ~. , data=trainsplit,method = "treebag",trControl = ctrl)
predictors <- names(trainsplit)[names(trainsplit)!= 'target']
pred <- predict(tmodel$finalModel, testsplit[,predictors])

#scoring predictions : Area under the curve  
library(pROC)
auc <- roc(testSplit$target, pred)
print(auc)
#smote requires targets to be factor
trainsplit$target <- as.factor(trainsplit$target)
trainsplit <-SMOTE(target ~ . , trainsplit , prec.over=100, prec.under =200)

table(trainsplit$target)
dim(trainsplit)

#change the package again
trainsplit$target <- as.numeric(trainsplit$target)
tmodel <- train(target ~. , data=trainsplit,method = "treebag",trControl = ctrl)
pred <- predict(tmodel$finalModel,testsplit[,predictors])
auc <- roc(testsplit$target,pred)
print(auc)

#prim package 
