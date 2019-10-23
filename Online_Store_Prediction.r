
#Packages Used
library(glmnet)
install.packages("InformationValue")
library(InformationValue)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("party")
library(party)
install.packages("partykit")
library(partykit)
library(rpart)         # CART algorithm
library(party)         # to print trees
library(partykit)      # to print trees using "party"
install.packages("rattle")
library(rattle)        # for graphics
install.packages("adabag")
library(adabag)        # for boosting
install.packages("ipred")
library(ipred)         # for bagging and error estimation
library(randomForest)  # for Random Forests
library(e1071)
library(caret)         # for training and modeling
library(MASS)          # to obtain the sample data set "Glass"
library(mlbench)
install.packages("irr")
library(irr)          # Kappa test
install.packages("Metrics")
library(Metrics)
install.packages("ROCR")
install.packages("gplots")
library(gplots)
library(ROCR)
install.packages("pROC")
library(pROC)


#Reading training and test files.
infile_Train <- read.csv("Train.csv")
infile_Test <- read.csv("Test.csv")

#Creating a new copy of tarin and test data.
infile_Train_new <- infile_Train
infile_Test_new <- infile_Test

#Loop foor imputing 'NA' in blank spaces. And printing number of missing spaces in each variables.
for (i in c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38))
{
  infile_Train_new[, i][which(infile_Train_new[, i] == "")] <- NA
  print(paste(i, sum(is.na(infile_Train_new[, i]))))
}

#Dataset without variables which has missing values.
infile_Train_reference <- infile_Train_new[, c(4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 34, 38)]

#Checking for missing patterns in data set.
library(naniar)
#vis_miss(infile_Train_new$continent)
gg_miss_upset(infile_Train_new)

#Creating a new binary response variable, '1' if revenue is non zero and '0' otherwise.
infile_Train_new$positiveTransaction[infile_Train$revenue > 0] <- 1
infile_Train_new$positiveTransaction[infile_Train$revenue <= 0] <- 0

#Creating a new binary response variables, for all categories of year.
infile_Train_new$year_2016 <- 0
infile_Train_new$year_2016[infile_Train$year == 2016] <- 1
infile_Train_new$year_2017 <- 0
infile_Train_new$year_2017[infile_Train$year == 2017] <- 1

infile_Test_new$year_2016 <- 0
infile_Test_new$year_2016[infile_Test$year == 2016] <- 1
infile_Test_new$year_2017 <- 0
infile_Test_new$year_2017[infile_Test$year == 2017] <- 1

#Creating a new binary response variables, for all categories of month.
infile_Train_new$month_1 <- 0
infile_Train_new$month_1[infile_Train$month == 1] <- 1
infile_Train_new$month_2 <- 0
infile_Train_new$month_2[infile_Train$month == 2] <- 1
infile_Train_new$month_3 <- 0
infile_Train_new$month_3[infile_Train$month == 3] <- 1
infile_Train_new$month_4 <- 0
infile_Train_new$month_4[infile_Train$month == 4] <- 1
infile_Train_new$month_5 <- 0
infile_Train_new$month_5[infile_Train$month == 5] <- 1
infile_Train_new$month_6 <- 0
infile_Train_new$month_6[infile_Train$month == 6] <- 1
infile_Train_new$month_7 <- 0
infile_Train_new$month_7[infile_Train$month == 7] <- 1
infile_Train_new$month_8 <- 0
infile_Train_new$month_8[infile_Train$month == 8] <- 1
infile_Train_new$month_9 <- 0
infile_Train_new$month_9[infile_Train$month == 9] <- 1
infile_Train_new$month_10 <- 0
infile_Train_new$month_10[infile_Train$month == 10] <- 1
infile_Train_new$month_11 <- 0
infile_Train_new$month_11[infile_Train$month == 11] <- 1
infile_Train_new$month_12 <- 0
infile_Train_new$month_12[infile_Train$month == 12] <- 1

infile_Test_new$month_1 <- 0
infile_Test_new$month_1[infile_Test$month == 1] <- 1
infile_Test_new$month_2 <- 0
infile_Test_new$month_2[infile_Test$month == 2] <- 1
infile_Test_new$month_3 <- 0
infile_Test_new$month_3[infile_Test$month == 3] <- 1
infile_Test_new$month_4 <- 0
infile_Test_new$month_4[infile_Test$month == 4] <- 1
infile_Test_new$month_5 <- 0
infile_Test_new$month_5[infile_Test$month == 5] <- 1
infile_Test_new$month_6 <- 0
infile_Test_new$month_6[infile_Test$month == 6] <- 1
infile_Test_new$month_7 <- 0
infile_Test_new$month_7[infile_Test$month == 7] <- 1
infile_Test_new$month_8 <- 0
infile_Test_new$month_8[infile_Test$month == 8] <- 1
infile_Test_new$month_9 <- 0
infile_Test_new$month_9[infile_Test$month == 9] <- 1
infile_Test_new$month_10 <- 0
infile_Test_new$month_10[infile_Test$month == 10] <- 1
infile_Test_new$month_11 <- 0
infile_Test_new$month_11[infile_Test$month == 11] <- 1
infile_Test_new$month_12 <- 0
infile_Test_new$month_12[infile_Test$month == 12] <- 1

#Creating a new binary response variables, for all categories of channel.
infile_Train_new$channel_others <- 0
infile_Train_new$channel_others[infile_Train$channelGrouping == 'others'] <- 1
infile_Train_new$channel_Affiliates <- 0
infile_Train_new$channel_Affiliates[infile_Train$channelGrouping == 'Affiliates'] <- 1
infile_Train_new$channel_Direct <- 0
infile_Train_new$channel_Direct[infile_Train$channelGrouping == 'Direct'] <- 1
infile_Train_new$channel_Display <- 0
infile_Train_new$channel_Display[infile_Train$channelGrouping == 'Display'] <- 1
infile_Train_new$channel_OrganicSearch <- 0
infile_Train_new$channel_OrganicSearch[infile_Train$channelGrouping == 'Organic Search'] <- 1
infile_Train_new$channel_PaidSearch <- 0
infile_Train_new$channel_PaidSearch[infile_Train$channelGrouping == 'Paid Search'] <- 1
infile_Train_new$channel_Referral <- 0
infile_Train_new$channel_Referral[infile_Train$channelGrouping == 'Referral'] <- 1
infile_Train_new$channel_Social <- 0
infile_Train_new$channel_Social[infile_Train$channelGrouping == 'Social'] <- 1

infile_Test_new$channel_others <- 0
infile_Test_new$channel_others[infile_Test$channelGrouping == 'others'] <- 1
infile_Test_new$channel_Affiliates <- 0
infile_Test_new$channel_Affiliates[infile_Test$channelGrouping == 'Affiliates'] <- 1
infile_Test_new$channel_Direct <- 0
infile_Test_new$channel_Direct[infile_Test$channelGrouping == 'Direct'] <- 1
infile_Test_new$channel_Display <- 0
infile_Test_new$channel_Display[infile_Test$channelGrouping == 'Display'] <- 1
infile_Test_new$channel_OrganicSearch <- 0
infile_Test_new$channel_OrganicSearch[infile_Test$channelGrouping == 'Organic Search'] <- 1
infile_Test_new$channel_PaidSearch <- 0
infile_Test_new$channel_PaidSearch[infile_Test$channelGrouping == 'Paid Search'] <- 1
infile_Test_new$channel_Referral <- 0
infile_Test_new$channel_Referral[infile_Test$channelGrouping == 'Referral'] <- 1
infile_Test_new$channel_Social <- 0
infile_Test_new$channel_Social[infile_Test$channelGrouping == 'Social'] <- 1

#Creating a new binary response variables, for all categories of Browser.
infile_Train_new$browser_Amazon_silk <- 0
infile_Train_new$browser_Amazon_silk[infile_Train$browser == 'Amazon Silk'] <- 1
infile_Train_new$browser_Android_Browser <- 0
infile_Train_new$browser_Android_Browser[infile_Train$browser == 'Android Browser'] <- 1
infile_Train_new$browser_Android_Webview <- 0
infile_Train_new$browser_Android_Webview[infile_Train$browser == 'Android Webview'] <- 1
infile_Train_new$browser_Chrome <- 0
infile_Train_new$browser_Chrome[infile_Train$browser == 'Chrome'] <- 1
infile_Train_new$browser_Edge <- 0
infile_Train_new$browser_Edge[infile_Train$browser == 'Edge'] <- 1
infile_Train_new$browser_Firefox <- 0
infile_Train_new$browser_Firefox[infile_Train$browser == 'Firefox'] <- 1
infile_Train_new$browser_Internet_Explorer <- 0
infile_Train_new$browser_Internet_Explorer[infile_Train$browser == 'Internet_Explorer'] <- 1
infile_Train_new$browser_Opera <- 0
infile_Train_new$browser_Opera[infile_Train$browser == 'Opera'] <- 1
infile_Train_new$browser_Opera_Mini <- 0
infile_Train_new$browser_Opera_Mini[infile_Train$browser == 'Opera Mini'] <- 1
infile_Train_new$browser_Safari <- 0
infile_Train_new$browser_Safari[infile_Train$browser == 'Safari'] <- 1
infile_Train_new$browser_Safari_app <- 0
infile_Train_new$browser_Safari_app[infile_Train$browser == 'Safari (in-app)'] <- 1

infile_Test_new$browser_Amazon_silk <- 0
infile_Test_new$browser_Amazon_silk[infile_Test$browser == 'Amazon Silk'] <- 1
infile_Test_new$browser_Android_Browser <- 0
infile_Test_new$browser_Android_Browser[infile_Test$browser == 'Android Browser'] <- 1
infile_Test_new$browser_Android_Webview <- 0
infile_Test_new$browser_Android_Webview[infile_Test$browser == 'Android Webview'] <- 1
infile_Test_new$browser_Chrome <- 0
infile_Test_new$browser_Chrome[infile_Test$browser == 'Chrome'] <- 1
infile_Test_new$browser_Edge <- 0
infile_Test_new$browser_Edge[infile_Test$browser == 'Edge'] <- 1
infile_Test_new$browser_Firefox <- 0
infile_Test_new$browser_Firefox[infile_Test$browser == 'Firefox'] <- 1
infile_Test_new$browser_Internet_Explorer <- 0
infile_Test_new$browser_Internet_Explorer[infile_Test$browser == 'Internet_Explorer'] <- 1
infile_Test_new$browser_Opera <- 0
infile_Test_new$browser_Opera[infile_Test$browser == 'Opera'] <- 1
infile_Test_new$browser_Opera_Mini <- 0
infile_Test_new$browser_Opera_Mini[infile_Test$browser == 'Opera Mini'] <- 1
infile_Test_new$browser_Safari <- 0
infile_Test_new$browser_Safari[infile_Test$browser == 'Safari'] <- 1
infile_Test_new$browser_Safari_app <- 0
infile_Test_new$browser_Safari_app[infile_Test$browser == 'Safari (in-app)'] <- 1

#Creating a new binary response variables, for all categories of device category.
infile_Train_new$deviceCategory_desktop <- 0
infile_Train_new$deviceCategory_desktop[infile_Train$deviceCategory == 'desktop'] <- 1
infile_Train_new$deviceCategory_mobile <- 0
infile_Train_new$deviceCategory_mobile[infile_Train$deviceCategory == 'mobile'] <- 1

infile_Test_new$deviceCategory_desktop <- 0
infile_Test_new$deviceCategory_desktop[infile_Test$deviceCategory == 'desktop'] <- 1
infile_Test_new$deviceCategory_mobile <- 0
infile_Test_new$deviceCategory_mobile[infile_Test$deviceCategory == 'mobile'] <- 1


#Creating a new binary response variables, for all categories of continent.
infile_Train_new$continent_Africa <- 0
infile_Train_new$continent_Africa[infile_Train$continent == 'Africa'] <- 1
infile_Train_new$continent_Americas <- 0
infile_Train_new$continent_Americas[infile_Train$continent == 'Americas'] <- 1
infile_Train_new$continent_Asia <- 0
infile_Train_new$continent_Asia[infile_Train$continent == 'Asia'] <- 1
infile_Train_new$continent_Europe <- 0
infile_Train_new$continent_Europe[infile_Train$continent == 'Europe'] <- 1
infile_Train_new$continent_Oceania <- 0
infile_Train_new$continent_Oceania[infile_Train$continent == 'Oceania'] <- 1

infile_Test_new$continent_Africa <- 0
infile_Test_new$continent_Africa[infile_Test$continent == 'Africa'] <- 1
infile_Test_new$continent_Americas <- 0
infile_Test_new$continent_Americas[infile_Test$continent == 'Americas'] <- 1
infile_Test_new$continent_Asia <- 0
infile_Test_new$continent_Asia[infile_Test$continent == 'Asia'] <- 1
infile_Test_new$continent_Europe <- 0
infile_Test_new$continent_Europe[infile_Test$continent == 'Europe'] <- 1
infile_Test_new$continent_Oceania <- 0
infile_Test_new$continent_Oceania[infile_Test$continent == 'Oceania'] <- 1

#Creating a new binary response variable, for customer who visited shop before.
infile_Train_new$timeSinceLastVisit_zero[infile_Train_new$timeSinceLastVisit > 0] <- 1
infile_Train_new$timeSinceLastVisit_zero[infile_Train_new$timeSinceLastVisit <= 0] <- 0

infile_Test_new$timeSinceLastVisit_zero[infile_Test_new$timeSinceLastVisit > 0] <- 1
infile_Test_new$timeSinceLastVisit_zero[infile_Test_new$timeSinceLastVisit <= 0] <- 0

#Creating a new response variable, for customer who visited shop in interval of 4 weeks.
infile_Train_new$timeSinceLastVisit_4weeks <- infile_Train_new$timeSinceLastVisit/2419200

infile_Test_new$timeSinceLastVisit_4weeks <- infile_Test_new$timeSinceLastVisit/2419200

View(infile_Train_new)

#For this part, we are removing the few observations which do not have any continent value.
#Then evaluating the model: outcome ~ continent.
#Here we have created different dummy variables for all the categories of continents.
#Explain the coe?cients relating to continent.
Train_Q_a_i <- infile_Train_new[, c(75, 76, 77, 78, 79, 39)]
reglog_Q_a_i <- glm(data = Train_Q_a_i, positiveTransaction~ ., family = "binomial")
summary(reglog_Q_a_i)
exp(coef(reglog_Q_a_i))

#Evaluating the model: outcome ~ month.
#Here we have created different dummy variables for all the categories of months.
#Explain the coe?cients relating to continent.
Train_Q_a_ii <- infile_Train_new[, c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 39)]
reglog_Q_a_ii <- glm(data = Train_Q_a_ii, positiveTransaction~ ., family = "binomial")
summary(reglog_Q_a_ii)
exp(coef(reglog_Q_a_ii))

#Evaluating the model: outcome ~ timeSinceLastVisit.
Train_Q_a_iii <- infile_Train_new[, c(9, 39)]
reglog_Q_a_iii <- glm(data = Train_Q_a_iii, positiveTransaction~ ., family = "binomial")
summary(reglog_Q_a_iii)
exp(coef(reglog_Q_a_iii))

#Evaluating the model: outcome ~ timeSinceLastVisit + timeSinceLastVisit > 0.
#Creating a new binary response variable, '1' if timeSinceLastVisit is non zero and '0' otherwise.
Train_Q_a_iv <- infile_Train_new[, c(9, 39)]
Train_Q_a_iv$timeSinceLastVisit_zero[Train_Q_a_iii$timeSinceLastVisit > 0] <- 1
Train_Q_a_iv$timeSinceLastVisit_zero[Train_Q_a_iii$timeSinceLastVisit <= 0] <- 0
reglog_Q_a_iv <- glm(data = Train_Q_a_iv, positiveTransaction~ ., family = "binomial")
summary(reglog_Q_a_iv)
exp(coef(reglog_Q_a_iv))


#Creating a new response variable, for customer who visited shop in interval of 4 weeks.
Train_Q_a_iv_2 <- Train_Q_a_iii
Train_Q_a_iv_2$timeSinceLastVisit_4weeks <- Train_Q_a_iv$timeSinceLastVisit/2419200
reglog_Q_a_iv_2 <- glm(data = Train_Q_a_iv_2, positiveTransaction~ ., family = "binomial")
summary(reglog_Q_a_iv_2)
exp(coef(reglog_Q_a_iv_2))


infile_Train_new2 <- infile_Train_new[, c(2, 7, 8, 9, 12, 34, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81)]

#Logistic Regression

reglog <- glm(data = infile_Train_new2, as.factor(positiveTransaction)~ ., family = "binomial")
summary(reglog)
reglog
#Hoslem test
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(infile_Train_new2$positiveTransaction, fitted(reglog))
#Pseudo R-sq value
lln2 <- reglog$null.deviance/-2
llp2 <- reglog$deviance/-2
(lln2 - llp2)/ lln2

#Penalized Logistic Regression
#Creating datasets for dependent and independent variables.
y <- infile_Train_new[, 39]
x <- infile_Train_new2[, -6]
#Creating matrix of above datasets.
mat <- data.matrix(x)
mattest <- data.matrix(infile_Test_new)
mat_y <- as.factor(y)

#Estimation of Lambda
grid <- 10^seq(10, -2, length = 100)
#Cross-Validation of lambda value.
lamreglas <- cv.glmnet(x = mat, y = mat_y, alpha = 1, family = "binomial", lambda = grid, type.measure = "class")
#Choosing Min.lambda
#OUTPUT
#$lambda.min
#[1] 0.1232847
#$lambda.1se
#[1] 0.1232847

#Penalized Regression (LASSO)
#reglas <- glmnet(x = mat, y = spmat_y, family = "binomial", alpha = 0, lambda = 80)
reglas <- glmnet(x = mat, y = mat_y, family = "binomial", alpha = 0, lambda = lamreglas$lambda.min, standardize = FALSE)
reglas
summary(reglas)


#Performance Measurement
#Kappa
kappa2(cbind(regrandfor$predicted, infile_Train_new2$positiveTransaction))



#Decision Tree
regdect <- rpart(data = infile_Train_new2, positiveTransaction ~ ., method = "class", parms=list(split="gini"), control = rpart.control(minisplot = 1, minibucket = 0, cp = 0))
summary(regdect)
regdect
#rpart.plot(regdect,box.palette="RdBu", shadow.col="gray", nn=TRUE)

regdect$cptable
plotcp(regdect)

pfit<-prune(regdect,cp=0.0011) #setting the cp to 0.0011 will produce tree with 18 leaves
fancyRpartPlot(pfit)
pfit<-prune(regdect,cp=0.0039) #setting the cp to 0.0039 will produce tree with 9 leaves
fancyRpartPlot(pfit)
pfit<-prune(regdect,cp=0.008) #setting the cp to 0.0080 will produce tree with 7 leaves
fancyRpartPlot(pfit)

#Performance Measurement
#Confusion Matrix
confusionMatrix(regdect$y, infile_Train_new2$positiveTransaction)
#Kappa
kappa2(cbind(regdect$y, infile_Train_new2$positiveTransaction))


#Random Forest

regrandfor <- randomForest(data = infile_Train_new2, as.factor(positiveTransaction) ~ ., ntree = 700, prob = TRUE, mtry = 8)
regrandfor


#Performance Measurement
#Kappa
kappa2(cbind(regrandfor$predicted, infile_Train_new2$positiveTransaction))



#Support Vector Machine
#Reducing number of observations for enhancing computing performance.
infile_Train_newsvm <- data.frame(infile_Train_new2[1:40000,])
#Tunning Parameter for cost
tune <- tune(svm, data = infile_Train_new2, as.factor(positiveTransaction)~., ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
#Support Vector Machine
regsvm <- svm(data = infile_Train_newsvm, as.factor(positiveTransaction)~., kernel = "radial", probability = TRUE, scale = TRUE, cost = 10)
regsvm

#Performance Measurement
#Kappa
kappa2(cbind(regsvm$fitted, infile_Train_new2$positiveTransaction))


regrandfor <- randomForest(data = infile_Train_new2, as.factor(positiveTransaction) ~ ., ntree = 800, prob = TRUE, mtry = 8)
regrandfor

pred3_1 <- predict(regrandfor, infile_Test_new, type = "prob")
pred3_1
sub3_1 <- cbind(infile_Test_new$sessionId, pred3_1)
#Writing the predicted values in a csv file
write.csv(sub3_1, file = "Prediction-3_2.csv")


#Confusion Matrix
confusionMatrix(regrandfor$predicted, infile_Train_new2$positiveTransaction)

#Kappa
kappa2(cbind(regrandfor$predicted, infile_Train_new2$positiveTransaction))

#LogLoss
logLoss(infile_Train_new2$positiveTransaction, regrandfor$votes[,2])


#ROC
predicted <- as.factor(regrandfor$predicted)
actual <- as.factor(infile_Train_new2$positiveTransaction)
predd <- prediction(as.numeric(predicted), as.numeric(actual))


#ROC
roccurve <- performance(predd, "tpr", "fpr")
plot(roccurve, colorsize = TRUE, print.cutoffs.at = c(0.5, 0.75, 1))


#AUROC
auc(regrandfor$predicted, infile_Train_new2$positiveTransaction)


#D Statistics

#now let's take a look at the residuals
pearsonRes <-residuals(regrandfor,type="pearson")
devianceRes <-residuals(regrandfor,type="deviance")
rawRes <-residuals(regrandfor,type="response")
#studentDevRes<-rstudent(regrandfor)
fv<-regrandfor$votes[,2]
fitted.values(regrandfor)
#let's go ahead and create a classification based on the probability
#infile_Train_new2$pred<-as.numeric(regrandfor$fitted.values>0)

predVals <-data.frame(trueVal=infile_Train_new2$positiveTransaction, predClass=regrandfor$predicted,predProb=fv)
tail(predVals)

positiveTransaction.1<-predVals[predVals$trueVal==1,]
positiveTransaction.0<-predVals[predVals$trueVal==0,]

mean(positiveTransaction.1$predProb) - mean(positiveTransaction.0$predProb)


#K-S chart  (Kolmogorov-Smirnov chart) 
# measures the degree of separation 
# between the positive (y=1) and negative (y=0) distributions

predVals$group<-cut(predVals$predProb,seq(1,0,-.1),include.lowest=T)
xtab<-table(predVals$group,predVals$trueVal)

xtab

#make empty dataframe
KS<-data.frame(Group=numeric(10),
               CumPct0=numeric(10),
               CumPct1=numeric(10),
               Dif=numeric(10))

#fill data frame with information: Group ID, 
#Cumulative % of 0's, of 1's and Difference
for (i in 1:10) {
  KS$Group[i]<-i
  KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
  KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
  KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
}

KS  

KS[KS$Dif==max(KS$Dif),]

maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]

#and the K-S chart
ggplot(data=KS)+
  geom_line(aes(Group,CumPct0),color="blue")+
  geom_line(aes(Group,CumPct1),color="red")+
  geom_segment(x=maxGroup,xend=maxGroup,
               y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
  labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")


#Cummulative Gain Chart
regrandprediction <- prediction(regrandfor$votes[,2], infile_Train_new2$positiveTransaction)
regrandpositive_gain = performance(regrandprediction, "tpr", "rpp")
plot(regrandpositive_gain, col = "blue")

actual <- infile_Train_new2$positiveTransaction
predicted <- regrandfor$predicted

#cumulative gains and lift chart

myGains <- function(actual, predicted, num.groups=10, cost="F", intervention.cost=0, fp.cost=0, fn.cost=0, reward=0, TOTAL=length(actual))
{
  total.n<-length(predicted)
  
  pred.rank <- total.n + 1 - rank(predicted, ties.method = "random")
  pred.group <- ceiling(pred.rank/(total.n/num.groups))
  
  total.resp <- sum(actual)
  total.resp.rate <- total.resp/total.n
  
  n <- as.vector(tapply(predicted, pred.group, FUN = length))
  cume.n <- cumsum(n)
  
  depth <- 100 * round(cume.n/total.n, 4)
  
  resp <- as.vector(tapply(actual, pred.group, FUN = sum))
  cume.resp <- cumsum(resp)
  resp.rate <- resp/n
  cume.resp.rate <- cume.resp/cume.n
  cume.pct.of.total <- cume.resp/total.resp
  
  rand <- rep(total.resp/num.groups,num.groups)
  cume.rand <- cumsum(rand)
  rand.rate <- rep(total.resp.rate,num.groups)
  cume.rand.rate <- rand.rate
  cume.pct.of.total.rand <- seq(1/num.groups,1,length=num.groups)
  
  lift <- round(100 * (resp.rate/total.resp.rate), 0)
  cume.lift <- round(100 * (cume.resp.rate/total.resp.rate), 0)
  
  opt.rank <- total.n + 1 - rank(actual, ties.method = "random")
  opt.group <- cut(opt.rank, c(0, cume.n), labels = F)
  
  opt.resp <- as.vector(tapply(actual, opt.group, FUN = sum))
  opt.cume.resp <- cumsum(opt.resp)
  opt.resp.rate <- opt.resp/n
  opt.cume.resp.rate <- opt.cume.resp/cume.n
  opt.cume.pct.of.total <- opt.cume.resp/total.resp
  
  opt.lift <- round(100 * (opt.resp.rate/total.resp.rate), 0)
  opt.cume.lift <- round(100 * (opt.cume.resp.rate/total.resp.rate), 0)
  
  mean.prediction <- as.vector(tapply(predicted, pred.group, FUN = mean))
  min.prediction <- as.vector(tapply(predicted, pred.group, FUN = min))
  max.prediction <- as.vector(tapply(predicted, pred.group, FUN = max))
  
  if (cost=="T") {
    IC <- cume.n*intervention.cost/total.n*TOTAL
    IC <- c(0,IC)
    maxR<-cume.resp*reward/total.n*TOTAL
    maxR<- c(0,maxR)
    FP<-(cume.n-cume.resp)*fp.cost/total.n*TOTAL
    FP <- c(0,FP)
    FN<-(total.resp-cume.resp)*fn.cost/total.n*TOTAL
    FN<-c(total.resp*fn.cost/total.n*TOTAL,FN)
    total.value <- maxR - IC - FP - FN
    
    bestValue = character(num.groups+1)
    bestValue[which.max(total.value)]="*"
    
    opt.maxR<-opt.cume.resp*reward/total.n*TOTAL
    opt.maxR<- c(0,opt.maxR)
    opt.FP<-(cume.n-opt.cume.resp)*fp.cost/total.n*TOTAL
    opt.FP <- c(0,opt.FP)
    opt.FN<-(total.resp-opt.cume.resp)*fn.cost/total.n*TOTAL
    opt.FN<-c(total.resp*fn.cost/total.n*TOTAL,opt.FN)
    opt.total.value <- opt.maxR - IC - opt.FP - opt.FN
    
    rand.maxR<-cume.rand*reward/total.n*TOTAL
    rand.maxR<- c(0,rand.maxR)
    rand.FP<-(cume.n-cume.rand)*fp.cost/total.n*TOTAL
    rand.FP <- c(0,rand.FP)
    rand.FN<-(total.resp-cume.rand)*fn.cost/total.n*TOTAL
    rand.FN<-c(total.resp*fn.cost/total.n*TOTAL,rand.FN)
    rand.total.value <- rand.maxR - IC - rand.FP - rand.FN
    
    obj <- list(predicted=predicted, pred.group=pred.group, depth = depth, obs = n, cume.obs = cume.n, 
                mean.resp = resp.rate, cume.mean.resp = cume.resp.rate, 
                cume.pct.of.total = cume.pct.of.total, 
                rand.resp = rand.rate, cume.rand.resp=cume.rand, cume.pct.of.total.rand = cume.pct.of.total.rand,
                lift = lift, 
                opt.resp = opt.resp, opt.resp.rate = opt.resp.rate, opt.cume.resp = opt.cume.resp, 
                opt.cume.pct.of.total=opt.cume.pct.of.total,
                cume.lift = cume.lift, mean.prediction = mean.prediction, 
                min.prediction = min.prediction, max.prediction = max.prediction, 
                num.groups = num.groups, cost=cost,
                opt.lift = opt.lift, opt.cume.lift=opt.cume.lift,
                intervention.cost=intervention.cost,reward=reward,fp.cost=fp.cost,fn.cost=fn.cost,
                TOTAL=TOTAL, IC=IC,max_reward=maxR,FP=FP,FN=FN,total.value=total.value, bestValue=bestValue,
                opt.maxR=opt.maxR, opt.FP=opt.FP, opt.FN=opt.FN, opt.total.value=opt.total.value,
                rand.maxR=rand.maxR, rand.FP=rand.FP, rand.FN=rand.FN, rand.total.value=rand.total.value)
  }
  else
  {
    
    obj <- list(predicted=predicted, pred.group=pred.group, depth = depth, obs = n, cume.obs = cume.n, 
                mean.resp = resp.rate, cume.mean.resp = cume.resp.rate, 
                cume.pct.of.total = cume.pct.of.total, 
                rand.resp = rand.rate, cume.rand.resp=cume.rand, cume.pct.of.total.rand = cume.pct.of.total.rand,
                lift = lift, 
                opt.resp = opt.resp, opt.resp.rate = opt.resp.rate, opt.cume.resp = opt.cume.resp, 
                opt.cume.pct.of.total=opt.cume.pct.of.total,
                cume.lift = cume.lift, mean.prediction = mean.prediction, 
                min.prediction = min.prediction, max.prediction = max.prediction, 
                num.groups = num.groups, cost=cost,
                opt.lift = opt.lift, opt.cume.lift=opt.cume.lift)
  }
  
  class(obj)<-"myGains"
  return(obj)
}

print.myGains <- function(x,digits=2){
  cat("\n")
  cat("                          Random              Model                                 Wizard              ntile predicted \n")
  cat("Depth               |---------------|----------------------|             |---------------------------|    probability   \n")
  cat(" of            Cume | ntile  Cume % | ntile   Cume  Cume % |  Lift  Cume | ntile Cume %  Lift   Cume |------------------- \n")
  cat("File     N       N  | Rate  Capture | Rate    Rate Capture | Index  Lift | Rate  Capture Index  Lift | (Min, Mean, Max)  \n")
  cat("-------------------------------------------------------------------------------------------------------------------------\n")
  for (i in 1:x$num.groups) {
    cat(format(x$depth[i], width = 4), 
        format(x$obs[i], width = 7), 
        format(x$cume.obs[i], width = 8), " |",
        formatC(100 * x$rand.resp[i], width = 5, digits = 1, format = "f"), "%", 
        formatC(100 * x$cume.pct.of.total.rand[i], width = 7, digits = 1, format = "f"), "%", " |",
        formatC(100 * x$mean.resp[i], width = 5, digits = 1, format = "f"), "%", 
        formatC(100 * x$cume.mean.resp[i], width = 6, digits = 1, format = "f"),"%", 
        formatC(100 * x$cume.pct.of.total[i], width = 7, digits = 1, format = "f"), "%", " |",
        formatC(x$lift[i], width = 6, digits = 0, format = "f"), 
        formatC(x$cume.lift[i], width = 6, digits = 0, format = "f"), " |",
        formatC(100 * x$opt.resp.rate[i], width = 4, digits = 0, format = "f"), "%", 
        formatC(100 * x$opt.cume.pct.of.total[i], width = 7, digits = 0, format = "f"),"%", 
        formatC(x$opt.lift[i], width = 7, digits = 0, format = "f"), 
        formatC(x$opt.cume.lift[i], width = 6, digits = 0, format = "f"), " |",
        " (",formatC(x$min.prediction[i], width = 4, digits = 2, format = "f"), ", ",
        formatC(x$mean.prediction[i], width = 4, digits = 2, format = "f"), ", ",
        formatC(x$max.prediction[i], width = 4, digits = 2, format = "f"), ")\n", sep = "")
  }
  if (x$cost=="T"){
    cat("\n\nIndividual costs")
    cat("\n-----------------------------------\n")
    cat("  Intervention cost: $", formatC(x$intervention.cost,digits=2, width=8, format = "f"), "\n")
    cat("False positive cost: $", formatC(x$fp.cost,digits=2, width=8, format = "f"), "\n")
    cat("False negative cost: $", formatC(x$fn.cost,digits=2, width=8, format = "f"), "\n")
    cat("             Reward: $", formatC(x$reward,digits=2, width=8, format = "f"), "\n")
    cat("\nTotal costs extrapolated for application to: ", format(x$TOTAL,width=10), " objects.   All costs/benefits in thousands of $.\n\n")  
    cat("                         Costs ($)                    Max Reward  \n")
    cat("Depth  |-------------------------------------------|---------------|       \n")
    cat(" of    |  Intervention     False Pos     False Neg |  Intervention | \n")
    cat("File   |          Cost          Cost          Cost |        Reward |   Total Value  \n")
    cat("------------------------------------------------------------------------------------\n")
    
    for (i in 1:(x$num.groups+1)) {
      depth=c(0,x$depth)
      cat(format(depth[i], width = 4), "   |",
          formatC(x$IC[i]/1000, width = 14, digits = 1, format = "f"),  
          formatC(x$FP[i]/1000, width = 14, digits = 1, format = "f"),  
          formatC(x$FN[i]/1000, width = 14, digits = 1, format = "f"),  " |",
          formatC(x$max_reward[i]/1000, width = 14, digits = 1, format = "f"),  " |", 
          formatC(x$total.value[i]/1000, width = 14, digits = 1, format = "f"),
          formatC(x$bestValue[i], width = 1, format = "c"),"\n", sep = "")
    }
  }
}


myGainsPlot<-function(x)
  
{
  require(ggplot2)
  
  df<-data.frame(depth=c(0,x$depth),
                 cume.pct.of.total=c(0,x$cume.pct.of.total),
                 opt.cume.pct.of.total=c(0,x$opt.cume.pct.of.total),
                 cume.pct.of.total.rand=(c(0,x$cume.pct.of.total.rand))
  )
  
  if (x$num.groups<=25) {
    L = x$num.groups
  }
  else {
    L = 25
  }
  
  groupBreaks = seq(0,100,length=L+1)
  
  g1<-ggplot(data=df)+
    geom_line(aes(depth,opt.cume.pct.of.total),size=1.0,color="dark green")+
    geom_line(aes(depth,cume.pct.of.total.rand),size=1.0,color="black")+
    geom_line(aes(depth,cume.pct.of.total),size=1.2,color="blue")+ 
    labs(title = "Cumulative Gains Chart", x= "Depth of file (%)", y = "Cumulative Percent of Target Captured")+
    scale_x_discrete(limits=groupBreaks)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1),breaks=groupBreaks/100)+
    theme_bw()
  
  df<-data.frame(depth=c(0,x$depth),
                 modelValue=x$total.value,
                 optValue=x$opt.total.value,
                 randValue=x$rand.total.value)
  
  g2<-ggplot(data=df)+
    geom_line(aes(depth,randValue),size=1.0,color="black")+
    geom_line(aes(depth,optValue),size=1.0,color="dark green")+
    geom_line(aes(depth,modelValue),size=1.2,color="blue")+
    labs(title = "Cost/Benefit of Target Depth", x= "Depth of file (%)", y = "Reward ($)")+
    scale_x_discrete(limits=groupBreaks)+
    theme_bw()
  
  if (x$num.groups <= 50){
    g3<-ggplot(data.frame(predicted=x$predicted,pred.group=x$pred.group)) +  
      geom_boxplot(aes(x=as.factor(pred.group),y=predicted))+
      labs(title = "Parallel boxplots of predicted probability by ntile", x= "ntile", y = "Predicted probabilities")+
      theme_bw()
  }
  else{
    g3 = "Too many ntiles for parallel boxplots"
  }
  
  return(list(g3,g2,g1))
  
}

#example usage
#trueVal is vector of correct labels, predProb is predicted probability from model
#10 ntiles (default)
#cost="T" to produce the cost related analysis

predProb <- regrandfor$votes[, 2]
trueVal <- as.vector(infile_Train_new2$positiveTransaction)

g <- myGains(trueVal,predProb,10,cost="T",intervention.cost = 155, fp.cost=1, fn.cost=1,reward = 300, TOTAL=1000000)
g
myGainsPlot(g)
