# Installing and importing libraries

if(!require("pROC")) install.packages("pROC"); library("pROC")
if(!require("rpart")) install.packages("rpart"); library("rpart")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("fastDummies")) install.packages("fastDummies"); library("fastDummies")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("ROCR")) install.packages("ROCR"); library("ROCR")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")

#Set work directory, change it!
setwd("~/OneDrive/IESEG_MBDA_2018/Semester_1/Descriptive_Predictive_Analytics/GroupProject")

# Reading raw data:
dTrain <-read.table("campaign20130411.csv",sep=";",stringsAsFactors=FALSE, header = TRUE)
dTest <-read.table("campaign20140115.csv",sep=";",stringsAsFactors=FALSE, header = TRUE)
dDonors <-read.table("donors.csv",sep=";",stringsAsFactors=FALSE, header = TRUE)
dGifts <-read.table("gifts.csv",sep=";",stringsAsFactors=FALSE, header = TRUE)

# Inspecting the data:
#str(dTrain)
#str(dTest)
#str(dDonors)



#Preparing Target variable (donation > 35 euros) converting Amount
#Greater than 35 means 1
#Les than 35 means 0
dTrain$amount[dTrain$amount>100]<-100 #outliers
dTrain$gt35[dTrain$amount>35]=1
dTrain$gt35[dTrain$amount<=35]=0
dTrain$gt35<-as.factor(dTrain$gt35)
dTrain_<-dummy_cols(dTrain, remove_first_dummy = TRUE)
dTrain1<-dTrain_[c("donorID","gt35_1")]

dTest$amount[dTest$amount>60]<-60 #outliers
dTest$gt35[dTest$amount>35]=1
dTest$gt35[dTest$amount<=35]=0
dTest$gt35<-as.factor(dTest$gt35)
dTest_<-dummy_cols(dTest, remove_first_dummy = TRUE)
dTest1<-dTest_[c("donorID","gt35_1")]


#Preparing dDonors table
dDonors$gender=as.factor(dDonors$gender)
dDonors$language=as.factor(dDonors$language)

#Zip code by regions
#R1 Wallonia 1300-1499, 4000-7999
#R2 Flandres 1500-3999, 8000-9999
#R3 Brussels 1000-1299

indexR1<-(dDonors$zipcode>=1300 & dDonors$zipcode<=1499) | (dDonors$zipcode>=4000 & dDonors$zipcode<=7999)
dDonors$zipcode[indexR1]<-'R1'

indexR2<-(dDonors$zipcode>=1500 & dDonors$zipcode<=3999) | (dDonors$zipcode>=8000 & dDonors$zipcode<=9999)
dDonors$zipcode[indexR2]<-'R2'

indexR3<-(dDonors$zipcode>=1000 & dDonors$zipcode<=1299)
dDonors$zipcode[indexR3]<-'R3'

#Cleaning bad data in zipcode
indexClean<-(dDonors$zipcode=="") | (dDonors$zipcode==0) | (dDonors$zipcode=="SW6 3PS")
dDonors$zipcode[indexClean]<-"R1"
dDonors$zipcode<-as.factor(dDonors$zipcode)

dDonors0<-dDonors[c("donorID","gender","language","zipcode")]
dDonors0_ <- dummy_cols(dDonors0, remove_first_dummy = TRUE)
dDonors1<-dDonors0_[c("donorID","gender_F","gender_C","gender_S","gender_U","language_F","zipcode_R3","zipcode_R1")]


#Preparing dGifts
dGifts$date<-as.Date(dGifts$date,"%d/%m/%Y")


#Years since las gift YSLG
#We decided to work with data from 5 years.
index1<-dGifts$date <= as.Date("11/04/2013","%d/%m/%Y")
index2<-dGifts$date > as.Date("11/04/2013","%d/%m/%Y")
dGifts$YSLG[index1]<- (as.Date("11/04/2013","%d/%m/%Y")-dGifts$date[index1])/365
dGifts$YSLG[index2]<- (as.Date("15/01/2014","%d/%m/%Y")-dGifts$date[index2])/365
#Creating dummy variables for days of week and months
dGifts$month<- as.factor(months(dGifts$date))
dGifts$day<- as.factor(weekdays(dGifts$date))
dGifts1 <- dummy_cols(dGifts, remove_first_dummy = TRUE)


#Separate Gift in train and test
dGifts_train<-filter(dGifts1,dGifts1$date >= as.Date("11/04/2008","%d/%m/%Y") & dGifts1$date <= as.Date("11/04/2013","%d/%m/%Y"))
dGifts_test<- filter(dGifts1,dGifts1$date >= as.Date("01/01/2009","%d/%m/%Y") & dGifts1$date <= as.Date("01/01/2014","%d/%m/%Y"))



#Sumarizing by gift
#Computing n gifts, amount average and years since last gift (YSLG)
dGifts_train1<-summarise(group_by(dGifts_train, donorID), gift_avg_amount = mean(amount), gift_n=n(),gift_yslg = min(YSLG),
                         gJan=sum(month_January),
                         gFeb=sum(month_February),
                         gMar=sum(month_March),
                         gApr=sum(month_April),
                         gMay=sum(month_May),
                         gJun=sum(month_June),
                         gJul=sum(month_July),
                         gAug=sum(month_August),
                         gSep=sum(month_September),
                         gNov=sum(month_November),
                         gDec=sum(month_December),
                         gMon=sum(day_Monday),
                         gTue=sum(day_Tuesday),
                         gWed=sum(day_Wednesday),
                         gThu=sum(day_Thursday),
                         gSat=sum(day_Saturday),
                         gSun=sum(day_Sunday)
                         )

dGifts_test1<-summarise(group_by(dGifts_test, donorID), gift_avg_amount = mean(amount), gift_n=n(),gift_yslg = min(YSLG),
                        gJan=sum(month_January),
                        gFeb=sum(month_January),
                        gMar=sum(month_March),
                        gApr=sum(month_April),
                        gMay=sum(month_May),
                        gJun=sum(month_June),
                        gJul=sum(month_July),
                        gAug=sum(month_August),
                        gSep=sum(month_September),
                        gNov=sum(month_November),
                        gDec=sum(month_December),
                        gMon=sum(day_Monday),
                        gTue=sum(day_Tuesday),
                        gWed=sum(day_Wednesday),
                        gThu=sum(day_Thursday),
                        gSat=sum(day_Saturday),
                        gSun=sum(day_Sunday)
                        )


#Join tables Training+Donors+Gifts, Testing+Donors+Gifts
dTrain2<-merge(x=dTrain1,y=dDonors1,by="donorID",all.x=TRUE)
dTrain3<-merge(x=dTrain2,y=dGifts_train1,by="donorID",all.x=TRUE)

dTest2<-merge(x=dTest1,y=dDonors1,by="donorID",all.x=TRUE)
dTest3<-merge(x=dTest2,y=dGifts_test1,by="donorID",all.x=TRUE)


#Cleaning NA values in dTrain3 and dTest3
dTrain3[, c(1:11)][is.na(dTrain3[, c(1:11)])] <- 0
dTrain3[, 12][is.na(dTrain3[, 12])] <- 5
dTrain4<-dTrain3[c(3:12,2)]
train<-dTrain4

#Layout for plotting (c(2,2))
par(mfrow = c(3, 3))
corrplot(cor(train), method="circle",tl.col="black", tl.srt=45, mar=c(0,0,1,0),title = "Train Data")


dTest3[, c(1:11)][is.na(dTest3[, c(1:11)])]<- 0
dTest3[, 12][is.na(dTest3[, 12])]<-5
dTest4<-dTest3[c(3:12,2)]
test<-dTest4
corrplot(cor(test), method="circle",tl.col="black", tl.srt=45, mar=c(0,0,1,0) ,title = "Test Data")

#write.csv(data, file = "Donors_BaseTable.csv")


######################################################################################################################


# Function to calculate AUC: 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}
######################################################################################################################

# Running Stepwise Logistic Regression

# All possible variables:
variables = head(names(train),-1)
variablesorder = c()

# Construct a logistic regression model with no variables
model = glm(gt35_1 ~ 1,data=train,family=binomial)

# Construct a formula with all the variables
formula<-formula(paste("gt35_1","~",paste(variables,collapse="+")))

#Loop over the steps
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=train)
  #get variable with highest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("gt35_1","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=train,family=binomial)
  print(v)
}

auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)
for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("gt35_1","~",paste(vars,collapse="+"))
  model<-glm(formula,data=train,family="binomial")	
  predicttrain<-predict(model,newdata=train,type="response")
  predicttest<-predict(model,newdata=test,type="response")
  auctrain[i] = auc(train$gt35_1,predicttrain)
  auctest[i] = auc(test$gt35_1,predicttest)
}

#par(fmrow=c(1,1))
plot(auctrain, main="AUC", col="red", xlim=c(1, 12),ylim=c(0.4,0.8), xlab ="N variables",ylab="AUC value", pch=16)
points(auctest,col="blue", pch=21)
text(10,0.65,"Train",adj = 1,col="red")
text(10,0.60,"Test",adj = 1,col="blue")

#Select the model with optimal number of variables:
#After ROC curve we decided to run a model with 6 variables
finalvariables = variablesorder[c(1:6)]
formula_f<-paste("gt35_1","~",paste(finalvariables,collapse="+"))
model_f<-glm(formula_f,data=train,family="binomial")	
predicttrain_f<-predict(model_f,newdata=train,type="response")
predicttest_f<-predict(model_f,newdata=test,type="response")
auctrain_f = auc(train$gt35_1,predicttrain_f)
auctest_f = auc(test$gt35_1,predicttest_f)
print("6 Variables selected")
print(finalvariables)
print("AUC Train")
print(auctrain_f)
print("AUC Test")
print(auctest_f)

# Building a cumulative gains curve as follows: 
# Make an object that contains the true values and the predictions:

pred_gain_train <- prediction(predicttest_f,test$gt35_1)
perf_gain_train <- performance(pred_gain_train,"tpr","rpp")
plot(perf_gain_train, main="Cumulative Gains - Train", col="red",xlim=c(0,1),ylim=c(0,1),lwd=2)
abline(a=0, b=1, col="black",lty=2)

#par(fmrow=c(1,1))
pred_gain_test <- prediction(predicttest_f,test$gt35_1)
#perf_gain_test <- performance(pred_gain_test,"tpr","fpr")
perf_gain_test <- performance(pred_gain_test,"tpr","rpp")
plot(perf_gain_test, main="Cumulative Gains - Test", col="blue",xlim=c(0,1),ylim=c(0,1),lwd=2)
abline(a=0, b=1, col="black",lty=2)


#Lift curve
#par(fmrow=c(1,1))
pred_lift_train <- prediction(predicttest_f,test$gt35_1)
perf_lift_train <- performance(pred_lift_train,"lift","rpp")
plot(perf_lift_train, main="Lift - Train", col="red",xlim=c(0,1),ylim=c(0,5),lwd=2)
abline(h=1, col="black",lty=2)

pred_lift_test <- prediction(predicttest_f,test$gt35_1)
perf_lift_test <- performance(pred_lift_test,"lift","rpp")
plot(perf_lift_test, main="Lift - Test", col="blue",xlim=c(0,1),ylim=c(0,5),lwd=2)
abline(h=1, col="black",lty=2)
