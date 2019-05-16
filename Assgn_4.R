#---R L1---
#---Assignment 4---


rm(list=ls(all=T)) #---Cleaning the environment 
set.seed(1)   # Setting the seed for random number generation 
#----Importing The Dataset----

   # Setting Working Directory
data_train=read.csv("Assignment4_Train_data.csv",header = T,
                    na.strings="",stringsAsFactors=T) # Reading the train dataset
data_test=read.csv("Assignment4_test_data.csv",header = T,
                   na.strings="",stringsAsFactors=T) # Reading the test dataset

#----Cleaning Training data----

str(data_train)
summary(data_train)

# Plotting to check outliers
plot(data_train$ApplicantIncome)
plot(data_train$CoapplicantIncome)
plot(data_train$LoanAmount)

# Variables with missing value

# Gender --- 13 obs (0.02%)          LoanAmount --- 22 obs (0.036%)
# Married --- 3 obs (0.004%)         Loan_Amount_Term --- 14 obs (0.02%)
# Dependents --- 15 obs (0.024%)     Credit_History --- 50 obs (0.08%)
# Self_Employed --- 32 obs (0.052%)

# Imputing missing values

mode_a=function(v)                            #---function for mode
{
  v=v[!is.na(v)]
  unique(v)[which.max(tabulate(match(v, unique(v))))]
}

data_train$Gender[is.na(data_train$Gender)]=mode_a(as.character(data_train$Gender))
data_train$Married[is.na(data_train$Married)]=mode_a(as.character(data_train$Married))
data_train$Dependents[is.na(data_train$Dependents)]=mode_a(data_train$Dependents)
data_train$Self_Employed[is.na(data_train$Self_Employed)]=mode_a(as.character(data_train$Self_Employed))
data_train$Credit_History[is.na(data_train$Credit_History)]=mode_a(as.character(data_train$Credit_History))
data_train$LoanAmount[is.na(data_train$LoanAmount)]=median(data_train$LoanAmount,na.rm=T)
data_train$Loan_Amount_Term[is.na(data_train$Loan_Amount_Term)]=mode_a(data_train$Loan_Amount_Term)
data_train$Loan_Amount_Term=data_train$Loan_Amount_Term/12

# Capping the outliers at 95 and 5 percentile

x <- data_train$ApplicantIncome
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data_train$ApplicantIncome <- x

x <- data_train$CoapplicantIncome
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data_train$CoapplicantIncome <- x

x <- data_train$LoanAmount
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data_train$LoanAmount <- x



# Changing data type of Loan_ID , Credit_History , Dependents

data_train$Loan_ID=as.character(data_train$Loan_ID)
data_train$Credit_History=as.factor(data_train$Credit_History)
data_train$Dependents=as.character(data_train$Dependents)
data_train$Dependents=as.numeric(as.character(ifelse(data_train$Dependents=="3+",
                                          3,data_train$Dependents)))

# converting Loan_Status to 0-1 (Y=>0) 
data_train$Loan_Status=as.factor(ifelse(data_train$Loan_Status=="Y",0,1))

# Creating new variables 
# Per Head Income
# Loan ratio
# Loan dispersed

data_train$Per_Head_Income=round(data_train$ApplicantIncome/(data_train$Dependents+1),2)
data_train$Loan_ratio=round((data_train$LoanAmount/data_train$ApplicantIncome)*100,2)
data_train$Loan_dispersed=round(data_train$Loan_ratio/data_train$Loan_Amount_Term,2)

# Scaling the three money variables 

data_train$ApplicantIncome=scale(data_train$ApplicantIncome)

data_train$CoapplicantIncome=scale(data_train$CoapplicantIncome)

data_train$LoanAmount=scale(data_train$LoanAmount)


#----Visualizing the data set----

library("ggplot")
plot1=ggplot(data=data_train,aes(x=Loan_Status,fill=Gender))
plot1+geom_bar()+
  xlab("Loan Status")+ylab("Count")+
  ggtitle("Distribution of Bad Loans Across Gender")


plot2=ggplot(data=data_train,aes(x=Loan_Status,fill=Education))
plot2+geom_bar()+
  xlab("Loan Status")+ylab("Count")+
  ggtitle("Distribution of Bad Loans Across Education Qualification")

plot3=ggplot(data=data_train,aes(x=Loan_Status,fill=Self_Employed))
plot3+geom_bar()+
  xlab("Loan Status")+ylab("Count")+
  ggtitle("Distribution of Bad Loans Across Employment Status")

plot4=ggplot(data=data_train,aes(fill=Property_Area,x=Loan_Status))
plot4+geom_bar()+
  xlab("Loan Status")+
  ggtitle("Distribution of Bad Loans Across Area of Dwelling")

plot5=ggplot(data=data_train,aes(y=Per_Head_Income,x=Loan_ID))
plot5+geom_point(aes(color=Loan_Status))+
  xlab("Loan Status")+
  ggtitle("Per Head Income and Bad Loans")

plot6=ggplot(data=data_train,aes(y=Loan_ratio,x=Loan_ID))
plot6+geom_point(aes(color=Loan_Status))+
  xlab("Loan Status")+
  ggtitle("Loan ratio and Bad Loans")

plot7=ggplot(data=data_train,aes(y=Loan_dispersed,x=Loan_ID))
plot7+geom_point(aes(color=Loan_Status))+
  xlab("Loan Status")+
  ggtitle("Loan ratio and Bad Loans")


# splitting the train set into train and validating set taking 30% of the train data 

data_final=data.frame(Gender=data_train$Gender,
                      Married=data_train$Married,
                      Dependents=data_train$Dependents,
                      Education=data_train$Education,
                      Self_Employed=data_train$Self_Employed,
                      CoapplicantIncome=data_train$CoapplicantIncome,
                      Credit_History=data_train$Credit_History,
                      Property_Area=data_train$Property_Area,
                      Per_Head_Income=data_train$Per_Head_Income,
                      Loan_Ratio=data_train$Loan_ratio,
                      Loan_Dispersed=data_train$Loan_dispersed,
                      Loan_Status=data_train$Loan_Status)


idx=sample(1:nrow(data_final),size=0.3*nrow(data_final),replace=F)

final_train=data_final[-idx,]
final_vali=data_final[idx,]


#----Building a model---- 

rf=randomForest::randomForest(Loan_Status~.,data = final_train,type="prob",ntree=1000)

# Variable Importance
randomForest::varImpPlot(rf)

pred1=predict(rf,newdata =final_train)

# Validating the model

pred_vali=predict(rf,newdata = final_vali)

# Confusion matrix
t=table(Actual=final_vali$Loan_Status,Predicted=pred_vali)

# Validation error rate (cut-off 0.619)
#             Actual
#Predicted     0  1 
#         0  116 40  
#         1    5 23  
# Precision=82.1%
# Recall=36.5%


#----Cleaning Test data----

str(data_test)
summary(data_test)

# Variables with missing value

# Gender --- 11 obs (3%)            LoanAmount --- 5 obs (1.36%)
# Self_Employed --- 23 obs (6.26%)  Loan_Amount_Term --- 6 obs (1.63%)
# Dependents --- 10 obs (2.72%)     Credit_History --- 29 obs (7.9%)

# Imputing missing values
data_test$Gender[is.na(data_test$Gender)]=mode_a(as.character(data_test$Gender))
data_test$Dependents[is.na(data_test$Dependents)]=mode_a(as.character(data_test$Dependents))
data_test$Self_Employed[is.na(data_test$Self_Employed)]=mode_a(as.character(data_test$Self_Employed))
data_test$Credit_History[is.na(data_test$Credit_History)]=mode_a(as.character(data_test$Credit_History))
data_test$LoanAmount[is.na(data_test$LoanAmount)]=median(data_test$LoanAmount,na.rm=T)
data_test$Loan_Amount_Term[is.na(data_test$Loan_Amount_Term)]=mode_a(data_test$Loan_Amount_Term)
data_test$Loan_Amount_Term=data_test$Loan_Amount_Term/12
data_test$ApplicantIncome=ifelse(data_test$ApplicantIncome==0,
                                 median(data_test$ApplicantIncome),data_test$ApplicantIncome)

# Changing data type Credit_History
data_test$Credit_History=as.factor(data_test$Credit_History)
data_test$Dependents=as.character(data_test$Dependents)
data_test$Dependents=as.numeric(as.character(ifelse(data_test$Dependents=="3+",
                                                     3,data_test$Dependents)))

# Creating new variables 
# Per Head Income
# Loan ratio
# Loan dispersed

data_test$Per_Head_Income=scale(data_test$ApplicantIncome/(data_test$Dependents+1),2)
data_test$Loan_Ratio=scale((data_test$LoanAmount/data_test$ApplicantIncome)*100,2)
data_test$Loan_Dispersed=scale(data_test$Loan_Ratio/data_test$Loan_Amount_Term,2)

# Scaling the three money variables 

data_test$ApplicantIncome=scale(data_test$ApplicantIncome)

data_test$CoapplicantIncome=scale(data_test$CoapplicantIncome)

data_test$LoanAmount=scale(data_test$LoanAmount)

# fitting the model

pred_final=predict(rf,newdata=data_test[,-c(1,7,9,10)])

# Predicting the loan status for test dataset
data_test$Predicted_Loan_Status=pred_final  
 



