#---R L1---
#---Practice Assignment 1---


rm(list=ls(all=T)) #---Cleaning the environment 

#---Importing The Dataset---

setwd("C:\\Users\\arka.bose.BLR\\Desktop\\R-L1(JUL17)")     # Setting Working Directory
A=read.csv("Data_Practice_Assignment1.csv",header = T,na.strings="",stringsAsFactors=F) # Reading the dataset
str(A) # Structure of the Data

#---Cleaning the variable AD_CONDITION_CODE

# Creating predefined vectors of five different grades  

vec0=c("0","0 - OTHER","GRADE 0-SALVAGE","P")           
vec1=c("1","1 - EXTRA ROUGH","1 - ROUGH","1.1","1.4","GRADE 1-EXTRA ROUGH")                 
vec2=c("1.5","1.6","1.7","1.8","1.9","2","2-BELOW AVERAGE","2 - BELOW AVERAGE","2 - ROUGH","2.1","2.2","2.3","2.4","GRADE 2-ROUGH")
vec3=c("2.5","2.6","2.7","2.8","2.9","3","GRADE3-AVERAGE","3 - AVERAGE","3.1","3.2","3.3","3.4","GRADE 3-AVERAGE")             
vec4=c("3.5","3.6","3.7","3.8","3.9","4","GRADE4-CLEAN","4 - ABOVE AVERAGE","4 - CLEAN","4.1","4.2","4.3","4.4","GRADE 4-CLEAN")                
vec5=c("4.5","4.6","4.7","4.8","4.9","5","GRADE 5-EXTRA CLEAN","5 - CLEAN","5 - EXTRA CLEAN","GRADE 5-EXTRA CLEAN")    
             
A$AD_CONDITION_CODE=ifelse(A$AD_CONDITION_CODE %in% vec0,"Grade-0",
                  ifelse(A$AD_CONDITION_CODE %in% vec1,"Grade-1",
                  ifelse(A$AD_CONDITION_CODE %in% vec2,"Grade-2",
                  ifelse(A$AD_CONDITION_CODE %in% vec3,"Grade-3",
                  ifelse(A$AD_CONDITION_CODE %in% vec4,"Grade-4",
                  ifelse(A$AD_CONDITION_CODE %in% vec5,"Grade-5",NA
                         ))))))

# Converting the Date variables to date type object 

A$AD_MODEL_YEAR=as.Date(ISOdate(A$AD_MODEL_YEAR, 1, 1))  
A$AD_SALE_DATE=as.Date(A$AD_SALE_DATE,"%Y-%m-%d")       

#----1----

#--- Identify the column having most NA---

# Name of the variable having most number of missing valius 
names(which.max(sapply(A,function(x){sum(ifelse(is.na(x)==T,1,0))})))

# Number of missing values of that variable 
sapply(A,function(x){sum(ifelse(is.na(x)==T,1,0))})[names(which.max(sapply(A,function(x){sum(ifelse(is.na(x)==T,1,0))})))]

# Ans :- AD_CONDITION_CODE (4091 missing values)

#----2----

#---Imputing the missing values of AD_CONDITION_CODE---

# Function for mode

mode_a=function(v)
{
  v=v[!is.na(v)]
  unique(v)[which.max(tabulate(match(v, unique(v))))]
}

# Imputing with a mode for each make

S=NULL
for(i in 1:length(unique(A$AD_MAKE)))
  {
  s=subset(A$AD_CONDITION_CODE,A$AD_MAKE==unique(A$AD_MAKE)[i]) # subsetting the conditon for each make type
  s[is.na(s)]= mode_a(s)                                  # replacing the missing values by mode
  S=c(S,s)         # Concatinating all the subsets 
  }
A$AD_CONDITION_CODE1=S


# Alternative 

# Making a summary dataframe of mode for each make and joining with the original dataframe 
A1=merge(x=A,y=aggregate(AD_CONDITION_CODE~AD_MAKE,data=A,mode_a),by="AD_MAKE") 

# Replacing the missing values 
A1$AD_CONDITION_CODE.x[is.na(A1$AD_CONDITION_CODE.x)]=A1$AD_CONDITION_CODE.y[is.na(A1$AD_CONDITION_CODE.x)]

#----3----

#---Finding out the most popular brand for used cars

# Name of the most popular brand for used cars  
which.max(table(A$AD_MAKE))

# Total number of Chevrolet cars sold 
max(table(A$AD_MAKE))

# Ans:- CHEVROLET with 1592 cars sold


#----4----

# Creating a new variable---Age of the car

A$AD_AGE=round(as.numeric(difftime(A$AD_SALE_DATE,A$AD_MODEL_YEAR,units="weeks") ),2)

#---Using correlation to capture relationship
cor(A$AD_AGE,A$AD_AMS_SALE_PRICE)  # cor= -0.7326 (Strong relationship)
cor(A$AD_ODOMETER,A$AD_AMS_SALE_PRICE)  # cor= -0.7006 (Strong relationship)

#---Plotiing the variables to capture the relationship

# Plotting Age and Final Auction Price
plot(A$AD_AGE,A$AD_AMS_SALE_PRICE,xlab="Age of the car(in Weeks)",
     ylab="Final Acution Price",pch=19,cex=.35,
     main="Final Auction Price vs. Age of The Car")


# Plotting Milage and Final Auction Price
plot(A$AD_ODOMETER,A$AD_AMS_SALE_PRICE,xlab="Mileage of The Car",
     ylab="Final Acution Price",pch=19,cex=.35,
     main="Final Auction Price vs. Mileage of The Car")


#----5----

# Dataset comprising of the mean age of cars for every Make-Series combination.
A2=aggregate(AD_AGE~AD_MAKE+AD_SERIES,mean,data=A)

#----6----

# dataset with age, mileage and price, with their respective Z-Scores.
A3=transform(A,Age=round((AD_AGE-mean(AD_AGE))/sd(AD_AGE),1),
               Milage=round((AD_ODOMETER-mean(AD_ODOMETER))/sd(AD_ODOMETER),1),
               Price=round((AD_AMS_SALE_PRICE-mean(AD_AMS_SALE_PRICE))/sd(AD_AMS_SALE_PRICE),1))

