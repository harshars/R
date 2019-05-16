#---R L1---
#---Assignment 1---


rm(list=ls(all=T)) #---Cleaning the environment 

#---Importing The Dataset---


D=read.csv("Placement_Data.csv",header = T,stringsAsFactors=F)
str(D)

# Data Cleaning 

# Converting Placement date to date varibale
D$Placement.Date=as.Date(D$Placement.Date)
D$Company=as.factor(D$Company)

# Cleaning the company variable 

levels(D$Company)<-c("Aarogya Mitra","Aarogya Mitra","ADFC",
                     "ADFC","ADFC","ADFC","Aditya Birla","Ageis","Bata","BIS",                           
 "Cable Company","CCD","Care India","CCD","CCD","CCD","CCD","Client Relations",              
 "Club Mahindra","CorpOne","Dinshaw Icecreams","Dominos","NA","e-Clerx",
 "ESMS","Eureka Forbes","Eureka Forbes","First Rand Bank","Firstsource",
 "Foodworld","FRB","FRB","FRB","Fresh & Honest","GebbS Health Care",              
 "Gebbs Health care","Hazel","Health Prime Services","Heritage","HGS",
 "HGS","Hinduja","ICICI Backoffice","Idea Cellular Sales","Interview in process",
 "Karvy Data Management","KFC","KFC","Local Restaurant","Metro Shoes",                   
 "MnS","Mohsin Enterprises","NK Groups","NA","NA","Pizza hut","Reliance",
 "Reliance","Reliance","Reliance","Reliance","Serco","Shoppers Stop",
 "Skills Academy","Tricom","NA","Vandana Foundation","Vertex BPO","Vikalp",
 "Voylla","Westside","Wipro","Writer")


#----1----

mean_salary=round(mean(D$SALARY,na.rm=T))

# Counting the number of students with salary higher than the mean salary

sum(D$SALARY>mean_salary,na.rm=T) # Average salary is 8447

# Ans:- Number of student getting higher salary than average is 103 

#----2----

#  Dataframe with students who were offered salaries more than 3000 and less than 6000.
D_1=subset(D,D$SALARY>=3000 & D$SALARY<=6000)

#----3----

# Subsetting the data such that none of the rows contain a missing value.

D_2=na.omit(D)

#----4----

# Frequency table of the different job profiles that were offered

table(D$JOB.TYPE)

#----5----

# Bi-variate frequency table of all the companies and profiles

table(Company=D_2$Company,Profile=D_2$JOB.TYPE)

#----6----

# Checking if all the Student IDs are unique or not 
length(D$StudID)==length(unique(D$StudID))
# Checking the max frequency 
max(table(D$StudID))

#Ans:- No student got multiple offers

#----7----

# Month which recorded the maximum number of placements

names(which.max(table(months(D_2$Placement.Date))))

#----8----

# creating a new variable called Salary_quantile

D_2$Salary_Quartile=ifelse(D_2$SALARY<=quantile(D_2$SALARY,probs=0.25),"1st Quartile",
                    ifelse(D_2$SALARY>quantile(D_2$SALARY,probs=0.25) &
                             D_2$SALARY<=quantile(D_2$SALARY,probs=0.50),"2nd Quartile",
                    ifelse(D_2$SALARY>quantile(D_2$SALARY,probs=0.50) &
                             D_2$SALARY<=quantile(D_2$SALARY,probs=0.75),"3rd Quartile",
                             "4th Quartile" )))                          
# Subsetting the dataframe into a list 

mylist=list(Quartile_1=subset(D_2,D_2$Salary_Quartile=="1st Quartile"),
            Quartile_2=subset(D_2,D_2$Salary_Quartile=="2nd Quartile"),
            Quartile_3=subset(D_2,D_2$Salary_Quartile=="3rd Quartile"),
            Quartile_4=subset(D_2,D_2$Salary_Quartile=="4th Quartile"))
mylist
str(mylist)

#----9----

# Profile highest median salary offered

aggregate(SALARY~JOB.TYPE,"median",data=D_2)

# Ans:- Job profile Security had the higest median salary 12200 offered

#----10----

# Max salary offered on different placement days 

D_2$Placement_Week=format(D_2$Placement.Date,format="%U")

D_3=aggregate(SALARY~Placement_Week,"max",data=D_2)

plot(D_3[,1],D_3[,2],xlab="Week Number",ylab="Max salary offered",
     main="Max Salary Offered in a Perticular Week",type="l")


# Ans:- Company allotment wasn't right,
#       ideally the company offering highest salary should visit first.
#       Here The higest Package was offered in Week 20 

