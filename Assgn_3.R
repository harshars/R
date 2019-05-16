#---R L1---
#---Assignment 3---


rm(list=ls(all=T)) #---Cleaning the environment 


#----1----

# Function for creating dummy

create_dummy=function(x,na_action=c("replace","omit"))  #---takes two arguments
{                                                        
  ind=which(sapply(x,class)=="factor")           #---extracting the columns which are factor 
  stopifnot(is.data.frame(x))                    #--- Cheking if the input is a data frame or not
  if(na_action=="omit")                          #---If na_action is omit then drop the missing values
    {
     x=na.omit(x)                                #---dropping the NAs
     o=NULL
      for(i in ind)
       {
        y=x[,i]                               
        O=model.matrix(~y-1)                     #---Creating Dummies
        name=names(x)[i]
        colnames(O)=sub("^y", paste(name,"_", sep = ""), colnames(O)) #---Naming the columns 
        o=cbind(o,O)
       }
    }
     else                                         # if na-action is replace then imputing with mode 
      { 
       mode_a=function(v)                            #---function for mode
       {
        v=v[!is.na(v)]
        unique(v)[which.max(tabulate(match(v, unique(v))))]
       }
   o=NULL
     for(i in ind)
      {
       x[,i][is.na(x[,i])]=mode_a(x[,i])            #---imputing with mode
       y=x[,i]
       O=model.matrix(~y-1)
       name=names(x)[i]
       colnames(O)=sub("^y", paste(name,"_", sep = ""), colnames(O))
       o=cbind(o,O)
       }
     }
    final=data.frame(x[,-ind],o)    #---final data frame with dummies
    return(final)
}


#----2----

# Function for keyword searching 

# Arguments -
# 1) keyword=Keyword to search, 2) x=Text, 3) exact= exact match or not, 3)ignore_case  

search_keyword=function(x=as.character(),keyword=as.character(),exact=c(T,F),ignore_case=c(T,F))
{
  if(exact==T) # If exact is true
  {
    if(ignore_case==T) # if ignore_case is true
    { 
      z=agrep(keyword,x,max =list(all=0),value=T,useBytes=T,ignore.case=T) # finding the text  
      z1=agrep(keyword,x,max =list(all=0),value=F,useBytes=T,ignore.case=T) # finding the row number
    }
     else
     {
       z=agrep(keyword,x,max =list(all=0),value=T,useBytes=T,ignore.case=F)
       z1=agrep(keyword,x,max =list(all=0),value=F,useBytes=T,ignore.case=F)
     }
  }
   else
   {
    if(ignore_case==T)
    {
      z=agrep(keyword,x,max =list(all=1),value=T,useBytes=T,ignore.case=T)
      z1=agrep(keyword,x,max =list(all=1),value=F,useBytes=T,ignore.case=T)
    }
     else
     {
       z=agrep(keyword,x,max =list(all=1),value=T,useBytes=T,ignore.case=F)
       z1=agrep(keyword,x,max =list(all=1),value=F,useBytes=T,ignore.case=F)
     }
   }
 return(data.frame(row_number=z1,tweet=z))   # returning a data frame with the matched texts and row numbers
}






















  

  
