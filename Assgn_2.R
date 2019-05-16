#---R L1---
#---Assignment 2---


rm(list=ls(all=T)) #---Cleaning the environment 

#----1----

# Function for checking for Prime number 

check_prime=function(x)
 {
  stopifnot(x>=1)
  if(x==1)                                       # if input is 1 its prime trivially
    {
      return(paste(x,"is a prime number"))
    }else
      {
       if((x>1) & (any((x%%(2:(x-1)))==0)))      # If input is greater than 1 then checking primality by division 
        {
         return(paste(x,"is not a prime number"))
        }else
          {return(paste(x,"is a prime number"))}
      }
  }

# Prompting user for a number to check its primality

input_1=as.numeric(readline("Give a Number: ")) 
check_prime(input_1)


#----2----

#--Checking if a given string is a palindrome

check_palindrome=function(x)
 {
  v=toupper(x)  # making all the letters uppercase 
  v=gsub(" ","",v)  # Trimming all the white spaces
  v=unlist(strsplit(v,"")) # splitting every letter and storing as a vector 
  ifelse(all(v==rev(v)),
         print(paste(x,"is a palindrome")),
         print(paste(x,"is not a palindrome")))
  }

# Prompting user for a string to check if it is palindrome 

input_2=readline("Give a String: ")
check_palindrome(input_2)


#----3----

# Converting a decimal number to binary

convert=function(x)
{
  if(x==floor(x))  # Condition to check if the given number is integer
   {
     if(x==2)      # if the given number is 2 then printing
      {
       B=10        # the binary equivalent 10
      }
     else          # else dividing the number consequtively by powers of 2
      {
       i=0
       B=NULL
       while(2^i<x)
        {
          b=(x%/%(2^i))%%2
          B=c(B,b)
          i=i+1
        }
      }
     return(as.numeric(paste(rev(B),collapse="")))  # returning the decimal equivalent 
  }
  else                 # If the given number is a fraction
  {
    y=floor(x)
    z=abs(x-floor(x))
    if(y %in% c(1,0))  
     {
      B1=y
     }
    if(y==2)
     {
      B1=10
      }
    else
     {
     i=0
     B1=NULL
     while(2^i<y)
      {
       b1=(y%/%(2^i))%%2
       B1=c(B1,b1) 
       i=i+1
      }
     }  
     B2=NULL
     j=0
     while((floor(z)!=z) & (j<20))    #converting the fractional part seperately
      {
       b2=z*2
       B2=c(B2,floor(b2))
       z=abs(b2-floor(b2)) 
       j=j+1
      }
  return(as.numeric(paste(paste(rev(B1),collapse=""),
                        paste(B2,collapse=""),
                          sep=".",collapse="")))
  }
}
  

# Prompting user for a number to convert it to binary 

input_3=as.numeric(readline("Give a Number: "))
convert(input_3)

#----4----

# Function for Pascal's Triangle 

pascal=function(n) 
  {
   for(i in 0:(n-1)) 
     {
      space <- ""                         # Printing the spaces 
      for(k in 0:(n-i)) 
      {
        space=paste(space, " ", sep="")   # Printing the binomial coefficients
      }
       for(j in 0:i) 
         {
         space=paste(space, sprintf("%3d",choose(i, j)), sep=" ")
         }
    print(space)
     }
  }

# Prompting user for a number specifying the number of rows 

input_4=as.numeric(readline("Give the required number of rows of Pascal's Triangle: "))
pascal(input_4)

