library(tidyverse)
library(plyr)
library(readr)
library(dplyr)


#----------------------------
#READING THE DATA
#----------------------------
pro <- read.csv(file.choose())

dim(pro)#to get the shape of the original data

str(pro)# to get the structure of the data

head(pro,20)# get first 20 observations

tail(pro,20)# get last 20 observations

pro1 <- pro# make a copy


pro1[pro1=='']<-NA #assign NA to missing values


pro1[pro1=='Unknown']<-NA  #assign NA to 'Unknown' value
sum(is.na(pro1))    # check total NA values

sum(is.na(pro1$Marital_Status)) 
sum(is.na(pro1$Education_Level))
sum(is.na(pro1$Income_Category))


pro1 <- na.omit(pro1) # get all observations except NA 


pro1 <- pro1[,-c(1, 10:13,15:23)] # selected the columns we care about

dim(pro1) 


colnames(pro1)[1] <- c("Customer Status")

pro1$`Customer Status`[pro1$`Customer Status`=='Attrited Customer']<- "Inactive"
pro1$`Customer Status`[pro1$`Customer Status`=='Existing Customer']<- "Active"

str(pro1)



########################################################################################################
#Question 1 - How much is the average of numeric values for all the customers and what is your findings?   
########################################################################################################

aggregate(pro1[c(2,4,9)],pro1[1] ,mean) # finding mean for all numeric variables from dataset.


#######################################################################################################
#Question 2 - Is there any relation between Customer Status and Income_Category?
#######################################################################################################

library(MASS)       # load the MASS package
#Test the hypothesis whether the Customer Status is independent of the Income_Category at .05 significance level.
# Null hypothesis Customer status is independent of Income_Category
#Solution
#We apply the chisq.test function to the contingency table Chi_tbl.
Chi_tbl <- table(pro1$`Customer Status`,pro1$Income_Category)
Chi_tbl
chisq.test(Chi_tbl)


#######################################################################################################
#Question 3: What is the distribution of target(Customer Status)?
#######################################################################################################


#since target is categorical variable, in univariate Analysis for summarization I will find frequency and 
#for visualization I plot: pie chart or barchart 


# Pie Chart with Percentages
count<-table(pro1$`Customer Status`)
count
lbls <- c("Active", "Inactive")
pct <- round(count/sum(count)*100)
lbls <- paste(lbls, pct) # adding percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(count,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Customer Status")
# we have 84% Active customers and 16% inactive (exited) customers, so we are dealing with unbalanced data


########################################################################################################
#Question 4:What is the distribution of Gender, Education_Level, Marital Status, Income_Category and Card_Category in 
#terms of Customer Status?
#######################################################################################################

table(pro1$`Customer Status`,pro1$Gender)

table(pro1$`Customer Status`,pro1$Education_Level)

table(pro1$`Customer Status`,pro1$Marital_Status)

table(pro1$`Customer Status`,pro1$Income_Category)

table(pro1$`Customer Status`,pro1$Card_Category)


########################################################################################################
#Question 5:Is there any outliers in the data?
#######################################################################################################

#Creating a function to find outliers based n Mean and Standard deviation
notout<-function(x){
  print("summary before applying this method ")
  print(summary(x))  
  M1<-mean(x,na.rm = TRUE)
  S1<-sd(x,na.rm=TRUE)
  low1<-M1-3*S1
  up1<-M1+3*S1
  x[x<low1]<-NA 
  x[x>up1]<-NA 
  print("summary after applying this method ")
  print(summary(x)) 
  return(x)
}  

pro1$Credit_Limit<-notout(pro1$Credit_Limit)

pro1$Dependent_count<-notout(pro1$Dependent_count)



#####################################################################################################
#Question 6: What is the Customer Status in relation to each of the below variables?
####################################################################################################

library(ggplot2)

# A)	Income_Category
#converting to factor and adding levels
pro1$Income_Category <- factor(pro1$Income_Category, 
                               levels = c("Less than $40K","$40K - $60K","$60K - $80K","$80K - $120K","$120K +"))

ggplot(pro1 , aes(x = Income_Category)) +
  geom_bar(aes(fill = `Customer Status`)) + 
  xlab("Income Category")+ ylab("Number of Customers")+
  ggtitle(" Customer Status by Income" )

# B) Education_Level
#converting to factor and adding levels
pro1$Education_Level <- factor(pro1$Education_Level, 
                  levels = c("Uneducated","High School", "College","Graduate","Post-Graduate","Doctorate"))

ggplot(pro1, aes(x = Education_Level)) +
  geom_bar(aes(fill = `Customer Status`)) +
  ylab("Number of Customers") +
  xlab("Education Level") + ggtitle("Customer Status by Education Level" )


# C) Marital_Status
#converting to factor and adding levels
pro1$Marital_Status <- factor(pro1$Marital_Status, 
                               levels = c("Single", "Married","Divorced"))

ggplot(pro1 , aes(x = Marital_Status)) +
  geom_bar(aes(fill = `Customer Status`), position = position_stack(reverse = FALSE)) +
  theme(legend.position = "top") + theme_classic() + ylab("Number of Customers") + 
  xlab("Martial Status") + ggtitle("Customer Status by Martial Status" )


# D) Card_Category
#converting to factor and adding levels
pro1$Card_Category <- factor(pro1$Card_Category, 
                              levels = c("Blue", "Silver","Gold","Platinum"))

ggplot(pro1 , aes(x = Card_Category)) +
  geom_bar(aes(fill = `Customer Status`), position = position_stack(reverse = FALSE)) +
  theme(legend.position = "top") + theme_classic() + ylab("Number of Customers") + 
  xlab("Card Category") + ggtitle("Customer Status by Card Category" )

#############################################################################################
#Question 7: What are the Quantile values for Customer Status by Credit_Limit?
############################################################################################
ggplot(pro1 , aes(`Customer Status`,Credit_Limit,color= Credit_Limit)) + 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75),colour="dark red",size=1.5) + 
  theme_classic() +xlab("Customer Status") + ylab("Credit Limit") + 
  ggtitle("Customer Status by Credit Limit" )  

###########################################################################################
# Question 8: What is the correlation between Income_Category and Credit_Limit?
##########################################################################################

ggplot(pro1 , aes(Income_Category,Credit_Limit,color= Credit_Limit)) + 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75),colour="dark green",size=1.)+
  theme_classic()+xlab("Income Category") + ylab("Credit Limit") + 
  ggtitle("Income Category by Credit Limit" )



library(tidyverse)
library(modelr)
library(plyr)
library(readr)
library(dplyr)
library(caret)

glimpse(pro1)

#Continuous Vs. Continuous
#################################################################################################
#Question 9: What is the relation between Credit_Limit and Dependent_count?
#################################################################################################

plot(x = pro1$Credit_Limit, y = pro1$Dependent_count,
     pch = 46,            
     xlab = "Credit Limit", ylab = "Dependent Count", col = "red")
colors()

#or

cor(pro1$Credit_Limit,pro1$Dependent_count)#default method = "pearson"



##############################################################################################
# Question 10:  Analyze using histogram Customer_Age vs Customer Status?
###############################################################################################
ggplot(pro1, aes(x=Customer_Age, fill=`Customer Status`, color=`Customer Status`)) +
  geom_histogram(position="identity",binwidth = 2, alpha=0.5) + ylab("Number of Customers")


