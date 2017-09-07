# Logistic Regression for Bank X-Sell Product
######################################################

# getting some important packages for the analysis
install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)

cat("\014") # Clearing the screen

# Setting the working directory
setwd('/Users/stacy/Downloads')
getwd()

# reading client datasets
df.client <- read.csv('bank_client.csv')
str(df.client)
View(df.client)
# reading other attributes
df.attr <- read.csv('bank_other_attributes.csv')
str(df.attr)

# reading campaign data
df.campaign <- read.csv('latest_campaign.csv')
str(df.campaign)
View(df.campaign)

# reading campaign outcome
df.campOutcome <- read.csv('campaign_outcome.csv')
str(df.campOutcome)

# Create campaign data by joining all tables together
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)
length(unique(df.data$Cust_id)) == nrow(df.data) #checking for any duplicate customer ID

# clearing out temporary tables
rm(df.temp1,df.temp2)

# see few observations of merged dataset
head(df.data)

# see a quick summary view of the dataset
summary(df.data)

# see the tables structure
str(df.data)

# check the response rate
CrossTable(df.data$y)

# split the data into training and test
set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
nrow(df.train)
nrow(df.test)

# how the categorical variables are distributed and are related with target outcome
CrossTable(df.train$job, df.train$y)
CrossTable(df.train$marital, df.train$y)
CrossTable(df.train$education, df.train$y)
CrossTable(df.train$default, df.train$y)
CrossTable(df.train$housing, df.train$y)
CrossTable(df.train$loan, df.train$y)
CrossTable(df.train$poutcome, df.train$y)
#__________________________________________________________________


# let see how the numerical variables are distributed
hist(df.train$age)
hist(df.train$balance)
hist(df.train$duration)
hist(df.train$campaign)
hist(df.train$pdays)
hist(df.train$previous)
describe(df.train[c("age", "balance", "duration", "campaign", "pdays", "previous")])

df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)






#------------------------------------------------------------------------------------------------------------------------------
#--------------------------original model with all the insignifiant variables -------------------------------------------------
df.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))

df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

full.model1 <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                     job + marital + education + default + housing + loan + poutcome, 
                   data=df.train, family = binomial)
summary(full.model1)
anova(full.model1)

#Testing sample for model 
fitted.results.model <- predict(full.model1, newdata=df.test,type='response')
fitted.results.model <- ifelse(fitted.results.model1 > 0.5,1,0)
misClasificError <- mean(fitted.results.model != df.test$yact)
print(paste('Accuracy',1-misClasificError))

table(df.test$yact,fitted.results.model1)
#------------------------------------------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------------------------------------------
#------------------Model2 ----------------Removing job enterpreneur as its pvalue is high.-------------------------------------
#loading variables
df.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="enterpreneur",]
full.model2.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                                                job + marital + education + default + housing + loan + poutcome, 
                                              data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="enterpreneur",]
predict.results <- predict(full.model2.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,default,housing,loan,poutcome,pdays)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------







#------------------------------------------------------------------------------------------------------------------------------
#-------------Model 3 ------------------ Removing pdays as pvalue is 0.81------------------------------------------------------
#loading variables
df.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="enterpreneur",]
full.model3.minus.insignificant<- glm(formula = yact ~ age + balance + duration + campaign  + previous +
                                                job + marital + education + default + housing + loan + poutcome, 
                                              data=df.train.minus.insignificant, family = binomial )
summary(full.model3.minus.insignificant)
#Testing sample
test.sample <- df.test[df.test$job!="enterpreneur",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
predict.results <- predict(full.model3.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,default,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------------------------------
#---------Model 4 --------------- Removing job unknown as pvalue is 0.9--------------------------------------------------------
#loading variables
sdf.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education + default + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,default,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------------------------------
#-----------Model 5 --------- Removing marital status single-------------------------------------------------------------------
#loading variables
sdf.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$marital!="single",]

full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education + default + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- test.sample[test.sample$marital!="single",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,default,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------------------------------
#-----------Model 6 --------- Removing dafault---------------------------------------------------------------------------------
#loading variables
sdf.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$marital!="single",]

full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education  + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- test.sample[test.sample$marital!="single",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
test.sample <- subset(test.sample,select=-c(default)) #eliminating "default"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------









#------------------------------------------------------------------------------------------------------------------------------
#----------------Removing job managemnt----------------------------------------------------------------------------------------
#loading variables
sdf.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="management",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$marital!="single",]

full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education  + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- test.sample[test.sample$job!="management",]
test.sample <- test.sample[test.sample$marital!="single",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
test.sample <- subset(test.sample,select=-c(default)) #eliminating "default"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------------------------------
#Model 7 ---------  removing Poutcome "other" ---------------------------------------------------------------------------------
#loading variables
sdf.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="management",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$marital!="single",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$poutcome!="other",]

full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education  + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- test.sample[test.sample$job!="management",]
test.sample <- test.sample[test.sample$marital!="single",]
test.sample <- test.sample[test.sample$poutcome!="other",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
test.sample <- subset(test.sample,select=-c(default)) #eliminating "default"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------------------------------
#Model 8 -------------------------------- removing education  "unknown "   ------------------------------
#loading variables
sdf.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="management",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$marital!="single",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$poutcome!="other",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$education!="unknown",]

full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education  + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)

#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- test.sample[test.sample$job!="management",]
test.sample <- test.sample[test.sample$marital!="single",]
test.sample <- test.sample[test.sample$poutcome!="other",]
test.sample <- test.sample[test.sample$education!="unknown",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
test.sample <- subset(test.sample,select=-c(default)) #eliminating "default"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------------------------------
#Model 9 ----- removing job  student ---------------------------------------- FINAL MODEL
#loading variables
df.client <- read.csv('bank_client.csv')
df.campaign <- read.csv('latest_campaign.csv')
df.campOutcome <- read.csv('campaign_outcome.csv')
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)

set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
df.test$yact = ifelse(df.test$y == 'yes',1,0)
df.train$yact = ifelse(df.train$y == 'yes',1,0)

df.train.minus.insignificant <- df.train[df.train$job!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="entrepreneur",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="management",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$marital!="single",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$poutcome!="other",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$education!="unknown",]
df.train.minus.insignificant <- df.train.minus.insignificant[df.train.minus.insignificant$job!="student",]

full.model1.minus.insignificant <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                         job + marital + education  + housing + loan + poutcome, 
                                       data=df.train.minus.insignificant, family = binomial )
summary(full.model1.minus.insignificant)
#Testing sample
test.sample <- df.test[df.test$job!="unknown",]
test.sample <- test.sample[test.sample$job!="entrepreneur",]
test.sample <- test.sample[test.sample$job!="management",]
test.sample <- test.sample[test.sample$job!="student",]
test.sample <- test.sample[test.sample$marital!="single",]
test.sample <- test.sample[test.sample$poutcome!="other",]
test.sample <- test.sample[test.sample$education!="unknown",]
test.sample <- subset(test.sample,select=-c(pdays)) #eliminating "pdays"
test.sample <- subset(test.sample,select=-c(default)) #eliminating "default"
predict.results <- predict(full.model1.minus.insignificant,newdata=subset(test.sample,select=c(age,balance,duration,campaign,previous,job,marital,education,housing,loan,poutcome)),type='response')
predict.results <- ifelse(predict.results>0.5,1,0)
misClasificError <- mean(predict.results != test.sample$yact)
print(paste('Accuracy',1-misClasificError))
#------------------------------------------------------------------------------------------------------------------------------







#TO PROVE DROPPING A SIGNIFICANT VARIABLE RESULTS IN DROP IN ACCURACY OF THE MODEL
#Model--Testing- To prove dropping of a significant variable resulting in decrease in accuracy
#--------------------------------------------------------------
full.model.check <- glm(formula = yact ~ age + balance +  campaign  +
                     job + marital + education + default + housing + loan + poutcome, 
                   data=df.train, family = binomial)
summary(full.model.check)
anova(full.model.check,test="Chisq")

fitted.results.model3 <- predict(full.model.check,newdata=subset(df.test,select=c(2,7,15,16,3,4,5,6,8,9,18)),type='response')
fitted.results.model3 <- ifelse(fitted.results.model3 > 0.5,1,0)
misClasificError <- mean(fitted.results.model3 != df.test$yact)
print(paste('Accuracy',1-misClasificError))
#---------------------------------------------------------------------------------------------

df.train <- df.train.minus.insignificant

full.model <- full.model1.minus.insignificant
anova(final.model) #check the annova model to verify the deviance of residual


#confusion mstrix
table(predict.results,test.sample$yact)


# check for vif
fit <- lm(formula <- yact ~ age + balance + duration + campaign + pdays + previous +
            job + marital + education + default + housing + loan + poutcome, 
          data=df.train)
vif(fit)

# automated variable selection - Backward
backward <- step(full.model, direction = 'backward')
summary(backward)


# variable importance
varImp(full.model, scale = FALSE)


# DO IT YOURSELF ------------------------------------------------------------------
# Improve your model by removing insignificant variables
# check performance of test data
