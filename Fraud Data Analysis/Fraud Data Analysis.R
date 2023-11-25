# importing the dataset

setwd('C:/Users/DELL/Desktop/python project/r/New folder (2)')

dataframe = read.csv('./Fraud_check.csv')

head(dataframe, 5)

tail(dataframe, 5)

# changing the columns names to make it easy to analyze

colnames(dataframe) = c(
  "Undergraduate", 
  "Marital_Status", 
  "Taxable_Income", 
  "City_Population", 
  "work_Experience", 
  "Urban"
)

head(dataframe)

# setting the taxable income value of less than or equal to 30000 as risky otherwise good 

dataframe$Taxable_Income = ifelse(dataframe$Taxable_Income <= 30000, "risky",
                                  "good")

head(dataframe)

dataframe$Taxable_Income = as.factor(dataframe$Taxable_Income)

head(dataframe)
str(dataframe)


# plotting against the taxable income 

library(ggplot2)

barplot(table(as.factor(dataframe$Taxable_Income),as.factor(dataframe$Marital_Status)), 
        xlab = "Taxable Income", 
        ylab = "Frequency", 
        legend=c("Risky","Good"))

barplot(table(as.factor(dataframe$Taxable_Income),as.factor(dataframe$Undergraduate)), 
        xlab = "Taxable Income", 
        ylab = "Frequency", 
        legend=c("Risky","Good"))

barplot(table(as.factor(dataframe$Taxable_Income),as.factor(dataframe$Urban)), 
        xlab = "Taxable Income", 
        ylab = "Frequency", 
        legend=c("Risky","Good"))

library(ggforce)



ggplot(dataframe,
       aes(dataframe$Taxable_Income, 
           dataframe$City_Population))+
  geom_sina(aes(color = dataframe$Taxable_Income), size = 1)+
  scale_color_manual(values = c("#34d800","#d80400"))


ggplot(dataframe,
       aes(dataframe$Taxable_Income, 
           dataframe$work_Experience))+
  geom_sina(aes(color = dataframe$Taxable_Income), size = 1)+
  scale_color_manual(values = c("#34d800","#d80400"))

# converting into 1's and 0's

library(dplyr)

dataframe <- mutate(dataframe, Undergraduate = ifelse(Undergraduate == "YES", 1, 0))

# splitting dataframe into training and testing data frame

library(caTools)

split = sample.split(dataframe$Undergraduate, 
                      SplitRatio = 0.8)
train_data = subset(dataframe, split==TRUE)
test_data = subset(dataframe, split==FALSE)

# applying random forest 

library(randomForest)

data_model = randomForest(Taxable_Income~.,data = train_data,
                          importance = TRUE)
plot(data_model)
train_pred = predict(data_model, train_data)
mean(train_data$Taxable_Income == train_pred) #88.90

# error analysis

library(caret)

confusionMatrix(train_data$Taxable_Income, train_pred)


test_pred <- predict(data_model, test_data)
mean(test_data$Taxable_Income == test_pred) # 73.33

confusionMatrix(test_data$Taxable_Income, test_pred)
