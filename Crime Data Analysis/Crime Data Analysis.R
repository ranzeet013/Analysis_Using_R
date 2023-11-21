# loading the libraries

library(readxl)
library(ggplot2)

# reading the case file data 

setwd('C:/Users/DELL/Desktop/python project/r/excel')
case_file = read_xls('./FBI Crime Data.xls')

# lets check the head and tail of the data 

head(case_file, 5)

tail(case_file, 5)

# we can see that out header is in the third row

colnames(case_file) = case_file[3, ]
print(case_file)

# lets see the data summeary

summary(case_file)

# we can see that the first column is character so let's create a nee column called
# converted_index
# use sapply to create it

case_file['converted_index'] = sapply(case_file[,1], as.numeric)

# dropping the columns with the NA value

case_file_filter = (
  case_file[!is.na(
    case_file[, ncol(case_file)]
  ),]
)

# removing the years with 5 digit using substr

years = substr(case_file$Year, 1, 4)

# drop the column years and converted_index

drop_columns = c('Year', 'converted_index')

case_file_filter = (
  case_file_filter[! colnames(case_file_filter) %in% drop_columns]
)

# performing exploratory data analysis
# let's convert data into numeric data

numeric_crime_data = data.frame(sapply(case_file_filter, as.numeric))

# set row names

row.names(numeric_crime_data) = years
row.names(numeric_crime_data) = years 

# creating the dataframe with rates and nominal columns

numeric_crime_data_rates = (
  numeric_crime_data[, grepl('rate', names(numeric_crime_data))]
)


numeric_crime_data_nominal = (
  numeric_crime_data[,!grepl( "rate" , names(numeric_crime_data))]
)

# lets see the summary for numeric data

summary(numeric_crime_data)

# lets change the row names 

colnames(numeric_crime_data_nominal) = c(
  'Population',
  'Violent Crime',
  'Murder',
  'Robbery',
  'Aggravated Assault',
  'Property Crime',
  'Burglary',
  'Theft',
  'Motor Theft'
)

# calculate the mean of each variable

crime_means = as.data.frame(sapply(numeric_crime_data_nominal, mean))
colnames(crime_means) = c("mean_variable")


crime_means = tail(crime_means,-1)

# plotting the mean

library(ggplot2)
ggplot(data = crime_means, aes(x = row.names(crime_means),
                               y = crime_means$mean_variable/1000000)) +
  geom_bar(stat="identity", 
           fill="darkred")+
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1)) +
  xlab('Type of Crime') + ylab('Mean (in Millions)')

#voilent_crime

violent_crime = numeric_crime_data_nominal['Violent Crime']
violent_crime$Type = colnames(violent_crime)
violent_crime$Year = rownames(violent_crime)
colnames(violent_crime) = c('Values','Type', 'Year')

#robbery

robbery = numeric_crime_data_nominal['Robbery']
robbery$Type = colnames(robbery)
robbery$Year = rownames(robbery)
colnames(robbery) = c('Values','Type', 'Year')

#property crime

property_crime = numeric_crime_data_nominal['Property Crime']
property_crime$Type = colnames(property_crime)
property_crime$Year = rownames(property_crime)
colnames(property_crime) = c('Values','Type', 'Year')

#crime evolution calculation

evolution_crime = rbind(violent_crime, robbery, property_crime)

# plotting the crime evolution

ggplot(data=evolution_crime, aes(x=evolution_crime$Year,
                                 y=evolution_crime$Values/1000000,
                                 group=evolution_crime$Type,
                                 color=evolution_crime$Type
)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab('Years') + ylab('Count of Crimes (In Millions)') + 
  labs(color='Type of Crime')


# crimes in term of percent 

minimum_year = numeric_crime_data_nominal[min(row.names(numeric_crime_data_nominal)),]
maximum_year = numeric_crime_data_nominal[max(row.names(numeric_crime_data_nominal)),]
maximum_year/minimum_year




