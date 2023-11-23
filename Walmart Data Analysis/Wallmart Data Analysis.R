#load the walmart_data and walmart_features files

setwd('C:/Users/DELL/Desktop/python project/r/analysis')

getwd()

walmart_data = read.csv('./walmart_data.csv')
walmart_features = read.csv('./walmart_features.csv')

#store the number of rows of walmart data in the variable called n_rowa

n_rows = nrow(walmart_data)
print(n_rows)

#print the first five row of the walmart_data

head(walmart_data, 5)

#calculate the number of rows per store using table and store the result in rows_per_store

rows_per_store = table(walmart_data$Store)
print(rows_per_store)

#convert rows_per_store to dataframe

rows_per_store = as.data.frame(rows_per_store)
print(rows_per_store)

#which store has most rows

rows_per_store[order(rows_per_store$Freq), 'Var1']

#sum the sale per store in walmart data and store the data in the sum_by_store object

sum_by_store = aggregate(
   x = walmart_data$Weekly_Sales, 
   by = list(walmart_data$Store), 
   FUN = sum
)

print(sum_by_store)

#rename the column as store_number and total_sales

colnames(sum_by_store) = c('store_number', 'total_sales')

print(sum_by_store)

#plot bar plot using base r by sorting total sales from sales to most sales to least sales 
#with title sales by store and color darkgreen

barplot(sum_by_store[order(-sum_by_store$total_sales), 'total_sales'], 
        main = 'Sales By Store', 
        col = 'darkgreen')

#compute the mean of every columns in walmart features

print(walmart_features)

sapply(X = walmart_features, FUN = mean, na.rm = TRUE)

#create a new column in walmart features dataframe called standerized_cpi subtracting the mean
#and dividing by the standard deviation

walmart_features['standerized_cpi'] = (
  (walmart_features$CPI - mean(walmart_features$CPI, na.rm = TRUE))
  / sd(walmart_features$CPI, na.rm = TRUE)
)

print(walmart_features)


#produce a sale of store number 1 for every department

store_1 = walmart_data[walmart_data$Store == 1,]

print(store_1)

store_1_total = aggregate(
  x = store_1$Weekly_Sales, 
  by = list(store_1$Date), 
  FUN = sum
)
print(store_1_total)


# use ggplot to plot the total sale  per week for store 20, 
#add points to your plot

store_20 = walmart_data[walmart_data$Store == 20, ]
print(store_20)

store_20_total = aggregate(
  x = store_20$Weekly_Sales, 
  by = list(store_20$Date), 
  FUN = sum
)

print(store_20_total)

library(ggplot2)
ggplot(
  data = store_20_total, 
  aes(x = Group.1, y = x, group = 1)
) +
  geom_line(color = 'darkorange') +
  geom_point(color = 'darkorange') + ylab('Sales')

#plot the sale for the top 5 department for store 2 with ggplot2

store_2 = walmart_data[walmart_data$Store == 2,]
print(store_2)

sales_by_department = aggregate(
  x = store_2$Weekly_Sales,
  by = list(store_2$Dept),
  FUN = sum
)

print(store_2)

top_5_dept = sales_by_department[order(-sales_by_department$x), 'Group.1'][1:5]

top_5_dept_sales = store_2[store_2$Dept %in% top_5_dept,]

print(top_5_dept_sales)

ggplot(
  data = top_5_dept_sales,
  aes(x = Date, y = Weekly_Sales, group = Dept, color = Dept)
) + geom_line()


#in the graph above convert the dept to a factor and the date to a date type columns


ggplot(
  data = top_5_dept_sales,
  aes(x = Date, y = Weekly_Sales, group = Dept, color = as.factor(Dept))
) + geom_line()


#




