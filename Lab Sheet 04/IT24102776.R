# Q1
setwd("C:\\Users\\it24102776\\Desktop\\lab")
branch_data <- read.csv("Exercise.txt", header = TRUE)
View(branch_data)
str(branch_data)
attach(branch_data)
head (branch_data)

boxplot(Sales_X1, 
        main="Box Plot for Sales", 
        ylab="Sales", 
        outline=TRUE, 
        horizontal=FALSE)

five_num_summary <- summary(Advertising_X2)
print("Five Number Summary for Advertising:")
print(five_num_summary)
iqr_value <- IQR(Advertising_X2)
print("Interquartile Range (IQR) for Advertising:")
print(iqr_value)


get_outliers <- function(x) {
  q1 <- quantile(x, 0.25)  
  q3 <- quantile(x, 0.75)  
  iqr <- q3 - q1  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr  
  outliers <- x[x < lower_bound | x > upper_bound]  
  return(outliers)  
}


outliers_years <- get_outliers(Years_X3)


print("Outliers in Years_X3:")
print(outliers_years)