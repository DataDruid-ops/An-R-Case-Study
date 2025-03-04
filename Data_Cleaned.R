# SECTION 1: Data Cleaning and Preparation
#install necessary libraries
install.packages("readxl") # for reading excel file
install.packages("dplyr") # for data manipulation
install.packages("lubridate") # for handling dates
install.packages("ggplot2") # for creating visualizations

#load libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# load the excel file
data <- read_excel("Case Study Data.xlsx", sheet = "case_study_data_2025-01-16T06_4")

#view the first few rows
head(data)

#check the structure of the data
str(data)

#check for missing values
colSums(is.na(data))

#handle missing values
data_cleaned <- data %>% filter(!is.na('UNIT PRICE'))

#check for duplicates
sum(duplicated(data))

#remove duplicates
data_cleaned <- data_cleaned %>% distinct()

# create the MONTH-YEAR column
data_cleaned <- data_cleaned %>%
  mutate(Month_Year = format(DATE, "%B %Y"))

#save our cleaned data to a new file
write.csv(data_cleaned, "Cleaned_Data.csv", row.names = FALSE)

# SECTION 2: Exploratory Data Analysis
# the sales overview 

sales_by_category <- data_cleaned %>%
  group_by(`Anonymized Category`) %>%
  summarise(Total_Quantity = sum(Quantity), Total_Value = sum(Value))

sales_by_business <- data_cleaned %>%
  group_by(`Anonymized Business`) %>%
  summarise(Total_Quantity = sum(Quantity), Total_Value = sum(Value))
 
#the visualizations
ggplot(sales_by_category, aes(x = `Anonymized Category`, y = Total_Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Value by Category")

ggplot(sales_by_business, aes(x = `Anonymized business`, y = Total_Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Value by Business")

ggplot(Total_Quantity, aes(x = `Quantity`, y = Total_Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Quantity")

# our trends over time
sales_by_month_year <- data_cleaned %>%
  group_by(Month_Year) %>%
  summarise(Total_Quantity = sum(Quantity), Total_Value = sum(Value))

# Time series plot:
ggplot(sales_by_month_year, aes(x = as.Date(paste0(Month_Year, "-01"), format = "%B %Y-%d"), y = Total_Value)) + # Convert to date for plotting
  geom_line() +
  labs(title = "Sales Trend Over Time", x = "Month-Year", y = "Total Value")

# performance analysis
top_5_products_quantity <- data_cleaned %>%
  group_by('Anonymized Product') %>% 
  summarise(Total_Quantity = sum(Quantity)) %>%
  arrange(desc(Total_Quantity)) %>%
  top_n(5, Total_Quantity)

top_5_products_value <- data_cleaned %>%
  group_by('Anonymized Product') %>%
  summarise(Total_Value = sum(Value)) %>%
  arrange(desc(Total_Value)) %>%
  top_n(5, Total_Value)

# SECTION 3: Advanced Analysis
# Customer segmentation
business_segments <- data_cleaned %>%
  group_by(`Anonymized Business`) %>%
  summarise(Total_Quantity = sum(Quantity), 
            Total_Value = sum(Value),
            Transaction_Count = n()) 

set.seed(123) 
kmeans_result <- kmeans(business_segments[, c("Total_Quantity", "Total_Value", "Transaction_Count")], 3)
business_segments$Segment <- kmeans_result$cluster

#forecasting
install.packages("forecast")
library(forecast)

# we should Convert sales_by_month_year to a time series object
sales_ts <- ts(sales_by_month_year$Total_Value, frequency = 12) # Assuming monthly data

# Forecasting (example using ARIMA):
arima_model <- auto.arima(sales_ts)
forecast_values <- forecast(arima_model, h = 3) # Forecast for next 3 months

# Plot the forecast
plot(forecast_values)





































































































