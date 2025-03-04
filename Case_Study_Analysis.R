# ==========================
# Case Study Analysis - R Script
# Author : James Mwangi Githu
# Date ; 31-01-2025
# =========================


# =========================================

# SECTION 1: Data Cleaning and Preparation

# =========================================

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

#load the excel file
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

#create the MONTH-YEAR column
data_cleaned <- data_cleaned %>%
  mutate(Month_Year = format(DATE, "%B %Y"))

#save our cleaned data to a new file
write.csv(data_cleaned, "Cleaned_Data.csv", row.names = FALSE)


# =======================================

# SECTION 2: Exploratory Data Analysis

# =======================================

#sales_overview
# Calculate Total Quantity and Value by Category
category_sales <- data_cleaned %>%
  group_by(`ANONYMIZED CATEGORY`) %>%
  summarise(Total_Quantity = sum(QUANTITY),
            Total_Value = sum(QUANTITY * `UNIT PRICE`)) %>%
  arrange(desc(Total_Value))

#view the summarized data
print(category_sales)

#calculate Total Quantity and Value by Business
business_sales <- data_cleaned %>%
  group_by(`ANONYMIZED BUSINESS`) %>%
  summarise(Total_Quantity = sum(QUANTITY),
            Total_Value = sum(QUANTITY * `UNIT PRICE`)) %>%
  arrange(desc(Total_Value))

#view the summarized data
print(business_sales)

#visualizing our sales overview
#bar chart for sales by category
ggplot(category_sales, aes(x = reorder(`ANONYMIZED CATEGORY`, Total_Value), y = Total_Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Sales Value by Category", x = "Category", y = "Total Sales Value") +
  theme_minimal()

#bar chart for sales by business
ggplot(business_sales, aes(x = reorder(`ANONYMIZED BUSINESS`, Total_Value), y = Total_Value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Total Sales Value by Business", x = "Business", y = "Total Sales Value") +
  theme_minimal()

#the trends over time(sales trends by month-year)
#we Aggregate the data by Month-Year
monthly_trends <- data_cleaned %>%
  group_by(`Month-Year`) %>%
  summarise(Total_Quantity = sum(QUANTITY),
            Total_Value = sum(QUANTITY * `UNIT PRICE`)) %>%
  arrange(`Month-Year`)

#time Series Plot: Sales Trends Over Time
ggplot(monthly_trends, aes(x = `Month-Year`, y = Total_Value, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(title = "Sales Trends Over Time", x = "Month-Year", y = "Total Sales Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#performance analysis
#by top 5 most frequently purchased products(by the quantity)
top_products_qty <- data_cleaned %>%
  group_by(`ANONYMIZED PRODUCT`) %>%
  summarise(Total_Quantity = sum(QUANTITY)) %>%
  arrange(desc(Total_Quantity)) %>%
  head(5)

#view the top products by quantity
print(top_products_qty)

#by top 5 most valuable products(by total sales value)
top_products_value <- data_cleaned %>%
  group_by(`ANONYMIZED PRODUCT`) %>%
  summarise(Total_Value = sum(QUANTITY * `UNIT PRICE`)) %>%
  arrange(desc(Total_Value)) %>%
  head(5)

#view the top products by total sales value
print(top_products_value)

#the bar chart for top 5 products by quantity
ggplot(top_products_qty, aes(x = reorder(`ANONYMIZED PRODUCT`, Total_Quantity), y = Total_Quantity)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 5 Most Purchased Products", x = "Product", y = "Total Quantity") +
  theme_minimal()

#bar chart for top 5 products by value
ggplot(top_products_value, aes(x = reorder(`ANONYMIZED PRODUCT`, Total_Value), y = Total_Value)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top 5 Most Valuable Products", x = "Product", y = "Total Sales Value") +
  theme_minimal()

# ============================

# SECTION 3: Advanced Analysis

# ===========================

#customer segmentation
#we aggregate customer purchase behavior
business_segmentation <- data_cleaned %>%
  group_by(`ANONYMIZED BUSINESS`) %>%
  summarise(
    Total_Quantity = sum(QUANTITY),
    Total_Value = sum(QUANTITY * `UNIT PRICE`),
    Frequency = n() # no. of transactions
  )

#we should normalize the data for clustering
business_scaled <- scale(business_segmentation[, 2:4])

#apply 3 clusters for High, Medium, Low
set.seed(123)
kmeans_result <- kmeans(business_scaled, centers = 3)

#add the cluster labels
business_segmentation$Segment <- factor(kmeans_result$cluster, 
                                        levels = c(1,2,3),
                                        labels = c("Low Value", "Medium Value", "High Value"))

#View segmentation results
print(business_segmentation)

#visualizations of business segmentation
ggplot(business_segmentation, aes(x = Total_Value, y = Frequency, color = Segment)) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation: Value vs Frequency",
       x = "Total Sales Value", y = "Transaction Frequency") +
  theme_minimal()

##Insights
#No 1. High-Value businesses should get VIP service and special promotions.
#No 2. Medium-Value businesses can be nurtured with targeted campaigns.
#No 3. Low-Value businesses might need retention efforts.

#forecasting sales for the next 3 months
install.packages("forecast")
library(forecast)

#we first Prepare the time series data
sales_ts <- ts(monthly_trends$Total_Value, frequency = 12, start = c(2024, 1)) 

#ARIMA model
arima_model <- auto.arima(sales_ts)

#Forecast for next 3 months
forecast_sales <- forecast(arima_model, h = 3)

#Plotting the forecast
autoplot(forecast_sales) +
  labs(title = "Sales Forecast for Next 3 Months",
       x = "Month-Year", y = "Total Sales Value") +
  theme_minimal()

#the forecast values
print(forecast_sales)

#anomaly detection to identify spikes in sales
install.packages("tsoutliers")
library(tsoutliers)

#to identify anomalies in sales trends
sales_outliers <- tso(sales_ts)

#Plotting the anomalies
autoplot(sales_ts) +
  autolayer(sales_outliers$outliers, series = "Outliers", col = "red", size = 3) +
  labs(title = "Anomaly Detection in Sales",
       x = "Month-Year", y = "Total Sales Value") +
  theme_minimal()

##insights
# No 1. Unexpected spikes indicate promotions have been done or seasonal demands increase.
# No 2. The drops indicate supply chain issues and declines in demand.


#Correlation Analysis
#relationship between quantity and value
correlation_value <- cor(data_cleaned$QUANTITY, data_cleaned$`UNIT PRICE` * data_cleaned$QUANTITY)
print(paste("Correlation between Quantity and Sales Value:", correlation_value))

#Scatter Plot: Quantity vs Value
ggplot(data_cleaned, aes(x = QUANTITY, y = QUANTITY * `UNIT PRICE`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlation between Quantity and Sales Value",
       x = "Quantity Sold", y = "Total Sales Value") +
  theme_minimal()

#insights
#No 1. Higher correlation means more quantity sold directly increases revenue.
#No 2. Weaker correlation indicates pricing effects, bulk discounts, or product mix variations.

# ==================================================

# SECTION 4: Strategic Insights and Recommendations

# =================================================

#Product strategy : prioritizing a product category for marketing

#we shall identify which product should be prioritized for marketing campaigns.

#approach
# no 1. choose the category with high sales value and consistent demand.
# no 2. avoid categories with seasonal spikes (only unless for seasonal campaigns).

#identify the top-selling product category
top_category <- category_sales %>%
  arrange(desc(Total_Value)) %>%
  head(1)

print(top_category)

#insights
# no 1.If a single product contributes most revenue, it should be prioritized in marketing.
# no 2. if multiple categories have stable demand, cross-promotions can be planned.

#Customer retention : identifying businesses with declining purchases
# Count transactions per business over time
business_trends <- data_cleaned %>%
  group_by(`ANONYMIZED BUSINESS`, `Month-Year`) %>%
  summarise(Transaction_Count = n(), .groups = "drop")

# Identify businesses with declining trend
declining_businesses <- business_trends %>%
  group_by(`ANONYMIZED BUSINESS`) %>%
  summarise(Change_in_Transactions = last(Transaction_Count) - first(Transaction_Count)) %>%
  filter(Change_in_Transactions < 0)

print(declining_businesses)

#retention strategies
# 1. Personalized discounts: offer targeted discounts to these businesses.
# 2. loyalty programs: introduce rewards for frequent purchases.
# 3. customer feedback: investigate reasons for decline e.g pricing

#Operational efficiency : improving inventory and supply chain
#Identify seasonal patterns in sales
ggplot(monthly_trends, aes(x = `Month-Year`, y = Total_Value)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Seasonal Demand Trends", x = "Month-Year", y = "Total Sales Value") +
  theme_minimal()

#improvement strategy
# 1.If sales peak in specific months, increase stock before those months.
# 2. If sales drop in some periods, reduce inventory costs tto avoid overstocking.
# 3. Adjust supply chain logistics based on sales fluctuations.

# ===================================

# SCETION 5: Dashboard and Reporting

# ==================================

# 1. sales overview by anonymized category
ggplot(category_sales, aes(x = reorder(`ANONYMIZED CATEGORY`, -Total_Value), y = Total_Value, fill = `ANONYMIZED CATEGORY`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Sales Value by Category",
       x = "Category", y = "Total Sales Value") +
  theme_minimal()

#Insight : identify top-selling product categories.

# 2.TOp businesses by sales value
ggplot(business_sales, aes(x = reorder(`ANONYMIZED BUSINESS`, -Total_Value), y = Total_Value, fill = `ANONYMIZED BUSINESS`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Businesses by Sales Value",
       x = "Business", y = "Total Sales Value") +
  theme_minimal()

#Insight : Target high-value businesses for premium offers.

# 3.Sales trends over time
ggplot(monthly_trends, aes(x = `Month-Year`, y = Total_Value)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Monthly Sales Trends",
       x = "Month-Year", y = "Total Sales Value") +
  theme_minimal()

#Insight : Helps identify seasonal trends for inventory planning.

# a. Total quantity and value by category
ggplot(category_sales, aes(x = `ANONYMIZED CATEGORY`)) +
  geom_bar(aes(y = Total_Quantity, fill = "Quantity"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_Value, fill = "Value"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Quantity" = "blue", "Value" = "red")) +
  labs(title = "Total Quantity and Value by Category", x = "Category", y = "Total") +
  theme_minimal()

# b. Top-performing products and businesses
ggplot(top_businesses, aes(x = reorder(`ANONYMIZED BUSINESS`, Total_Value), y = Total_Value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 5 Businesses by Sales Value", x = "Business", y = "Total Sales Value") +
  theme_minimal()

# c. Time-series chart of sales trends
ggplot(monthly_trends, aes(x = `Month-Year`)) +
  geom_line(aes(y = Total_Value, color = "Sales Value"), size = 1) +
  geom_line(aes(y = Total_Quantity, color = "Quantity Sold"), size = 1, linetype = "dashed") +
  labs(title = "Monthly Sales Trends", x = "Month-Year", y = "Sales") +
  scale_color_manual(values = c("Sales Value" = "blue", "Quantity Sold" = "red")) +
  theme_minimal()

# d. Customer segmentation summary
ggplot(customer_segmentation, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  labs(title = "Customer Segmentation Summary",
       x = "Segment", y = "Number of Businesses") +
  theme_minimal()

#Predictive analysis : incorporating external factors
# 1. economic conditions e.g inflation, interest rates and GDP growth
# 2. competitor actions e.g pricing, promotions
# 3. seasonality and events e.g holidays, events

#methodology to incorporate external factors
# a. collect external data : integrate external datasets(GDP growth from government sources, inflation data)
# b. Feature engineering : creating new variables e.g "Inflation rate," and "Holiday indicator".
# c. Regular updates : continuously update the model with real-time external data.
# d. Multivariate Time-series modelling : use models like SARIMA, XGBOOST to predict sales while considering external factors.

# Merge external data (e.g., inflation rate)
sales_data <- merge(sales_data, external_factors, by = "Month-Year")

# Fit a multiple regression model
lm_model <- lm(Total_Value ~ Inflation_Rate + Competitor_Price + Holiday_Indicator, data = sales_data)

# Summary of model
summary(lm_model)

#Outcome : a better prediction model incorporating market conditions.

#Scalability : Handling a 10x larger dataset
#Optimizations:
#storage
# 1. use cloud-based databases(google BigQuery).
# 2. store data in efficient formats(parquet).

#Processing
#1. use parallel computing
#2. apply chunking and streaming for large files.
 
#Analysis
# 1. use SQL queries for aggregations before loading data into R.
# 2. Leverage precomputed tables to reduce computational load.

install.packages("data.table")
library(data.table)
# Load large dataset efficiently
large_data <- fread("big_sales_data.csv")

# Use parallel processing
library(parallel)
cl <- makeCluster(detectCores() - 1)
parLapply(cl, unique(large_data$`ANONYMIZED BUSINESS`), function(business) {
  subset(large_data, `ANONYMIZED BUSINESS` == business)
})
stopCluster(cl)

#Outcome : faster processing and analysis for large datasets.


















































































































