# Data Analysis Case Study

## Overview
This project involves data cleaning, exploratory data analysis (EDA), and advanced analytics on sales data. The objective is to extract insights, visualize trends, and perform forecasting.

## Table of Contents
- [Installation](#installation)
- [Data Cleaning and Preparation](#data-cleaning-and-preparation)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Advanced Analysis](#advanced-analysis)
- [Results and Insights](#results-and-insights)
- [Forecasting](#forecasting)

## Installation
Ensure you have R installed on your system. The following libraries are required:

```r
install.packages("readxl") # for reading Excel files
install.packages("dplyr") # for data manipulation
install.packages("lubridate") # for handling dates
install.packages("ggplot2") # for visualizations
install.packages("forecast") # for time series forecasting
```

Load the libraries:

```r
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
```

## Data Cleaning and Preparation
- Load the dataset:

```r
data <- read_excel("Case Study Data.xlsx", sheet = "case_study_data_2025-01-16T06_4")
```

- Check for missing values and duplicates:

```r
colSums(is.na(data))
sum(duplicated(data))
```

- Remove duplicates and missing values:

```r
data_cleaned <- data %>% filter(!is.na('UNIT PRICE')) %>% distinct()
```

- Add a `Month_Year` column:

```r
data_cleaned <- data_cleaned %>% mutate(Month_Year = format(DATE, "%B %Y"))
```

- Save the cleaned dataset:

```r
write.csv(data_cleaned, "Cleaned_Data.csv", row.names = FALSE)
```

## Exploratory Data Analysis
### Sales Overview
- Sales by Category:

```r
sales_by_category <- data_cleaned %>%
  group_by(`Anonymized Category`) %>%
  summarise(Total_Quantity = sum(Quantity), Total_Value = sum(Value))
```

- Sales by Business:

```r
sales_by_business <- data_cleaned %>%
  group_by(`Anonymized Business`) %>%
  summarise(Total_Quantity = sum(Quantity), Total_Value = sum(Value))
```

- Visualizations:

```r
ggplot(sales_by_category, aes(x = `Anonymized Category`, y = Total_Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Value by Category")
```

```r
ggplot(sales_by_business, aes(x = `Anonymized Business`, y = Total_Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Value by Business")
```

### Trends Over Time

```r
sales_by_month_year <- data_cleaned %>%
  group_by(Month_Year) %>%
  summarise(Total_Quantity = sum(Quantity), Total_Value = sum(Value))
```

```r
ggplot(sales_by_month_year, aes(x = as.Date(paste0(Month_Year, "-01"), format = "%B %Y-%d"), y = Total_Value)) +
  geom_line() +
  labs(title = "Sales Trend Over Time", x = "Month-Year", y = "Total Value")
```

### Performance Analysis
- Top 5 products by quantity and value:

```r
top_5_products_quantity <- data_cleaned %>%
  group_by(`Anonymized Product`) %>%
  summarise(Total_Quantity = sum(Quantity)) %>%
  arrange(desc(Total_Quantity)) %>%
  top_n(5, Total_Quantity)
```

```r
top_5_products_value <- data_cleaned %>%
  group_by(`Anonymized Product`) %>%
  summarise(Total_Value = sum(Value)) %>%
  arrange(desc(Total_Value)) %>%
  top_n(5, Total_Value)
```

## Advanced Analysis
### Customer Segmentation

```r
business_segments <- data_cleaned %>%
  group_by(`Anonymized Business`) %>%
  summarise(Total_Quantity = sum(Quantity),
            Total_Value = sum(Value),
            Transaction_Count = n())

set.seed(123)
kmeans_result <- kmeans(business_segments[, c("Total_Quantity", "Total_Value", "Transaction_Count")], 3)
business_segments$Segment <- kmeans_result$cluster
```

### Forecasting
Convert sales data to a time series object and apply ARIMA forecasting:

```r
sales_ts <- ts(sales_by_month_year$Total_Value, frequency = 12)
arima_model <- auto.arima(sales_ts)
forecast_values <- forecast(arima_model, h = 3)
plot(forecast_values)
```

## Results and Insights
- Certain categories and businesses generate higher sales.
- Sales trends fluctuate seasonally.
- Top-performing products in terms of quantity differ from those by value.
- Customer segmentation helps identify different business behaviors.
- Forecasting provides a predictive outlook on sales performance.

## Contributing
Feel free to fork this repository, create feature branches, and submit pull requests for improvements.

