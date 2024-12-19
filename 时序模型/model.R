library(tidyverse)
library(lubridate)
library(forecast)

data <- read.csv("sales.csv")
data$datesold <- as.Date(data$datesold, format = "%Y-%m-%d")
data <- data %>%
  filter(bedrooms > 0)

# 将 propertyType 转换为二进制变量 (0 或 1)
monthly_data <- monthly_data %>%
  mutate(propertyType_binary = ifelse(propertyType == "house", 0, 1))  # 'house' -> 0, 'unit' -> 1

# 创建外生变量矩阵，只包括 propertyType_binary 和 bedrooms
xreg <- cbind(propertyType_binary = monthly_data$propertyType_binary, bedrooms = monthly_data$bedrooms)

# 按时间排序
monthly_data <- monthly_data %>%
  arrange(month)

# 划分训练集和测试集的数据
train_data <- monthly_data[1:train_length, ]
test_data <- monthly_data[(train_length + 1):nrow(monthly_data), ]
train_xreg <- xreg[1:train_length, ]
test_xreg <- xreg[(train_length + 1):nrow(xreg), ]

train_prices <- train_data$price
test_prices <- test_data$price

fit <- auto.arima(train_prices, xreg = train_xreg, seasonal = TRUE)
summary(fit)


forecast_result <- forecast(fit, xreg = test_xreg, h = 12)
forecast_dates <- seq.Date(from = max(monthly_data$month) %m+% months(1), by = "month", length.out = 12)

actual_test_values <- test_prices
predicted_test_values <- forecast_result$mean

comparison_test_data <- data.frame(
  month = forecast_dates,
  actual = actual_test_values,
  predicted = predicted_test_values
)

ggplot(comparison_test_data, aes(x = month)) +
  geom_line(aes(y = actual, color = "Actual"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "测试集房价实际值与预测值对比图",
       x = "时间", y = "房价均值") +
  theme_minimal() +
  theme(legend.title = element_blank())



# 使用模型进行未来36个月的预测（未来3年）
forecast_period <- 36
forecast_result <- forecast(fit, xreg = test_xreg, h = forecast_period)

forecast_data <- data.frame(
  forecast_month = seq.Date(from = max(monthly_data$month) %m+% months(1), 
                            by = "month", length.out = forecast_period),
  forecast_price = as.numeric(forecast_result$mean)
)

ggplot(forecast_data, aes(x = forecast_month, y = forecast_price)) +
  geom_line(color = "red", size = 1) +
  labs(title = "未来3年房价预测",
       x = "时间", y = "房价均值") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months")



