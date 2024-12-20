library(dplyr)
library(lubridate)

data <- read.csv("sales.csv")

data$datesold <- as.Date(data$datesold, format="%Y-%m-%d")

data <- data %>%
  filter(bedrooms > 0)

data$year_month <- format(data$datesold, "%Y-%m")

library(ggplot2)

# 按月份和房屋类型分组，计算每组的平均价格
monthly_avg_price <- data %>%
  group_by(year_month, propertyType) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

# 将 year_month 转换为日期格式，使用每月的第一天
monthly_avg_price$year_month <- as.Date(paste0(monthly_avg_price$year_month, "-01"), format="%Y-%m-%d")

# 查看转换后的数据
head(monthly_avg_price)

# 使用 ggplot2 进行数据可视化
ggplot(monthly_avg_price, aes(x = year_month, y = avg_price, color = propertyType)) +
  geom_line(size = 1) +  # 使用折线图
  labs(title = "每月不同类型房屋平均价格趋势",
       x = "日期",
       y = "平均房价") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 按月份、卧室数量分组，计算每组的平均价格
monthly_avg_price <- data %>%
  group_by(year_month, bedrooms) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

# 将 year_month 转换为日期格式，使用每月的第一天
monthly_avg_price$year_month <- as.Date(paste0(monthly_avg_price$year_month, "-01"), format="%Y-%m-%d")

# 查看转换后的数据（可选）
head(monthly_avg_price)

# 使用 ggplot2 进行数据可视化
ggplot(monthly_avg_price, aes(x = year_month, y = avg_price, color = factor(bedrooms))) +
  geom_line(size = 1) +
  labs(title = "每月不同卧室数量的房屋平均价格趋势",
       x = "日期",
       y = "平均房价") +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




library(dplyr)
library(tseries)

# 对每种房间数量进行 Ljung-Box 检验
ljung_box_results <- data %>%
  group_by(bedrooms) %>%
  summarise(
    ljung_box_test = list(Box.test(price, lag = 20, type = "Ljung-Box")),
    .groups = "drop"
  )

# 提取每种房间数量 Ljung-Box 检验的 p 值
ljung_box_results$p_value <- sapply(ljung_box_results$ljung_box_test, function(test) test$p.value)
print(ljung_box_results)



# 加载必要的包
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

# 假设数据已加载，并且数据框名为 'data'，包含 'price' 和 'datesold' 列

# 将日期转换为 Date 类型
data$datesold <- as.Date(data$datesold, format="%Y-%m-%d")

# 按日期升序排序数据
data <- data %>% arrange(datesold)

# 将最后 12 个月的数据作为测试集
last_12_months <- data %>%
  filter(datesold >= max(datesold) - months(12))  # 获取最后 12 个月的数据

# 训练集：除了最后 12 个月的数据
train_data <- data %>%
  filter(datesold < max(datesold) - months(12))

# 提取训练集数据并按月聚合计算平均价格
train_avg_price <- train_data %>%
  group_by(year_month = format(datesold, "%Y-%m")) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# 将训练集数据转换为时间序列格式
ts_train_data <- ts(train_avg_price$avg_price, frequency = 12, start = c(year(min(train_data$datesold)), month(min(train_data$datesold))))

# 拟合 ARIMA 模型
arima_model <- auto.arima(ts_train_data)

# 查看 ARIMA 模型的摘要
summary(arima_model)

# 提取测试集的时间信息
test_avg_price <- last_12_months %>%
  group_by(year_month = format(datesold, "%Y-%m")) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# 将测试集数据转换为时间序列
ts_test_data <- ts(test_avg_price$avg_price, frequency = 12, start = c(year(min(last_12_months$datesold)), month(min(last_12_months$datesold))))

# 进行对测试集的预测
forecast_result <- forecast(arima_model, h = 12)

# 查看预测结果
print(forecast_result)



# 获取预测的时间戳
forecast_time <- time(forecast_result$mean)

# 计算预测开始的时间，使用训练集的时间序列的结束时间
last_train_date <- time(ts_train_data)[length(ts_train_data)]

# 使用 last_train_date + forecast_time 计算预测时间
forecast_months <- seq(from = last_train_date + 1, by = 1, length.out = length(forecast_time))

# 将 forecast_months 转换为日期格式
forecast_dates <- as.Date(forecast_months, origin = "1970-01-01")

# 提取预测的房价和真实的测试集房价
forecast_data <- data.frame(
  forecast_month = forecast_dates,          # 预测的月份
  forecast_price = as.numeric(forecast_result$mean),  # 预测的价格
  actual_price = ts_test_data               # 测试集的实际房价
)

# 查看预测结果和实际房价的对比
head(forecast_data)

# 合并预测结果和实际房价
combined_data <- data.frame(
  month = forecast_data$forecast_month,
  forecast_price = forecast_data$forecast_price,
  actual_price = forecast_data$actual_price
)

# 可视化预测结果与实际结果
ggplot(combined_data, aes(x = month)) +
  geom_line(aes(y = forecast_price, color = "Forecast"), size = 1) +
  geom_line(aes(y = actual_price, color = "Actual"), size = 1) +
  labs(title = "房屋价格预测 vs 实际房价", x = "日期", y = "房价") +
  scale_color_manual(values = c("Forecast" = "blue", "Actual" = "red")) +
  theme_minimal()

