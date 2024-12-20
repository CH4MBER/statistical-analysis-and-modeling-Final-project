# 读取数据
employee_data <- read.csv("employee.csv")
# 确保 JoiningYear 列是数字类型
employee_data$JoiningYear <- as.numeric(as.character(employee_data$JoiningYear))
# 过滤非合理年份数据
employee_data <- subset(employee_data, !is.na(JoiningYear) & JoiningYear <= 2024)
# 计算服务年限
employee_data$ServiceYears <- 2024 - employee_data$JoiningYear
# 可视化
png("service_years_by_city.png", width = 800, height = 600)  # 设置文件名和图像大小
# 绘制箱线图
boxplot(ServiceYears ~ City, data = employee_data, main = "Service Years by City", col = "lightgreen")
# 关闭图形设备，保存文件
dev.off()
# 执行方差分析
anova_result <- aov(ServiceYears ~ City, data = employee_data)
# 显示ANOVA检验结果
print(summary(anova_result))
