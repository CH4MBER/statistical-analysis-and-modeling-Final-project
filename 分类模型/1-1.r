# 读取数据
employee_data <- read.csv("employee.csv")
# 删除 Education 列中有缺失值的行
employee_data <- employee_data[!is.na(employee_data$Education), ]
# 统计教育背景数量
education_distribution <- table(employee_data$Education)
# 计算比例
education_proportions <- round(education_distribution / sum(education_distribution) * 100,3)
# 拼接比例文本
labels_with_proportions <- paste(names(education_distribution), "(", education_proportions, "%)", sep = "")
# 保存横向布局图像
png("education_distribution_layout.png", width = 1200, height = 600)
# 设置图形窗口为1行2列并设置边距
par(mfrow = c(1, 2), mar = c(4, 4, 4, 2))  
# 1. 柱状图
barplot_heights <- barplot(education_distribution, 
                           main = "Education Background Distribution Bar Chart", 
                           col = "lightblue", ylim = c(0, max(education_distribution) * 1.2))
text(x = barplot_heights, 
     y = education_distribution, 
     labels = education_distribution, 
     pos = 3, cex = 0.8, col = "blue")
# 2. 饼状图
pie(education_distribution, 
    main = "Education Background Distribution Pie Chart", 
    labels = labels_with_proportions, 
    col = rainbow(length(education_distribution)), 
    cex = 1.2) 
# 关闭图形设备
dev.off()
