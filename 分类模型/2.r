library(bnlearn)
# 读取数据
employee_data <- read.csv("employee.csv")
# 检查数据中的缺失值
if (sum(is.na(employee_data)) > 0) {
  # 删除包含缺失值的行
  employee_data <- na.omit(employee_data)
}
# 将所有列转换为因子型
employee_data <- as.data.frame(lapply(employee_data, factor))
# 构建贝叶斯网络结构
bn_structure <- hc(employee_data)
# 训练贝叶斯网络
bn_model <- bn.fit(bn_structure, data = employee_data)
# 预测与模型评估
predicted <- predict(bn_model, node = "LeaveOrNot", data = employee_data)
# 计算混淆矩阵
conf_matrix <- table(predicted, employee_data$LeaveOrNot)
# 计算模型的准确率
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
# 计算精确率、召回率、F1-Score（特别在数据不平衡时有用）
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)
# 输出模型评估结果
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1_score))