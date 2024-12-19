library(tidyverse)
library(caret)
library(glmnet)
# 读取数据
employee_data <- read.csv("employee.csv")
# 检查数据中的缺失值
if (sum(is.na(employee_data)) > 0) {  # 删除包含缺失值的行
  employee_data <- na.omit(employee_data)
}
# 将所有列转换为因子型
employee_data <- as.data.frame(lapply(employee_data, factor))
# 划分训练集和测试集（80%训练，20%测试）
set.seed(821)  # 设置随机种子确保结果可重现
trainIndex <- createDataPartition(employee_data$LeaveOrNot, p = 0.8, list = FALSE)
train_data <- employee_data[trainIndex, ]
test_data <- employee_data[-trainIndex, ]
# 构建逻辑回归模型
logit_model <- glm(LeaveOrNot ~ Education + City + PaymentTier + Gender + EverBenched + ExperienceInCurrentDomain + JoiningYear + Age,
                   data = train_data, 
                   family = binomial(link = "logit"))
# 查看模型总结
print(summary(logit_model))
# 计算训练集和测试集的预测概率
train_pred_prob <- predict(logit_model, train_data, type = "response")
test_pred_prob <- predict(logit_model, test_data, type = "response")
# 将概率转换为预测类别（例如，0.5为阈值）
train_pred_class <- ifelse(train_pred_prob > 0.5, 1, 0)
test_pred_class <- ifelse(test_pred_prob > 0.5, 1, 0)
# 训练集评估
train_conf_matrix <- table(Predicted = train_pred_class, Actual = train_data$LeaveOrNot)
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)
train_precision <- train_conf_matrix[2,2] / sum(train_conf_matrix[2,])
train_recall <- train_conf_matrix[2,2] / sum(train_conf_matrix[,2])
train_f1 <- 2 * (train_precision * train_recall) / (train_precision + train_recall)
# 测试集评估
test_conf_matrix <- table(Predicted = test_pred_class, Actual = test_data$LeaveOrNot)
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_precision <- test_conf_matrix[2,2] / sum(test_conf_matrix[2,])
test_recall <- test_conf_matrix[2,2] / sum(test_conf_matrix[,2])
test_f1 <- 2 * (test_precision * test_recall) / (test_precision + test_recall)
# 输出评估结果
cat("训练集准确率:", train_accuracy, "\n")
cat("训练集精确率:", train_precision, "\n")
cat("训练集召回率:", train_recall, "\n")
cat("训练集F1分数:", train_f1, "\n")
cat("测试集准确率:", test_accuracy, "\n")
cat("测试集精确率:", test_precision, "\n")
cat("测试集召回率:", test_recall, "\n")
cat("测试集F1分数:", test_f1, "\n")
