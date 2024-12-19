# 读取数据
employee_data <- read.csv("employee.csv")
# 删除 PaymentTier 或 ExperienceInCurrentDomain 列中有缺失值的行
employee_data <- subset(employee_data, !is.na(PaymentTier) & !is.na(PaymentTier))
# 使用原始经验年数（合并前）
contingency_table_raw <- table(employee_data$PaymentTier, employee_data$ExperienceInCurrentDomain)
# 进行卡方检验
chi_test_raw <- chisq.test(contingency_table_raw)
# 输出合并前的卡方检验结果
print("卡方检验结果（合并前）：")
print(chi_test_raw)
# 查看列联表频次
contingency_table_raw <- table(employee_data$PaymentTier, employee_data$ExperienceInCurrentDomain)
print(contingency_table_raw)
# 确保领域经验年数为数字类型
employee_data$ExperienceInCurrentDomain <- as.numeric(as.character(employee_data$ExperienceInCurrentDomain))
# 合并经验年数
employee_data$ExperienceGroup <- cut(employee_data$ExperienceInCurrentDomain,
                                      breaks = c(-Inf, 0, 1, 2, 3, 4, Inf),
                                      labels = c("0 years", "1 year", "2 years", "3 years", "4 years", "4+ years"))
# 使用合并后的经验年数（合并后）
contingency_table_merged <- table(employee_data$PaymentTier, employee_data$ExperienceGroup)
# 进行卡方检验
chi_test_merged <- chisq.test(contingency_table_merged)
# 输出合并后的卡方检验结果
print("卡方检验结果（合并后）：")
print(chi_test_merged)