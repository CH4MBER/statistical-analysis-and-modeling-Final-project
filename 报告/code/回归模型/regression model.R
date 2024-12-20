library(ggplot2)
# 数据输入
dataset <- read.csv("data.csv")

# 数据处理
dataset$Surg.Med <- as.factor(dataset$Surg.Med)

# 1.单变量分布分析

# Age散点图
ggplot(dataset, aes(x = Age, y = Satisfaction)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Age与Satisfaction的关系", x = "Age", y = "Satisfaction") +
  theme_minimal()

# Severity散点图
ggplot(dataset, aes(x = Severity, y = Satisfaction)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Severity与Satisfaction的关系", x = "Severity", y = "Satisfaction") +
  theme_minimal()

# Surg.Med箱线图
ggplot(dataset, aes(x = Surg.Med, y = Satisfaction)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) + # 箱线图
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + # 添加均值点
  stat_summary(fun = mean, geom = "hline", aes(yintercept = after_stat(y)), linetype = "dashed", color = "red") + # 添加均值水平线
  labs(title = "Surg.Med与Satisfaction的关系", 
       x = "Surg.Med (0=内科, 1=外科)", 
       y = "Satisfaction") +
  theme_minimal()

# Anxiety散点图
ggplot(dataset, aes(x = Anxiety, y = Satisfaction)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Anxiety与Satisfaction的关系", x = "Anxiety", y = "Satisfaction") +
  theme_minimal()


# 2.数学建模
# 构建基础回归模型
model = lm(Satisfaction ~ Age + Severity + Surg.Med + Anxiety, data = dataset)
summary(model)

step_model=step(model) # 模型优化
summary(step_model)

# 3.模型诊断
plot(residuals(step_model), main = "残差分布")   # 残差分布图
abline(h = 0, col = "red")
qqnorm(residuals(step_model)) 
qqline(residuals(step_model), col = "red") # QQ 图检测残差正态性


# 可视化实际值 vs 预测值
dataset$predicted <- predict(step_model)

ggplot(dataset, aes(x = Satisfaction, y = predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  labs(title = "实际值 vs 预测值", x = "实际满意度", y = "预测满意度")+
  theme_minimal()


