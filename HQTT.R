# Đọc dữ liệu
data <- read.csv("C:/Users/Lenovo/OneDrive/Máy tính/BTL XSTK/data.csv")

# Lấy biến và tính các đặc trưng cơ bản
final_df <- (data[, c("Memory_log", "Memory_Bandwidth_log", "Memory_Speed_log", "Pixel_Rate_log", "TMUs_log", "Max_Power_log")])
summary(final_df[, c("Memory_log", "Memory_Bandwidth_log", "Memory_Speed_log", "Pixel_Rate_log", "TMUs_log", "Max_Power_log")])

# Mô hình hồi quy tuyến tính bội
library(car)
model <- lm(Pixel_Rate_log ~ Memory_log + Memory_Bandwidth_log + Memory_Speed_log + TMUs_log + Max_Power_log, data = final_df)
summary(model)  # Xem hệ số hồi quy và thống kê liên quan

# Kiểm tra đa cộng tuyến 
vif(model)  # Nếu VIF > 5 hoặc 10 thì có đa cộng tuyến mạnh

# Kiểm định phân phối chuẩn của phần dư
res <- residuals(model)
# Q-Q
qqnorm(res)
qqline(res, col = "red", lwd = 1.5)
# Shapiro-Wisk test
shapiro.test(residuals(model))

# Đồng nhất phương sai (Homoscedasticity)
library(lmtest)
#Kiểm định Breusch-Pagan
bptest(model)

# Không có tự tương quan (Independence of errors)
# Kiểm định Durbin-Watson
durbinWatsonTest(model)

# R-squared & R-squared adj
summary(model)$r.squared
summary(model)$adj.r.squared

# RMSE
sqrt(mean(residuals(model)^2))

# Dự đoán trên dữ liệu đã được logarit hóa
predicted <- predict(model)

# Bảng so sánh
comparison <- data.frame(
  Actual = final_df$Pixel_Rate_log,
  Predicted = predicted,
  Residuals = final_df$Pixel_Rate_log - predicted
)

# Đánh giá sai số
rmse <- sqrt(mean((comparison$Residuals)^2))
mae <- mean(abs(comparison$Residuals))
mean_actual <- mean(comparison$Actual)
mape <- mean(abs((comparison$Actual - comparison$Predicted)/comparison$Actual)) * 100

# Biểu đồ so sánh giá trị thực tế và dự đoán (log)
par(xpd = TRUE, mar = c(5, 6, 4, 8))  # Tăng lề phải để đặt legend bên ngoài
plot(comparison$Actual, type = "l", col = "lightblue", lwd = 2,
     ylab = "Pixel_Rate_log", main = "Actual vs Predicted (Log scale)")
lines(comparison$Predicted, col = "salmon", lwd = 2)
legend("topright", inset = c(-0.35, 0.5),  # Legend ra ngoài bên phải
       legend = c("Actual", "Predicted"), col = c("lightblue", "salmon"),
       lwd = 2, bty = "n", cex = 0.8)

# Dự đoán trên dự liệu original
predicted_log <- predict(model)
actual_original <- exp(final_df$Pixel_Rate_log) - 1
predicted_original <- exp(predicted_log) - 1

# Tạo bảng so sánh
comparison_original <- data.frame(
  Actual = actual_original,
  Predicted = predicted_original,
  Residuals = actual_original - predicted_original
)

# Biểu đồ so sánh (thang đo gốc)
par(xpd = TRUE, mar = c(5, 6, 4, 8))  # Reset lại margin nếu cần
plot(comparison_original$Actual, type = "l", col = "lightblue", lwd = 2,
     ylab = "Pixel_Rate", main = "Actual vs Predicted (Original scale)")
lines(comparison_original$Predicted, col = "salmon", lwd = 2)
legend("topright", inset = c(-0.35, 0.5),  # Legend ra ngoài bên phải
       legend = c("Actual", "Predicted"), col = c("lightblue", "salmon"),
       lwd = 2, bty = "n", cex = 0.8)

### Tìm khoảng tin cậy Y ###
set.seed(as.numeric(Sys.time()))

# Lấy ngẫu nhiên 30 dòng dữ liệu
sample_index <- sample(1:nrow(final_df), 30)
sample_data <- final_df[sample_index, ]

# Dự đoán với khoảng CI (độ tin cậy 95%)
ci_pred <- predict(model, newdata = sample_data, interval = "confidence")
# Dự đoán với khoảng PI (độ tin cậy 95%)
pi_pred <- predict(model, newdata = sample_data, interval = "prediction")

# Thực tế và dự đoán 
actual_vals <- sample_data$Pixel_Rate_log

# In ra màn hình
cat("=== Giá trị thực tế và khoảng ước lượng (CI) ===\n")
for (i in 1:30) {
  cat(sprintf("Obs %2d | Actual: %.3f | CI: [%.3f - %.3f] | Fit: %.3f\n", 
              i, actual_vals[i], ci_pred[i, "lwr"], ci_pred[i, "upr"], ci_pred[i, "fit"]))
}

cat("\n=== Giá trị thực tế và khoảng dự báo (PI) ===\n")
for (i in 1:30) {
  cat(sprintf("Obs %2d | Actual: %.3f | PI: [%.3f - %.3f] | Fit: %.3f\n", 
              i, actual_vals[i], pi_pred[i, "lwr"], pi_pred[i, "upr"], pi_pred[i, "fit"]))
}

# Tạo dataframe cho biểu đồ
ci_df <- data.frame(
  Index = 1:30,
  Actual = actual_vals,
  Fit = ci_pred[, "fit"],
  Lower = ci_pred[, "lwr"],
  Upper = ci_pred[, "upr"]
)

pi_df <- data.frame(
  Index = 1:30,
  Actual = actual_vals,
  Fit = pi_pred[, "fit"],
  Lower = pi_pred[, "lwr"],
  Upper = pi_pred[, "upr"]
)

# Vẽ biểu đồ
library(ggplot2)
library(gridExtra)

# CI Plot
p1 <- ggplot(ci_df, aes(x = Index)) +
  geom_point(aes(y = Fit), color = "seagreen", shape = 3, size = 2.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "mediumseagreen") +
  geom_point(aes(y = Actual), color = "indianred", size = 2.5) +
  scale_x_continuous(breaks = seq(0, max(ci_df$Index), by = 5)) +
  labs(
    title = "Confidence Interval for Mean Prediction",
    y = "Pixel_Rate_log",
    x = "Observation Index"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# PI Plot
p2 <- ggplot(pi_df, aes(x = Index)) +
  geom_point(aes(y = Fit), color = "seagreen", shape = 3, size = 2.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "mediumseagreen") +
  geom_point(aes(y = Actual), color = "indianred", size = 2.5) +
  scale_x_continuous(breaks = seq(0, max(pi_df$Index), by = 5)) +
  labs(
    title = "Prediction Interval for Future Observation",
    y = "Pixel_Rate_log",
    x = "Observation Index"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

print(p1)
print(p2)


