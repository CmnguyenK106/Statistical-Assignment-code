#Cài library
# install.packages("nortest")
# install.packages("ggplot2")
library(nortest)   # Để dùng hàm `lillie.test()` cho kiểm định phân phối chuẩn theo Lilliefors
library(ggplot2)   # Để vẽ biểu đồ histogram với `ggplot()`

# Đặt thư mục làm việc và đọc dữ liệu
setwd("your_direction")
GPUs <- read.csv("./GPU_cleaned.csv")

# Kiểm định Lilliefors (Kolmogorov–Smirnov) để kiểm tra phân phối chuẩn
lillie_test <- lillie.test(GPUs$Pixel_Rate)
print(lillie_test)

# Vẽ biểu đồ QQ plot
qqnorm(GPUs$Pixel_Rate)
qqline(GPUs$Pixel_Rate, col = "red")

# Tính giá trị kiểm định Z
z_alpha_2 <- qnorm(1 - 0.05/2)  # ≈ 1.96

xtb <- mean(GPUs$Pixel_Rate)
n <- length(GPUs$Pixel_Rate)
s <- sd(GPUs$Pixel_Rate)
z_qs <- (xtb - 15) / (s / sqrt(n))
print(z_qs)

# Cài và nạp ggplot2 (chỉ cài một lần)
# install.packages("ggplot2")
library(ggplot2)

# Vẽ biểu đồ histogram của Pixel_Rate
ggplot(GPUs, aes(x = Pixel_Rate)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.05) +
  geom_vline(xintercept = 15, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Pixel_Rate trong GPUs",
    x = "Pixel Rate (GPixel/s)",
    y = "Tần số"
  ) +
  theme_minimal()
