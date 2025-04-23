#Cài library
# install.packages("nortest")
# install.packages("ggplot2")
library(nortest)   # Để dùng hàm `lillie.test()` cho kiểm định phân phối chuẩn theo Lilliefors
library(ggplot2)   # Để vẽ biểu đồ histogram với `ggplot()`


# Đặt thư mục làm việc và đọc dữ liệu
setwd("your_direction")
GPUs <- read.csv("./GPU_cleaned.csv")

nvidia <- GPUs$Pixel_Rate[GPUs$Manufacturer == "Nvidia"]
print(lillie.test(nvidia))

amd <- GPUs$Pixel_Rate[GPUs$Manufacturer == "AMD"]
print(lillie.test(amd))

z_alpha <- qnorm(1 - 0.05)
# z_alpha = 1.644854

mean_nvidia <- mean(nvidia)
mean_amd <- mean(amd)
s1 <- sd(nvidia)
s2 <- sd(amd)
n1 <- length(nvidia)
n2 <- length(amd)
z_qs <- (mean_nvidia - mean_amd) / sqrt(s1^2 / n1 + s2^2 / n2)
print(z_qs)

nvidia_data <- GPUs[GPUs$Manufacturer == "Nvidia", ]
amd_data <- GPUs[GPUs$Manufacturer == "AMD", ]

ggplot() +
  geom_density(data = nvidia_data, aes(x = Pixel_Rate, fill = "Nvidia"), alpha = 0.5) +
  geom_density(data = amd_data, aes(x = Pixel_Rate, fill = "AMD"), alpha = 0.5) +
  scale_fill_manual(values = c("Nvidia" = "green", "AMD" = "orange")) +
  labs(title = "Phân phối Pixel_Rate giữa Nvidia và AMD",
       x = "Pixel Rate (GPixel/s)", y = "Mật độ", fill = "Hãng GPU") +
  theme_minimal()



var.test(nvidia, amd, alternative = "greater")

result <- t.test(nvidia, amd, var.equal = FALSE, alternative = "greater")
print(result)

df <- result$parameter  # bậc tự do được tính bởi Welch
t_crit <- qt(1 - alpha, df)
cat("Giá trị tới hạn (t_critical) tại mức ý nghĩa 0.05:", t_crit, "\n")

