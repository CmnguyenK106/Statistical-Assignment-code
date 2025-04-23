#Cài library
#install.packages("nortest")
#install.packages("car")
#install.packages("dplyr")
library(nortest)  # Dùng cho kiểm định Lilliefors (Kolmogorov–Smirnov) thông qua hàm `lillie.test()`
library(car)      # Dùng cho kiểm định phương sai đồng nhất với hàm `leveneTest()`
library(dplyr)    # Dùng cho thao tác với dữ liệu như lọc nhóm có mẫu > 30

# Bước 1: Đọc dữ liệu
library(nortest)
setwd("your_direction")
df <- read.csv("./GPU_cleaned.csv")

# Bước 2: Tiền xử lý
Memory_Type <- as.factor(df$Memory_Type)
Manufacturer <- as.factor(df$Manufacturer)
Pixel_Rate <- df$Pixel_Rate
alpha <- 0.05 #Mức ý nghĩa 5%

# Bước 3: Kiểm định phân phối chuẩn
# =======================================================
# PHẦN A: KIỂM ĐỊNH PHÂN PHỐI CHUẨN VỚI PIXEL_RATE GỐC
# =======================================================

cat("===== KIỂM TRA PHÂN PHỐI CHUẨN VỚI PIXEL_RATE GỐC =====\n")
groups <- split(df$Pixel_Rate, df$Manufacturer)
groups <- groups[sapply(groups, function(x) length(unique(x))) > 1]

for (i in 1:length(groups)) {
  cat("Nhóm", names(groups)[i], "\n")
  print(lillie.test(groups[[i]]))
  qqnorm(groups[[i]], main = paste("Q-Q Plot:", names(groups)[i]))
  qqline(groups[[i]])
}

# =======================================================
# PHẦN B: KIỂM ĐỊNH PHÂN PHỐI CHUẨN VỚI PIXEL_RATE ĐÃ LOG
# =======================================================

# Bước log-transform
df$Pixel_Rate_log <- log(df$Pixel_Rate + 1)

cat("\n===== KIỂM TRA PHÂN PHỐI CHUẨN SAU LOG PIXEL_RATE =====\n")
log_groups <- split(df$Pixel_Rate_log, df$Manufacturer)
log_groups <- log_groups[sapply(log_groups, function(x) length(unique(x))) > 1]

for (i in 1:length(log_groups)) {
  cat("Nhóm", names(log_groups)[i], "(log)\n")
  print(lillie.test(log_groups[[i]]))
  qqnorm(log_groups[[i]], main = paste("Q-Q Plot (log):", names(log_groups)[i]))
  qqline(log_groups[[i]])
}

# Bước 4: Kiểm định phương sai đồng nhất
library(car)
leveneTest(Pixel_Rate ~ Manufacturer, data = df)

# Bước 5: Lọc nhóm có mẫu > 30
library(dplyr)
filtered_data <- df %>%
  group_by(Manufacturer) %>%
  filter(n() > 30) %>%
  ungroup()

filtered_data %>%
  group_by(Manufacturer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Bước 6: Phân tích ANOVA
av <- aov(Pixel_Rate ~ Manufacturer, data = filtered_data)
anova_result <- summary(av)
print(anova_result)

# Tính miền bác bỏ và đưa ra kết luận
# --------------------------------------
# Lấy giá trị F quan sát và bậc tự do từ kết quả ANOVA
F_observed <- anova_result[[1]]$`F value`[1]
df1 <- anova_result[[1]]$Df[1]      # bậc tự do giữa các nhóm
df2 <- anova_result[[1]]$Df[2]      # bậc tự do trong nhóm

F_critical <- qf(1 - alpha, df1, df2)

print(F_critical)

cat("\n===== Miền bác bỏ giả thuyết H0 =====\n")
cat("F quan sát =", F_observed, "\n")
cat("F tới hạn ở mức ý nghĩa 0.05 =", F_critical, "\n")


# Bước 7: Hậu kiểm Tukey HSD
TukeyHSD(av)
