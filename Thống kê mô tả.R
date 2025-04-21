
# Phân tích biến định lượng
summary(final_df[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Pixel_Rate", "TMUs", "Max_Power")])

#Phân tích biến định tính
table(final_df$Manufacturer)
table(final_df$Memory_Type)

# Áp dụng logarithm cho Pixel Rate
final_df$Pixel_Rate_log <- log(final_df$Pixel_Rate + 1)

library(nortest)
# Kiểm định độ lệch và độ nhọn của Pixel Rate
ad.test(final_df$Pixel_Rate)

# Kiểm định độ lệch và độ nhọn của Pixel Rate logarithm
ad.test(final_df$Pixel_Rate_log)

# Áp dụng Logarithm
final_df$Memory_log <- log(final_df$Memory + 1)
final_df$Memory_Speed_log <- log(final_df$Memory_Speed + 1)
final_df$Memory_Bandwidth_log <- log(final_df$Memory_Bandwidth + 1)
final_df$TMUs_log <- log(final_df$TMUs + 1)
final_df$Max_Power_log <- log(final_df$Max_Power + 1)

# Vẽ Histogram của Pixel_rate và Pixel_rate_log
library("ggplot2")
ggplot(final_df, aes(x = Pixel_Rate)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Pixel Rate", x = "Pixel_Rate(GPixel/s)", y = "Frequency") +
  theme_minimal()

ggplot(final_df, aes(x = Pixel_Rate_log)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.09, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Logarithm Pixel Rate", x = "GPixel/s", y = "Frequency") +
  theme_minimal()

# Boxplot của Pixel_rate và biến định tính
ggplot(final_df, aes(x = Manufacturer, y = Pixel_Rate, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "Boxplot của Pixel Rate theo Manufacturer",
       x = "Manufacturer", y = "GPixel/s") +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

ggplot(final_df, aes(x = Manufacturer, y = Pixel_Rate_log, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "Boxplot của Pixel Rate log theo Manufacturer",
       x = "Manufacturer", y = "GPixel/s") +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
ggplot(final_df, aes(x = Memory_Type, y = Pixel_Rate, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "Boxplot của Pixel Rate theo Memory_Typer",
       x = "Memory_Type", y = "GPixel/s") +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
    
ggplot(final_df, aes(x = Memory_Type, y = Pixel_Rate_log, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "Boxplot của Pixel Rate log theo Memory_Type",
       x = "Memory_Type", y = "GPixel/s") +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Vẽ histogram cho 5 biến định lượng còn lại
# Histogram of Memory
ggplot(final_df, aes(x = Memory)) +
  geom_histogram(aes(y = ..density..), binwidth = 430, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Memory",
       x = "Memory",
       y = "Density") +
  theme_minimal()

jarque.bera.test(final_df$Memory)

# Histogram of Memory_Bandwidth
ggplot(final_df, aes(x = Memory_Bandwidth)) +
  geom_histogram(aes(y = ..density..), binwidth = 90, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Memory_Bandwidth",
       x = "Memory_Bandwidth",
       y = "Density") +
  theme_minimal()

jarque.bera.test(final_df$Memory_Bandwidth)

# Histogram of Memory_Speed
ggplot(final_df, aes(x = Memory_Speed)) +
  geom_histogram(aes(y = ..density..), binwidth = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Memory_Speed",
       x = "Memory_Speed",
       y = "Density") +
  theme_minimal()

jarque.bera.test(final_df$Memory_Speed)

# Histogram of Max Power
ggplot(final_df, aes(x = Max_Power)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Max Power",
       x = "Max Power",
       y = "Density") +
  theme_minimal()

# Histogram of Texture Rate
ggplot(final_df, aes(x = TMUs)) +
  geom_histogram(aes(y = ..density..), binwidth = 6, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of TMUs",
       x = "TMUs",
       y = "Density") +
  theme_minimal()

# Scatter của các biến định lượng và Pixel_rate
ggplot(final_df, aes(x = Max_Power, y = Pixel_Rate)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Max Power và Pixel Rate",
       x = "Max Power",
       y = "Pixel Rate") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Max_Power_log, y = Pixel_Rate_log)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Max Power log và Pixel Rate log",
       x = "Max Power log",
       y = "Pixel Rate log") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = TMUs, y = Pixel_Rate)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến TMUs và Pixel Rate",
       x = "TMUs",
       y = "Pixel Rate") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = TMUs_log, y = Pixel_Rate_log)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến TMUs log và Pixel Rate log",
       x = "TMUs",
       y = "Pixel Rate log") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Memory, y = Pixel_Rate)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Memory và Pixel Rate",
       x = "Memory",
       y = "Pixel Rate") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Memory_log, y = Pixel_Rate_log)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Memory log và Pixel Rate log",
       x = "Memory log",
       y = "Pixel Rate log") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Memory_Bandwidth, y = Pixel_Rate)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Memory_Bandwidth và Pixel Rate",
       x = "Memory",
       y = "Pixel Rate") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Memory_Bandwidth_log, y = Pixel_Rate_log)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Memory Bandwidth log và Pixel Rate log",
       x = "Memory_Bandwidth_log",
       y = "Pixel Rate log") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Memory_Speed, y = Pixel_Rate)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Memory_Speed và Pixel Rate",
       x = "Memory_Speed",
       y = "Pixel Rate") +
  theme_minimal()  # Sử dụng theme nền tối giản

ggplot(final_df, aes(x = Memory_Speed_log, y = Pixel_Rate_log)) +
  geom_point(color = "skyblue", size = 2) +   # Vẽ điểm: màu đỏ, kích thước 2
  labs(title = "Biểu đồ Scatter của biến Memory Speed log và Pixel Rate log",
       x = "Memory Speed log",
       y = "Pixel Rate log") +
  theme_minimal()  # Sử dụng theme nền tối giản

# Chọn biến để vẽ ma trận tương quan
Column <- c("Memory", "Memory_Bandwidth", "Memory_Speed", "TMUs", "Max_Power", "Pixel_Rate")

subset_df_final <- final_df[, Column]
cor_matrix <- cor(subset_df_final)

# Vẽ ma trận tương quan
corrplot(cor_matrix, method = "circle", addCoef.col = "white", number.digits = 2)
