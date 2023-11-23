#=============================================================
# file: main.r
# Author: Dang Cao Minh
# Comment language: Vietnamese
# Language: R
# Date modified: 20/11/2023
# Dataset file: data.csv
# Contributors:
#               Đoàn Minh Khôi
#               Hồ Cảnh Minh
#               Vũ Hoàng Quân
#               Lê Châu Nhật Minh
# Brief:
#    - This is part of our probability and statistics project
#    - The aim of the study is to determine how much of the
#    adjustment parameters in 3d printers affect the print
#    quality, accuracy, strength. Where there are nine setting
#    parameters, three measured output parameters.
#    - Content
#       + Setting Parameters:
#           Layer Height (mm)
#           Wall Thickness (mm)
#           Infill Density (%)
#           Infill Pattern ()
#           Nozzle Temperature (Cº)
#           Bed Temperature (Cº)
#           Print Speed (mm/s)
#           Material ()
#           Fan Speed (%)
#       + Output Parameters: (Measured)
#           Roughness (µm)
#           Tension (ultimate) Strenght (MPa)
#           Elongation (%)


#=============================================================
#=============================================================
#--============= USER-DEFINED FUNCTION =======================
#=============================================================
#=============================================================

#-------------------------------------------------------------
# Chuyển kiểu dữ liệu data frame có một cột sang array
# Giá trị nhập vào: data frame chỉ có một cột
# Giá trị trả về: array
# Độ phức tạp: O(x)
df_to_array <- function(datainput) {
  return(array(unlist(datainput)))
}

#-------------------------------------------------------------
# Hàm lấy tên của các cột
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(y)
get_col_name <- function(datainput) {
  col_names <- colnames(datainput)
  return(col_names)
}

#-------------------------------------------------------------
# Nhập vào hai vector a và b độ dài n
# Thay tất cả giá trị a[i] trong data frame
# thành b[i] với mọi i
# Giá trị nhập vào: data frame, hai vector
# Giá trị trả về: data frame
# Độ phức tạp: O(xyn)
replace_value <- function(datainput, a, b) {
  n <- length(a)
  b_data_type <- class(b[1])
  x <- nrow(datainput)
  y <- ncol(datainput)
  if (n == 1) {
    for (i in 1:x) {
      for (j in 1:y) {
        if (datainput[i, j] == a) {
          datainput[i, j] <- b
        }
      }
    }
  } else {
    # List chứa những cột bị thay đổi dữ liệu
    changed_j_index <- list()
    for (t in 1:n) {
      for (j in 1:y) {
        # Cờ báo hiệu giá trị j đã được append hay chưa
        is_appended <- 0
        for (i in 1:x) {
          if (datainput[i, j] == a[t]) {
            if (is_appended == 0) {
              changed_j_index <- append(changed_j_index, j)
              is_appended <- 1
            }
            datainput[i, j] <- b[t]
          }
        }
      }
    }
    for (j in changed_j_index) {
      datainput[, j] <- switch(b_data_type,
        numeric = as.numeric(datainput[, j]),
        character = as.character(datainput[, j]),
        factor = as.factor(datainput[, j]),
        logical = as.logical(datainput[, j]),
        datainput[, j]
      )
    }
  }
  return(datainput)
}

#-------------------------------------------------------------
# Hàm tính trung bình
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(xy)
mean <- function(datainput) {
  # Trả về giá trị trung bình của từng cột
  x <- nrow(datainput)
  y <- ncol(datainput)
  mean <- vector("numeric", y)
  for (i in 1:x) {
    for (j in 1:y) {
      mean[j] <- mean[j] + datainput[i, j] / x
    }
  }
  return(mean)
}

#-------------------------------------------------------------
# Hàm tính trung vị
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(xlog(x)y)
median <- function(datainput) {
  # Trả về giá trị trung vị của từng cột
  x <- nrow(datainput)
  y <- ncol(datainput)
  array <- vector("numeric", y)
  for (i in 1:y) {
    sorted_array <- sort(df_to_array(datainput[i]))
    if (x %% 2 == 1) {
      array[i] <- sorted_array[as.integer(x / 2) + 1]
    } else {
      array[i] <- (sorted_array[as.integer(x / 2)]) / 2
      array[i] <- array[i] +
        (sorted_array[as.integer(x / 2) + 1]) / 2
    }
  }
  return(array)
}

#-------------------------------------------------------------
# Hàm tính độ lệch chuẩn
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(xy)
standard_deviation <- function(datainput) {
  # Trả về độ lệch chuẩn của từng cột
  y <- ncol(datainput)
  array <- vector("numeric", y)
  for (j in 1:y) {
    array[j] <- sqrt(mean(df_to_array(datainput[j]^2)) -
                       mean(df_to_array(datainput[j]))^2)
  }
  return(array)
}

#-------------------------------------------------------------
# Hàm tính min
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(xy)
min <- function(datainput) {
  # Trả về giá trị nhỏ nhất của từng cột
  x <- nrow(datainput)
  y <- ncol(datainput)
  array <- vector("numeric", y)
  for (j in 1:y) {
    min_value <- datainput[1, j]
    for (i in 2:x) {
      if (datainput[i, j] < min_value) {
        min_value <- datainput[i, j]
      }
    }
    array[j] <- min_value
  }
  return(array)
}

#-------------------------------------------------------------
# Hàm tính max
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(xy)
max <- function(datainput) {
  # Trả về giá trị lớn nhất của từng cột
  x <- nrow(datainput)
  y <- ncol(datainput)
  array <- vector("numeric", y)
  for (j in 1:y) {
    max_value <- datainput[1, j]
    for (i in 2:x) {
      if (datainput[i, j] > max_value) {
        max_value <- datainput[i, j]
      }
    }
    array[j] <- max_value
  }
  return(array)
}

# =============================================================
# =============================================================
# ====================== MAIN CODE ============================
# =============================================================
# =============================================================

options(scipen = 99)
library(ggplot2)
library(reshape2)

# Vào dữ liệu
data <- read.csv("data.csv")
View(data)

# =============================================================
# ================== TIỀN XỬ LÝ SỐ LIỆU =======================
# =============================================================

#-------------------------------------------------------------
#Vẽ bảng phân phối của ba biến output
pairs(~roughness + elongation + fan_speed, data)

#-------------------------------------------------------------
# Lọc dữ liệu
filtered_data <- data.frame(
  data[1], data[2], data[3],
  data[4], data[5], data[6],
  data[7], data[8], data[9]
)

# Hiển thị kết quả
View(filtered_data)

#-------------------------------------------------------------
# Kiểm tra dữ liệu bị khuyết
print(apply(is.na(filtered_data), 2, which))

#-------------------------------------------------------------
# Chuyển đổi biến
# grid -> 0, honeycomb -> 1, abs -> 0, pla -> 1
# Sử dụng hàm tự viết là replace_value
converted_data <- replace_value(
  filtered_data,
  c("grid", "honeycomb", "abs", "pla"),
  c(0, 1, 0, 1)
)

# Hiển thị kết quả
View(converted_data)

#-------------------------------------------------------------
# Tính các giá trị thống kê mẫu
# Sử dụng các hàm tự viết như get_col_name, mean,
# median, standard_deviation
col.names <- get_col_name(converted_data)
mean <- mean(converted_data)
median <- median(converted_data)
min <- min(converted_data)
max <- max(converted_data)
sd <- standard_deviation(converted_data)
table <- data.frame(col.names, mean, median, min, max, sd)

# Hiển thị kết quả
View(table)

#-------------------------------------------------------------
# Hiển thị hai biến phân loại
print(table(data$infill_pattern))
print(table(data$material))

#=============================================================
#====================== VẼ HEATMAP ===========================
#=============================================================

# Tạo dataframe cho heatmap
heatmap_data <- replace_value(
  data,
  c("grid", "honeycomb", "abs", "pla"),
  c(0, 1, 0, 1)
)

# Tạo dải màu cho heatmap
color_palette <- c("#00ffff", "#8877ff", "#ff00ff")

# Tạo ma trận tương quan và làm tròn các hệ số
# đến hai số sau dấu phẩy
corr_matr <- round(cor(heatmap_data), 2)

# Hiển thị ma trận tương quan
View(corr_matr)

# Reshape ma trận về dạng thích hợp để đưa vào heatmap
melted_corr_matr <- melt(corr_matr)

# Hiển thị ma trận tương quan dạng melted
View(melted_corr_matr)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# In heatmap
print(
  ggplot(data = melted_corr_matr, aes(
    x = Var1,
    y = Var2, fill = value
  )) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(Var1,
        Var2,
        label = value
      ),
      color = "white", size = 4
    ) +
    scale_fill_gradientn(colors = color_palette)
)

#=============================================================
#====================== VẼ HISTOGRAM =========================
#=============================================================

# Tạo cửa sổ graphic mới
dev.new()

# Vẽ histogram của layer height
hist(converted_data[, "layer_height"],
  xlab = "layer_height",
  main = "Histogram của layer height"
)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# Vẽ histogram của wall thickness
hist(converted_data[, "wall_thickness"],
  xlab = "wall_thickness",
  main = "Histogram của wall thickness"
)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# Vẽ histogram của infill density
hist(converted_data[, "infill_density"],
  xlab = "infill_density",
  main = "Histogram của infill density"
)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# Vẽ histogram của material
hist(converted_data[, "material"],
  xlab = "material",
  main = "Histogram của material"
)

#=============================================================
#====================== TẠO BOXPLOT ==========================
#=============================================================

# Tạo dataframe cho boxplot
boxplot_data <- replace_value(
  data,
  c("grid", "honeycomb", "abs", "pla"),
  c(0, 1, 0, 1)
)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# Boxplot cho layer height và roughness
boxplot(layer_height ~ roughness,
  data = boxplot_data,
  main = "Boxplot cho layer height và roughness"
)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# Boxplot cho layer height và elongation
boxplot(layer_height ~ elongation,
  data = boxplot_data,
  main = "Boxplot cho layer height và elongation"
)

#-------------------------------------------------------------
# Tạo cửa sổ graphic mới
dev.new()

# Boxplot cho wall thickness và tension strength
boxplot(wall_thickness ~ tension_strenght,
  data = boxplot_data,
  main = "Boxplot cho wall thickness và tension strength"
)

#=============================================================
#=============== HỒI QUY TUYẾN TÍNH ĐA BIẾN ==================
#=============================================================

# Tạo dataframe cho hồi quy tuyến tính
linear_regression_data <- replace_value(
  data,
  c("grid", "honeycomb", "abs", "pla"),
  c(0, 1, 0, 1)
)

#-------------------------------------------------------------
# Hồi quy cho biến roughness và 8 biến input
roughness_model <- lm(formula =
    roughness ~
    (layer_height +
     wall_thickness +
     infill_density +
     infill_pattern +
     nozzle_temperature +
     bed_temperature +
     print_speed +
     material),
  data = linear_regression_data
)

# In ra kết quả
print(cat(
  "Kết quả hồi quy tuyến tính đa biến cho",
  "biến roughness và 8 biến input"
))
print(summary(roughness_model))

#-------------------------------------------------------------
# Điều chỉnh lại mô hình roughness, loại bỏ ba biến
# wall_thickness, infill_density và infill_pattern
adjusted_roughness_model <- lm(formula =
    roughness ~
    (layer_height +
     nozzle_temperature +
     bed_temperature +
     print_speed +
     material),
  data = linear_regression_data
)

# In ra kết quả
print("Điều chỉnh lại mô hình cho biến roughness")
print(summary(adjusted_roughness_model))

#-------------------------------------------------------------
# Hồi quy cho biến tension strength và 8 biến input
tension_strenght_model <- lm(formula =
    tension_strenght ~
    (layer_height +
     wall_thickness +
     infill_density +
     infill_pattern +
     nozzle_temperature +
     bed_temperature +
     print_speed +
     material),
  data = linear_regression_data
)

# In ra kết quả
print(cat(
  "Kết quả hồi quy tuyến tính đa biến cho biến",
  "tension strength và 8 biến input"
))
print(summary(tension_strenght_model))

#-------------------------------------------------------------
# Điều chỉnh lại mô hình tension strength, loại bỏ ba biến
# infill_pattern, print_speed và material
adjusted_tension_strenght_model <- lm(formula =
    tension_strenght ~
    (layer_height +
     wall_thickness +
     infill_density +
     nozzle_temperature +
     bed_temperature),
  data = linear_regression_data
)

# In ra kết quả
print("Điều chỉnh lại mô hình cho biến tension strength")
print(summary(adjusted_tension_strenght_model))

#-------------------------------------------------------------
# Hồi quy cho biến elongation và 8 biến input
elongation_model <- lm(formula =
    elongation ~
    (layer_height +
     wall_thickness +
     infill_density +
     infill_pattern +
     nozzle_temperature +
     bed_temperature +
     print_speed +
     material),
  data = linear_regression_data
)

# In ra kết quả
print(cat(
  "Kết quả hồi quy tuyến tính đa biến cho",
  "biến elongation và 8 biến input"
))
print(summary(elongation_model))

#-------------------------------------------------------------
# Điều chỉnh lại mô hình elongation, loại bỏ ba biến
# wall_thickness, infill_pattern, print_speed
adjusted_elongation_model <- lm(formula =
    elongation ~
    (layer_height +
     infill_density +
     nozzle_temperature +
     bed_temperature +
     material),
  data = linear_regression_data
)

# In ra kết quả
print("Điều chỉnh lại mô hình cho biến elongation")
print(summary(adjusted_elongation_model))

#=============================================================
#==================== DỰ ĐOÁN SỐ LIỆU ========================
#=============================================================

# Tạo test case
test_data <- data.frame(
  layer_height = 0.06,
  wall_thickness = 8,
  infill_density = 90,
  infill_pattern = 1,
  nozzle_temperature = 220,
  bed_temperature = 80,
  print_speed = 40,
  material = 1,
  fan_speed = 0
)

# In kết quả dự đoán roughness
print(predict(adjusted_roughness_model, test_data))

# In kết quả dự đoán tension strength
print(predict(adjusted_tension_strenght_model, test_data))

# In kết quả dự đoán elongation
print(predict(adjusted_elongation_model, test_data))
