# User-defined function
# Author: Dang Cao Minh
# Date modified: 20/11/2023

#-------------------------------------------------------------------------------
# Chuyển kiểu dữ liệu data frame có một cột sang array
# Giá trị nhập vào: data frame chỉ có một cột
# Giá trị trả về: array
# Độ phức tạp: O(x)
df_to_array <- function(datainput) {
  return(array(unlist(datainput)))
}

#-------------------------------------------------------------------------------
# Lấy tên của các cột
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(y)
get_col_name <- function(datainput) {
  col_names <- colnames(datainput)
  return(col_names)
}

#-------------------------------------------------------------------------------
# Nhập vào hai vector a và b độ dài n
# Thay tất cả giá trị a[i] trong data frame thành b[i] với mọi i
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

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
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
      array[i] <- array[i] + (sorted_array[as.integer(x / 2) + 1]) / 2
    }
  }
  return(array)
}

#-------------------------------------------------------------------------------
# Hàm tính độ lệch chuẩn
# Giá trị nhập vào: data frame
# Giá trị trả về: vector
# Độ phức tạp: O(xy)
standard_derivation <- function(datainput) {
  # Trả về độ lệch chuẩn của từng cột
  y <- ncol(datainput)
  array <- vector("numeric", y)
  for (j in 1:y) {
    array[j] <- sqrt(mean(df_to_array(datainput[j]^2)) -
                       mean(df_to_array(datainput[j]))^2)
  }
  return(array)
}

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
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

#--------------------------------------------------------------------------
#-----------------------------MAIN CODE------------------------------------
#--------------------------------------------------------------------------

options(scipen = 99)
library(ggplot2)
library(reshape2)
# Vào dữ liệu
data <- read.csv(".\\data.csv")

#-------------------------------------------------------------------------------
# Lọc dữ liệu
data2 <- data.frame(
  data[1], data[2], data[3],
  data[4], data[5], data[6],
  data[7], data[8], data[9],
)
View(data2)



#-------------------------------------------------------------------------------
# Lọc dữ liệu lần 2
data2 <- data.frame(
  data[1], data[2], data[3],
  data[4], data[5], data[6],
  data[7], data[8], data[9],
)
View(data2)

data3 <- replace_value(data2, 0, 0.0000001)
View(data3)
data4 <- log_conversion(data3)
View(data4)
col.names <- get_col_name(data2)
mean <- mean(data4)
median <- median(data4)
min <- min(data4)
max <- max(data4)
sd <- standard_derivation(data4)
table <- data.frame(col.names, mean, median, min, max, sd)
View(table)

dev.new()
# Histogram của layer height
hist(data3[, "wall_thickness"],
  xlab = "wall_thickness",
  main = "Histogram of wall_thickness"
)
dev.new()
hist(data4[, "wall_thickness"],
  xlab = "log(wall_thickness)",
  main = "Đồ thị Histogram of log(wall_thickness)"
)
dev.new()
# Box plot của layer height và wall thickness
boxplot(layer_height ~ wall_thickness,
  main = "Boxplot of LH for each WT",
  data = data3
)
dev.new()
boxplot(layer_height ~ wall_thickness,
  main = "Boxplot of log(LH) for each log(WT)",
  data = data4
)

heatmap_data <- replace_value(
  data,
  c("grid", "honeycomb", "abs", "pla"),
  c(0, 1, 0, 1)
)
# Define a custom color palette
color_palette <- c("#00ffff", "#8877ff", "#ff00ff")
corr_matr <- round(cor(heatmap_data), 2)
melted_corr_matr <- melt(corr_matr)
dev.new()
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
      color = "white", linewidth = 4
    ) +
    scale_fill_gradientn(colors = color_palette)
)

linear_regression_data <- heatmap_data

# Fit a multiple linear regression model
model <- lm(formula =
    linear_regression_data[, 10] ~
    (linear_regression_data[, 1] +
     linear_regression_data[, 2] +
     linear_regression_data[, 3] +
     linear_regression_data[, 4] +
     linear_regression_data[, 5] +
     linear_regression_data[, 6] +
     linear_regression_data[, 7] +
     linear_regression_data[, 8]) ,
  data = linear_regression_data
)

# Print the summary of the model
print(summary(model))
