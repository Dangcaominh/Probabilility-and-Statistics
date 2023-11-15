options(scipen = 99)
# Vào dữ liệu
data <- read.csv(".\\data.csv")

#-------------------------------------------------------------------------------
# Lọc dữ liệu
data2 <- data.frame(
  data[1], data[2], data[3],
  data[5], data[6], data[7],
  data[9]
)
View(data2)

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
# Chuyển đổi tất cả các ô bằng 0 thành 0.0000001
# Giá trị nhập vào: data frame
# Giá trị trả về: data frame
# Độ phức tạp: O(xy)
replace_zero <- function(datainput) {
  x <- nrow(datainput)
  y <- ncol(datainput)
  for (i in 1:x) {
    for (j in 1:y) {
      if (datainput[i, j] == 0) {
        datainput[i, j] <- 0.0000001
      }
    }
  }
  return(datainput)
}

#-------------------------------------------------------------------------------
# Chuyển đổi giá trị ở tất cả các ô thành log của nó
# Giá trị nhập vào: data frame
# Giá trị trả về: data frame
# Độ phức tạp: O(xy)
log_conversion <- function(datainput) {
  x <- nrow(datainput)
  y <- ncol(datainput)
  for (i in 1:x) {
    for (j in 1:y) {
      datainput[i, j] <- log(datainput[i, j])
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

data3 <- replace_zero(data2)
data4 <- log_conversion(data3)
col.names <- get_col_name(data2)
mean <- mean(data2)
median <- median(data2)
min <- min(data2)
max <- max(data2)
sd <- standard_derivation(data2)
table <- data.frame(col.names, mean, median, min, max, sd)
View(table)
print("Anh gymer lỏ")