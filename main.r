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
# Lấy tên của các cột
# Giá trị nhập vào: data frame
# Giá trị trả về: array
# Độ phức tạp: O(xy)
get_col_name <- function(datainput) {
  col_names <- colnames(datainput)
  return(col_names)
}

#-------------------------------------------------------------------------------
# Chuyển đổi tất cả các ô bằng 0 thành 0.0000001
# Giá trị nhập vào: data frame
# Giá trị trả về: data frame
# Độ phức tạp: O(y)
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
# Giá trị trả về: data frame
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
# Giá trị trả về: data frame
# Độ phức tạp: O(xlog(x)y)
median <- function(datainput) {
  # Trả về giá trị trung vị của từng cột
  x <- nrow(datainput)
  y <- ncol(datainput)
  array <- vector("numeric", y)
  for (i in 1:y) {
    # array(unlist(datainput[i])) để chuyển đổi data frame thành array
    sorted_array <- sort(array(unlist(datainput[i])))
    if (x %% 2 == 1) {
      array[i] <- sorted_array[as.integer(x / 2) + 1]
    } else {
      array[i] <- (sorted_array[as.integer(x / 2)]) / 2
      array[i] <- array[i] + (sorted_array[as.integer(x / 2) + 1]) / 2
    }
  }
  return(array)
}
data3 <- replace_zero(data2)
data4 <- log_conversion(data3)
mean <- mean(data2)
median <- median(data2)
table <- data.frame(mean, median)
View(table)
print("Anh Lân vơ ")