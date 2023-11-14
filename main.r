options(digits = 7)
data <- read.csv(".\\data.csv")
data2 <- data.frame(
  data[1], data[2], data[3],
  data[5], data[6], data[7],
  data[9]
)
View(data2)
# Chuyển đổi tất cả các ô bằng 0 thành 0.0000001
# Độ phức tạp O(xy)
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
# Chuyển đổi giá trị ở tất cả các ô thành log của nó
# Độ phức tạp O(xy)
log_conversion <- function(datainput){
  x <- nrow(datainput)
  y <- ncol(datainput)
  for (i in 1:x) {
    for (j in 1:y) {
      datainput[i, j] <- log(datainput[i, j])
    }
  }
  return(datainput)
}
data3 <- replace_zero(data2)
data4 <- log_conversion(data3)
View(data3)
View(data4)
print("Anh gymer lỏ")
