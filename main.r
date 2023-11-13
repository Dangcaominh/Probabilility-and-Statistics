data <- read.csv(".\\data.csv")
data2 <- data.frame(
  data[1], data[2], data[3],
  data[4], data[5], data[6],
  data[7], data[8], data[9]
)
print(apply(is.na(data2), 2, which))
print("Anh gymer lỏ")
