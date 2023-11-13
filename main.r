data <- read.csv(".\\data.csv")
data2 <- data.frame(
  data[1], data[2], data[3],
  data[4], data[5], data[6],
  data[7], data[8], data[9]
)
pow <- function(n, k) {
  if ((n == 1)  || (k == 1)) {
    return(n)
  } else if (k %% 2 == 1) {
    return(n * pow(n, as.integer(k / 2)) * pow(n, as.integer(k / 2)))
  } else {
    return(pow(n, as.integer(k / 2)) * pow(n, as.integer(k / 2)))
  }
}
# Tính 2 mũ 30
y <- pow(2, 30)
print(y)
print("Anh gymer lỏ")
