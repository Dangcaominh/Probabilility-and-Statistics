# Read the data from the CSV file
data <- read.csv("data.csv", stringsAsFactors = FALSE)

# Tìm outliers sử dụng phương pháp Turkey
numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]
outliers <- sapply(numeric_data, function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  x[x < lower_bound | x > upper_bound]
})

# In ra outliers cho mỗi biến
for (i in seq_along(outliers)) {
  variable_name <- names(outliers)[i]
  variable_outliers <- outliers[[i]]
  if (length(variable_outliers) > 0) {
    cat("Outliers cho", variable_name, ":", variable_outliers, "\n")
  } else {
    cat("Không có outliers cho", variable_name, "\n")
  }
}