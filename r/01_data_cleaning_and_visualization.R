#1. Data Import and Initial Cleaning
data_raw <- read_excel("data/survei_anonymized.xlsx")
head(data_raw)

# cek unique value from status 
unique(data_raw$status)
table(tolower(trimws(data_raw$status)))

# Retain only respondents with student status
data_mhs <- data_raw %>%
  filter(tolower(status) == "mahasiswa")
unique(data_mhs$status)

# Remove non-analytical variables (identifiers)
data_clean <- data_mhs %>%
  dplyr::select(-nama, -status)

data_clean <- data_clean %>%
  mutate(across(everything(), as.numeric))

#Check missing value
colSums(is.na(data_clean))

#Boxplot for outlier screening
p <- ceiling(sqrt(ncol(data_clean)))
par(mfrow = c(p, p))

for (v in names(data_clean)) {
  boxplot(
    data_clean[[v]],
    main = paste("Boxplot of", v),
    col = "lightblue",
    horizontal = FALSE
  )
}

# Detect outlier using IQR method
detect_iqr_outlier <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  which(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
}
out_sks <- detect_iqr_outlier(data_clean$SKS)
out_exercise <- detect_iqr_outlier(data_clean$ExerciseHoursPerDay)

# Inspect detected outlier values
data_clean[out_sks, "SKS"]
data_clean[out_exercise, "ExerciseHoursPerDay"]

# Remove extreme but implausible outliers
data_clean <- data_clean %>%
  filter(SKS != 88, ExerciseHoursPerDay != 24)

# Verify cleaned dataset
nrow(data_clean)
str(data_clean)
head(data_clean)

# 2. Exploratory Data Analysis (EDA)
d <- density(data_clean$StudyHoursPerDay)

# Plot histogram with density overlay
hist(
  data_clean$StudyHoursPerDay,
  probability = TRUE,
  breaks = 10,
  col = rgb(0.7, 0.85, 0.9, 0.6),
  border = "white",
  main = "Distribution of Study Hours per Day",
  xlab = "Study Hours per Day",
  ylim = c(0, max(d$y) * 1.1)  # buffer 10%
)

# Overlay kernel density curve
lines(d, lwd = 2, col = "black")

