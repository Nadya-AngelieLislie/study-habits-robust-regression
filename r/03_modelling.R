library(lmtest)
library(sandwich)
library(MASS)
library(nlme)
library(car)
library(readxl)
library(readxl)
library(MASS)
library(dplyr)

data <- read_excel("data/survei_anonymized.xlsx")
str(data)

data_raw <- read_excel("data/survei_anonymized.xlsx")
head(data_raw)

#data cleaning
unique(data_raw$status)

data_mhs <- data_raw %>%
  filter(tolower(status) == "mahasiswa")

unique(data_mhs$status)

data_clean <- data_mhs %>%
  dplyr::select(-nama, -status)

data_clean <- data_clean %>%
  mutate(across(everything(), as.numeric))

str(data_clean)
head(data_clean)

nrow(data_clean)

which(data_clean$ExerciseHoursPerDay == 24)
data_clean <- data_clean %>%
  filter(ExerciseHoursPerDay != 24)

which(data_clean$SKS == 88)
data_clean <- data_clean %>%
  filter(SKS != 88)

# Jadi, baseline OLS
# Analitik = Robust SE
# Kompetitor = WLS + (analitik cek ketahanan WLS), GLS

# BASELINE: Multiple Linear Regression (OLS)
model_ols <- lm(
  StudyHoursPerDay ~ Usia +
    SleepHoursPerDay +
    SocialMediaHoursPerDay +
    SKS +
    WorkHoursPerDay +
    OrganizationHoursPerDay +
    CommuteMinutesPerDay +
    ExerciseHoursPerDay,
  data = data_clean
)

summary(model_ols)

# Tes heteroskedastisitas
library(lmtest)
bptest(model_ols)

# Robust Standard Errors (HC3)
library(sandwich)
library(lmtest)

model_robustSE <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC3"))
model_robustSE

# OLS
res_ols <- resid(model_ols)
epsilon <- 1e-6
w_wls <- 1 / (res_ols^2 + epsilon)

# WLS
model_wls <- lm(
  StudyHoursPerDay ~ Usia +
    SleepHoursPerDay +
    SocialMediaHoursPerDay +
    SKS +
    WorkHoursPerDay +
    OrganizationHoursPerDay +
    CommuteMinutesPerDay +
    ExerciseHoursPerDay,
  data = data_clean,
  weights = w_wls
)

summary(model_wls)
bptest(model_wls)

# Robust WLS (RWLS)
model_rwls <- coeftest(model_wls, vcov = vcovHC(model_wls, type = "HC3"))
model_rwls

# Generalized Least Squares (GLS)
library(nlme)

model_gls <- gls(
  StudyHoursPerDay ~ Usia +
    SleepHoursPerDay +
    SocialMediaHoursPerDay +
    SKS +
    WorkHoursPerDay +
    OrganizationHoursPerDay +
    CommuteMinutesPerDay +
    ExerciseHoursPerDay,
  data = data_clean,
  weights = varIdent(form = ~ 1)
)

summary(model_gls)

plot(fitted(model_gls), resid(model_gls))
abline(h = 0, col="red")

