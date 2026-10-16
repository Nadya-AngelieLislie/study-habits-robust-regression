library(readxl)
library(MASS)
library(dplyr)

data_raw <- read_excel("data/survei_anonymized.xlsx")
head(data_raw)

#data cleaning

# cek unique value dari status (kita hanya ingin mahasiswa)
unique(data_raw$status)

# filter hanya responden berstatus "mahasiswa"
data_mhs <- data_raw %>%
  filter(tolower(status) == "mahasiswa")

# cek lagi
unique(data_mhs$status)

# karena udh clean, skrg kita hapus kolom yang gadi pake
data_clean <- data_mhs %>%
  dplyr::select(-nama, -status)

data_clean <- data_clean %>%
  mutate(across(everything(), as.numeric))

str(data_clean)
head(data_clean)

nrow(data_clean)


#### a. Fit multiple linear regression model ####
# Fit regression
model <- lm(StudyHoursPerDay ~ . , data = data_clean)
# Summary output
summary(model)

# Mengecek nilai ekstrim pada variabel respon
std_resid <-rstandard(model)
outlier_3sd <- which(abs(std_resid) > 3)
outlier_3sd

# Mengecek nilai ekstrim di prediktor
hatvalues(model)
# which(hatvalues(model) > 2*(p+1)/n)
which(hatvalues(model) > 2*(8+1)/47)

# Mengecek influential observation
cook <- cooks.distance(model)
which(cook > 1)

# Hapus data observasi 5 dan 47 karena datanya
# berupa outlier yang salah input (error)
data_clean <- data_clean[-c(5, 47),]

nrow(data_clean)






#### a. Fit multiple linear regression model ####
# Fit regression
model <- lm(StudyHoursPerDay ~ . , data = data_clean)
# Summary output
summary(model)

# Mengecek nilai ekstrim pada variabel respon
std_resid <-rstandard(model)
outlier_3sd <- which(abs(std_resid) > 3)
outlier_3sd

# Mengecek nilai ekstrim di prediktor
hatvalues(model)
# which(hatvalues(model) > 2*(p+1)/n)
which(hatvalues(model) > 2*(8+1)/45)

# Mengecek influential observation
cook <- cooks.distance(model)
which(cook > 1)

#### b. ANOVA table & test significance ####
anova(model)

#### c. R² and Adjusted R² ####
summary(model)$r.squared       # R^2
summary(model)$adj.r.squared   # R^2_adj

#### d. 95% CI untuk β₁ (engine displacement) ####
confint(model, level = 0.95)

#### e. t-statistics untuk uji ####
summary(model)$coefficients

#### Korelasi antar X
library(psych)
X <- data_clean[, c("Usia", "SleepHoursPerDay", "SocialMediaHoursPerDay", "SKS", "WorkHoursPerDay", 
              "OrganizationHoursPerDay", "CommuteMinutesPerDay", "ExerciseHoursPerDay")]

# Uji korelasi Pearson antar variabel X
hasil_korelasi <- corr.test(X)
hasil_korelasi

hasil_korelasi_with_Y <- corr.test(data_clean)
hasil_korelasi_with_Y


###################################
############ Cek Asumsi ###########
###################################

#### 1. cek distribusi normal ####
# H0: berdistribusi normal
# H1: tidak berdistribusi normal
library(nortest)
e <- resid(model)
lillie.test(e)

##### 2. cek homoskedasitas ##### 
# H0: Tidak heteroskedastisitas
# H1: Heteroskedastisitas
library(lmtest)
bptest(model)

##### 3. cek nonautokorelasi ##### 
# H0: Tidak terdapat autokorelasi
# H1: Terdapat autokorelasi
library(lmtest)
dwtest(model)

##### 4. cek multikolinear ##### 
# H0: Tidak terdapat multikolinearitas
# H1: Terdapat multikolinearitas
library(car)
vif(model) #Varian Inflation Factor (VIF)


#### p-value > 0.05 → Residual berdistribusi normal #####
#### p-value < 0.05 → Heteroskedastisitas terjadi #####
#### p-value > 0.05 ✔ Asumsi tidak ada autokorelasi terpenuhi #####
#### Semua VIF < 10 ✔ Tidak ada multikolinearitas Karena: ####
#### VIF < 5 → aman ####
#### VIF < 10 → sangat aman #####


####################################
####### Variable Selection #########
####################################

# Using Forward Selection

# 1 prediktor
m1 <- lm(StudyHoursPerDay ~ Usia, data=data_clean)
m2 <- lm(StudyHoursPerDay ~ SleepHoursPerDay, data=data_clean)
m3 <- lm(StudyHoursPerDay ~ SocialMediaHoursPerDay, data=data_clean)
m4 <- lm(StudyHoursPerDay ~ SKS, data=data_clean)
m5 <- lm(StudyHoursPerDay ~ WorkHoursPerDay, data=data_clean)
m6 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay, data=data_clean)
m7 <- lm(StudyHoursPerDay ~ CommuteMinutesPerDay, data=data_clean)
m8 <- lm(StudyHoursPerDay ~ ExerciseHoursPerDay, data=data_clean)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

# p-value
# x1 = 0.536
# x2 = 0.00409
# x3 = 0.427
# x4 = 0.397
# x5 = 0.211
# x6 = 2.64e-08
# x7 = 0.598
# x8 = 0.98

# x6 memiliki p-value terkecil, maka akan kita pakai untuk model dengan 2 prediktor selanjutnya.

# 2 prediktor
m61 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+Usia, data=data_clean)
m62 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay, data=data_clean)
m63 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SocialMediaHoursPerDay, data=data_clean)
m64 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SKS, data=data_clean)
m65 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+WorkHoursPerDay, data=data_clean)
m67 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+CommuteMinutesPerDay, data=data_clean)
m68 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+ExerciseHoursPerDay, data=data_clean)

summary(m61)
summary(m62)
summary(m63)
summary(m64)
summary(m65)
summary(m67)
summary(m68)

# p-value
# x1 = 0.834
# x2 = 0.02832
# x3 = 0.774694
# x4 = 0.956
# x5 = 0.043
# x7 = 0.183569
# x8 = 0.929

# x2 memiliki p-value terkecil dan signifikan, maka akan dipakai di model 3 prediktor selanjutnya.

# 3 prediktor
m621 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay+Usia, data=data_clean)
m623 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay+SocialMediaHoursPerDay, data=data_clean)
m624 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay+SKS, data=data_clean)
m625 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay+WorkHoursPerDay, data=data_clean)
m627 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay+CommuteMinutesPerDay, data=data_clean)
m628 <- lm(StudyHoursPerDay ~ OrganizationHoursPerDay+SleepHoursPerDay+ExerciseHoursPerDay, data=data_clean)

summary(m621)
summary(m623)
summary(m624)
summary(m625)
summary(m627)
summary(m628)

# p-value
# x1 = 0.46
# x3 = 0.83115
# x4 = 0.8127
# x5 = 0.05259
# x7 = 0.1842
# x8 = 0.79030

# Karena pada model dengan 3 prediktor tidak ada p-value yang < 0.05, 
# maka model final kita adalah model dengan 2 prediktor, yaitu m62.



# Backward Elimination
#Model 8 prediktor
m12345678 <- lm(StudyHoursPerDay ~ ., data = data_clean)
summary(m12345678)

# p-value
# x1 = 0.3119
# x2 = 0.0282
# x3 = 0.3618
# x4 = 0.8456
# x5 = 0.0169
# x6 = 6.82e-07
# x7 = 0.2331
# x8 = 0.537

# x4 dieliminasi karena memiliki p-value yang paling besar.

# Model 7 prediktor
m1235678 <- lm(StudyHoursPerDay ~ Usia+SleepHoursPerDay+SocialMediaHoursPerDay+
                 WorkHoursPerDay+OrganizationHoursPerDay+CommuteMinutesPerDay+
                 ExerciseHoursPerDay, data = data_clean)
summary(m1235678)

# p-value
# x1 = 0.3015
# x2 = 0.0243
# x3 = 0.3368
# x5 = 0.0157
# x6 = 3.18e-07
# x7 = 0.232
# x8 = 0.5302

# x8 akan dieliminasi karena p-valuenya yang paling besar.

# Model 6 prediktor
m123567 <- lm(StudyHoursPerDay ~ Usia+SleepHoursPerDay+SocialMediaHoursPerDay+
                WorkHoursPerDay+OrganizationHoursPerDay+CommuteMinutesPerDay, data = data_clean)

summary(m123567)

# p-value
# x1 = 0.2561
# x2 = 0.0265
# x3 = 0.4194
# x5 = 0.0144
# x6 = 2.08e-07
# x7 = 0.2801

# x3 akan dieliminasi karena p-valuenya yang paling besar.

# Model 5 prediktor
m12567 <- lm(StudyHoursPerDay ~ Usia+SleepHoursPerDay+WorkHoursPerDay+
               OrganizationHoursPerDay+CommuteMinutesPerDay, data = data_clean)
summary(m12567)

# p-value
# x1 = 0.2142
# x2 = 0.0208
# x5 = 0.0193
# x6 = 1.28e-07
# x7 = 0.2590

# x7 akan dieliminasi karena p-valuenya paling besar.

# Model 4 prediktor
m1256 <- lm(StudyHoursPerDay ~ Usia+SleepHoursPerDay+WorkHoursPerDay+
              OrganizationHoursPerDay, data = data_clean)
summary(m1256)

# p-value
# x1 = 0.1123
# x2 = 0.016
# x5 = 0.0174
# x6 = 1.7e-07

# x1 akan dieliminasi karena p-valuenya yang paling gede

# Model 3 prediktor
m256 <- lm(StudyHoursPerDay ~ SleepHoursPerDay+WorkHoursPerDay+
             OrganizationHoursPerDay, data = data_clean)
summary(m256)

# p-value
# x2 = 0.03485
# x5 = 0.05259
# x6 = 8.08e-08

# x5 akan dieliminasi karena p-valuenya paling gede dan masih > 0.05.

# Model 2 prediktor
m26 <- lm(StudyHoursPerDay ~ SleepHoursPerDay+OrganizationHoursPerDay, data = data_clean)
summary(m26)

# Karena nilai p-valuenya x2 dan x6 < 0.05, maka model final kita adalah model m26.

summary(m62)
summary(m12345678)

# Model dengan x2 dan x6 sebagai prediktor lebih bagus daripada model dengan 8 prediktor sekaligus,
# meskipun nilai R2 model 8 prediktor lebih tinggi dibandingkan model 2 prediktor (x2 dan x6).


