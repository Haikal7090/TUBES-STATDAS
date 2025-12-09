#Baca Data
library(readxl)
data <- read_excel("D:/ITB/Jurusan TM/S3/Statiska Dasar/Tubes/Dataset/archive (15)/clean_oil_well_data.xlsx")
data
library(forecast)

# Copy data
data_capped <- data

# variabel
Oil_Volume = data_capped$`Oil volume (m3/day)`
Gas_Volume = data_capped$`Gas volume (m3/day)`
Water_Volume = data_capped$`Water volume (m3/day)`
Water_Cut = data_capped$`Water cut (%)`
Reservoir_Pressure = data_capped$`Reservoir pressure (atm)`
Volume_liquid = data_capped$`Volume of liquid (m3/day)`
Working_hours = data_capped$`Working hours`
Dynamic_level = data_capped$`Dynamic level (m)`

data_capped$Date = as.Date((data_capped$Date))


# Grafik Boxplot
boxplot(Oil_Volume, horizontal=T, main="Boxplot Oil Volume",sub=paste("Outlier rows: ", boxplot.stats(Oil_Volume)$out))
boxplot(Gas_Volume, horizontal=T, main="Boxplot Gas Volume", sub=paste("Outlier rows: ", boxplot.stats(Gas_Volume)$out))
boxplot(Water_Volume, horizontal=T, main="Boxplot Water Volume", sub=paste("Outlier rows: ", boxplot.stats(Water_Volume)$out))
boxplot(Water_Cut, horizontal=T, main="Boxplot Water Cut")
boxplot(Reservoir_Pressure, horizontal=T, main="Boxplot Reservoir Pressure")
boxplot(Volume_liquid, horizontal=T, main="Boxplot Volume of Liquid")
boxplot(Working_hours, horizontal=T, main="Boxplot Working Hours")
boxplot(Dynamic_level, horizontal=T, main="Boxplot Dynamic Level")

# Korelasi
cor(Oil_Volume, Water_Cut) # korelasi negatif terbaik 
cor(Oil_Volume, Reservoir_Pressure) # korelasi positif terbaik
cor(Gas_Volume, Reservoir_Pressure) # korelasi positif terbaik
cor(Gas_Volume, Water_Cut) # korelasi positif terbaik

# Heat Map
library(corrplot)
num_data <- data[sapply(data, is.numeric)]
corr_matrix <- cor(num_data, use = "complete.obs")

corrplot(
  corr_matrix,
  method = "color",                         # warna blok
  col = colorRampPalette(c("navy","white","firebrick"))(200),
  type = "full",                            # tampilkan full matrix
  addCoef.col = "black",                    # tampilkan angka korelasi
  tl.col = "black",                         # warna label
  tl.cex = 0.8,                             # ukuran label
  number.cex = 0.6,                         # ukuran angka korelasi
  diag = FALSE                              # hilangkan diagonal
)

# Winsorize langsung pada dataframe utama agar permanen
data_capped$`Oil volume (m3/day)` <- Winsorize(data_capped$`Oil volume (m3/day)`, 
                        val=quantile(data_capped$`Oil volume (m3/day)`, 
                                     c(0.05, 0.95), na.rm=TRUE))
data_capped$`Gas volume (m3/day)` <- Winsorize(data_capped$`Gas volume (m3/day)`, 
                        val=quantile(data_capped$`Gas volume (m3/day)`, 
                                     c(0.05, 0.95), na.rm=TRUE))
data_capped$`Reservoir pressure (atm)` <- Winsorize(data_capped$`Reservoir pressure (atm)`, 
                                                    val = quantile(data_capped$`Reservoir pressure (atm)`, 
                                                                   c(0.05, 0.95), na.rm = TRUE))
data_capped$`Water cut (%)` <- Winsorize(data_capped$`Water cut (%)`, 
                                         val = quantile(data_capped$`Water cut (%)`, 
                                                        c(0.05, 0.95), na.rm = TRUE))
# Data spliiting : Data Train dan Data Test
cut_index <- floor(0.8 * nrow(data_capped))
train_data <- data_capped[1:cut_index, ]
test_data  <- data_capped[(cut_index+1):nrow(data_capped), ]

# Membuat TS object dari data yang SUDAH di-winsorize
Oil_train <- ts(train_data$`Oil volume (m3/day)`)
Oil_test  <- ts(test_data$`Oil volume (m3/day)`)
Gas_train <- ts(train_data$`Gas volume (m3/day)`)
Gas_test  <- ts(test_data$`Gas volume (m3/day)`)
Water_Cut_train <- ts(train_data$`Water cut (%)`)
Water_Cut_test <- ts(test_data$`Water cut (%)`)
Reservoir_Pressure_train <- ts(train_data$`Reservoir pressure (atm)`)
Reservoir_Pressure_test <- ts(test_data$`Reservoir pressure (atm)`)

# Model 1: Oil ~ Pressure
# Perhatikan: Kita panggil nama kolom ASLI di dalam train_data
linearMod <- lm(`Oil volume (m3/day)` ~ `Reservoir pressure (atm)`, data = train_data)
print(linearMod)
summary(linearMod)
AIC(linearMod)

# Model 2: Oil ~ Water Cut
linearMod_1 <- lm(`Oil volume (m3/day)` ~ `Water cut (%)`, data = train_data)
print(linearMod_1)
summary(linearMod_1)
AIC(linearMod_1) # <-- Perbaikan: Gunakan linearMod_1

# Model 3: Gas ~ Pressure
linearMod_2 <- lm(`Gas volume (m3/day)` ~ `Reservoir pressure (atm)`, data = train_data)
print(linearMod_2)
summary(linearMod_2)
AIC(linearMod_2) # <-- Perbaikan: Gunakan linearMod_2

# Model 4: Gas ~ Water Cut
linearMod_3 <- lm(`Gas volume (m3/day)` ~ `Water cut (%)`, data = train_data)
print(linearMod_3)
summary(linearMod_3)
AIC(linearMod_3) # <-- Perbaikan: Gunakan linearMod_3

# =========================================================
# 1. STEP PENTING: GENERATE PREDIKSI DULU
# =========================================================
# Kita ubah model (linearMod) menjadi angka prediksi yang sesuai dengan data test

pred_model_1 <- predict(linearMod, newdata = test_data)   # Prediksi Model 1
pred_model_2 <- predict(linearMod_1, newdata = test_data) # Prediksi Model 2
pred_model_3 <- predict(linearMod_2, newdata = test_data) # Prediksi Model 3
pred_model_4 <- predict(linearMod_3, newdata = test_data) # Prediksi Model 4

# =========================================================
# 2. VISUALISASI SCATTER PLOT (PERBAIKAN)
# =========================================================
par(mfrow=c(2,2)) # Bagi layar jadi 4

# --- Grafik 1: Oil vs Pressure ---
plot(test_data$`Reservoir pressure (atm)`, test_data$`Oil volume (m3/day)`,
     pch=19, col="red", cex=0.6,
     main="Validasi 1: Oil vs Pressure", 
     xlab="Pressure (atm)", ylab="Oil Vol")
# PERBAIKAN: Gunakan 'pred_model_1', JANGAN 'linearMod'
points(test_data$`Reservoir pressure (atm)`, pred_model_1, 
       col="blue", pch=19, cex=0.6)
legend("topleft", legend=c("Actual", "Prediksi"), col=c("red", "blue"), pch=19, cex=0.6)

# --- Grafik 2: Oil vs Water Cut ---
plot(test_data$`Water cut (%)`, test_data$`Oil volume (m3/day)`,
     pch=19, col="red", cex=0.6,
     main="Validasi 2: Oil vs Water Cut", 
     xlab="Water Cut (%)", ylab="Oil Vol")
points(test_data$`Water cut (%)`, pred_model_2, col="blue", pch=19, cex=0.6)

# --- Grafik 3: Gas vs Pressure ---
plot(test_data$`Reservoir pressure (atm)`, test_data$`Gas volume (m3/day)`,
     pch=19, col="red", cex=0.6,
     main="Validasi 3: Gas vs Pressure", 
     xlab="Pressure (atm)", ylab="Gas Vol")
points(test_data$`Reservoir pressure (atm)`, pred_model_3, col="blue", pch=19, cex=0.6)

# --- Grafik 4: Gas vs Water Cut ---
plot(test_data$`Water cut (%)`, test_data$`Gas volume (m3/day)`,
     pch=19, col="red", cex=0.6,
     main="Validasi 4: Gas vs Water Cut", 
     xlab="Water Cut (%)", ylab="Gas Vol")
points(test_data$`Water cut (%)`, pred_model_4, col="blue", pch=19, cex=0.6)

par(mfrow=c(1,1)) # Reset layar

# Membuat TS object dari data yang SUDAH di-winsorize
Oil_train <- ts(train_data$`Oil volume (m3/day)`)
Oil_test  <- ts(test_data$`Oil volume (m3/day)`)

# 1. Cek kapan data training berakhir
akhir_training <- length(Oil_train)

# 2. Buat Oil_test dengan parameter 'start'
# Artinya: Data test dimulai pada indeks (akhir_training + 1)
Oil_test <- ts(test_data$`Oil volume (m3/day)`, 
               start = akhir_training + 1)

# plot
plot(Oil_train,main="Grafik Volume Minyak Sumur № 807",ylab="Volume Minyak",xlab="Hari",type='o')

# test stasioner
library(tseries)
adf.test(Oil_train)

# didapat p-value < alpha, stasioner, namun di grafik ada tren

# diferensisasi
Oil_train_diff = diff(Oil_train)
plot(Oil_train_diff,main="Grafik Volume Minyak Sumur № 807",ylab="Volume Minyak",xlab="Hari",type='o')
adf.test(Oil_train_diff)

# dari grafik berubah

acf(Oil_train_diff, main="Grafik ACF") #MA(1)
pacf(Oil_train_diff, main="Grafik PACF") #AR(1)

#Model yang mungkin ada
# 1. Model AR : ARIMA(1,1,0)
# 2. Model IMA : ARIMA(0,1,1)
# 3. Model ARIMA : ARIMA(1,1,1)

model_ari = arima(Oil_train,order = c(1,1,0))
summary(model_ari) #  aic = 9575.36

model_ima = arima(Oil_train,order = c(0,1,1))
summary(model_ima) # aic = 9573.57

model_arima = arima(Oil_train,order = c(1,1,1))
summary(model_arima)  # aic = 9471.19

model_auto <- auto.arima(Oil_train, lambda = 0) 
summary(model_auto) # AIC=-2162.25

# Cek residual
checkresiduals(model_auto) #  p-value = 0.2422
checkresiduals(model) #  p-value = 0.07383
checkresiduals(model_ari) # p-value = 1.846e-07 
checkresiduals(model_ima) # p-value < 2.2e-16
checkresiduals(model_arima) # p-value = 0.003344

# Didapatkan model terbaik itu model_ima yakni model arima(1,1,1)
(prediksi = forecast(model_auto, h = 100))
plot(prediksi,main="Grafik Produksi Harian Minyak", ylab= "Produksi Minyak", xlab="Hari",type='o')

# PREDIKSI MENGGUNAKAN DATA TEST
# Kita prediksi sebanyak jumlah data test (h = length(Oil_test))
# Karena lambda=0, fungsi forecast akan otomatis mengembalikan ke skala asli (inverse log).
prediksi_test <- forecast(model_auto, h = length(Oil_test))

# VISUALISASI PERBANDINGAN (FORECAST VS ACTUAL)
# Plot hasil prediksi (Biru)
plot(prediksi_test, 
     main="Validasi Model: Forecast vs Actual Data", 
     ylab="Volume Minyak (m3/day)", 
     xlab="Waktu")

# Tambahkan garis Data Aktual / Test (Merah)
lines(Oil_test, col="red", lwd=2)

# Tambahkan legenda agar jelas
legend("topleft", legend=c("Prediksi Model", "Data Aktual (Test)"), 
       col=c("blue", "red"), lty=1, lwd=2)

# CEK AKURASI (METRICS)
# Fungsi accuracy() akan membandingkan hasil prediksi dengan Oil_test
akurasi <- accuracy(prediksi_test, Oil_test)
print(akurasi)

# Model Analisis Deret Waktu Gas Volume

# Membuat TS object dari data yang SUDAH di-winsorize
Gas_train <- ts(train_data$`Gas volume (m3/day)`)
Gas_test  <- ts(test_data$`Gas volume (m3/day)`

# 1. Cek kapan data training berakhir
akhir_training <- length(Gas_train)

# 2. Buat Oil_test dengan parameter 'start'
# Artinya: Data test dimulai pada indeks (akhir_training + 1)
Gas_test <- ts(test_data$`Gas volume (m3/day)`, 
               start = akhir_training + 1)

plot(Gas_train,main="Grafik Volume Gas Sumur № 807",ylab="Volume Gas",xlab="Hari",type='o')

# test stasioner
library(tseries)
adf.test(Gas_train)

Gas_train_diff = diff(Gas_train)
plot(Gas_train_diff,main="Grafik Volume Gas Sumur № 807",ylab="Volume Gas",xlab="Hari",type='o')

adf.test(Gas_train_diff)

acf(Gas_train_diff, main="Grafik ACF") #MA(1)
pacf(Gas_train_diff, main="Grafik PACF") #AR(1)

#Model yang mungkin ada
# 1. Model AR : ARIMA(1,1,0)
# 2. Model IMA : ARIMA(0,1,1)
# 3. Model ARIMA : ARIMA(1,1,1)

model_ari_gas = arima(Gas_train,order = c(1,1,0))
summary(model_ari_gas) #  aic = 35823.68

model_ima_gas = arima(Gas_train,order = c(0,1,1))
summary(model_ima_gas) # aic = 35822.57

model_arima_gas = arima(Gas_train,order = c(3,1,1))
summary(model_arima_gas) #  aic = 35704.51

model_auto_gas = auto.arima(Gas_train, lambda=0)
summary(model_auto_gas) # AIC=-1993.38  

checkresiduals(model_auto_gas) #   p-value = 0.1704
checkresiduals(model_ari_gas) # p-value = 2.22e-16
checkresiduals(model_ima_gas) # p-value < 2.2e-16
checkresiduals(model_arima_gas) # p-value = 0.02704

# Didapatkan model terbaik itu model auto yakni model arima(1,1,1)
(prediksi = forecast(model_auto_gas, h = 100))
plot(prediksi,main="Grafik Produksi Harian Gas", ylab= "Volume Gas", xlab="Hari",type='o')

# PREDIKSI MENGGUNAKAN DATA TEST
# Kita prediksi sebanyak jumlah data test (h = length(Oil_test))
# Karena lambda=0, fungsi forecast akan otomatis mengembalikan ke skala asli (inverse log).
prediksi_test_gas <- forecast(model_auto_gas, h = length(Gas_test))

# VISUALISASI PERBANDINGAN (FORECAST VS ACTUAL)
# Plot hasil prediksi (Biru)
plot(prediksi_test_gas, 
     main="Validasi Model: Forecast vs Actual Data", 
     ylab="Volume gas (m3/day)", 
     xlab="Waktu")

# Tambahkan garis Data Aktual / Test (Merah)
lines(Gas_test, col="red", lwd=2)

# Tambahkan legenda agar jelas
legend("topleft", legend=c("Prediksi Model", "Data Aktual (Test)"), 
       col=c("blue", "red"), lty=1, lwd=2)

# CEK AKURASI (METRICS)
# Fungsi accuracy() akan membandingkan hasil prediksi dengan Oil_test
akurasi <- accuracy(prediksi_test, Oil_test)
print(akurasi)


