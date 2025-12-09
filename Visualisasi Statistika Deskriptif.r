#Oil volume (m3/day)
library(readxl)
data <- read_excel("/Users/vella/Desktop/ITB/Semester 3/Statistika Dasar/Tubes/clean_oil_well_data.xlsx")

# CEK NAMA KOLOM
names(data)

# MISAL kolomnya bernama 'oil_volume'
oil <- data$`Oil volume (m3/day)`
oil <- as.numeric(oil)   # pastikan numeric

# ==============================
# 1. HISTOGRAM
# ==============================
hist(
  oil,
  freq = TRUE,
  main = "Histogram Oil Volume (m3/day)",
  xlab = "Oil Volume (m3/day)",
  ylab = "Frekuensi"
)

# ==============================
# 2. STEM & LEAF (Batang Daun)
# ==============================
stem(oil)

# ==============================
# 3. BOX PLOT
# ==============================
boxplot(
  oil,
  horizontal = TRUE,
  main = "Boxplot Oil Volume (m3/day)",
  xlab = "Oil Volume (m3/day)"
)

# ==============================
# 4. QQ PLOT
# ==============================
qqnorm(oil,
       main = "QQ Plot Oil Volume")
qqline(oil, col = 2)

# ==============================
# 5. PP PLOT
# ==============================
library(CircStats)
pp.plot(oil, ref.line = TRUE)




#Gas volume (m3/day)
library(readxl)
data <- read_excel("/Users/vella/Desktop/ITB/Semester 3/Statistika Dasar/Tubes/clean_oil_well_data.xlsx")

# CEK NAMA KOLOM
names(data)

# MISAL kolomnya bernama 'Gas volume (m3/day)'
gas <- data$`Gas volume (m3/day)`
gas <- as.numeric(gas)   # pastikan numeric

# ==============================
# 1. HISTOGRAM
# ==============================
hist(
  gas,
  freq = TRUE,
  main = "Histogram Gas Volume (m3/day)",
  xlab = "Gas Volume (m3/day)",
  ylab = "Frekuensi"
)

# ==============================
# 2. STEM & LEAF (Batang Daun)
# ==============================
stem(gas)

# ==============================
# 3. BOX PLOT
# ==============================
boxplot(
  gas,
  horizontal = TRUE,
  main = "Boxplot Gas Volume (m3/day)",
  xlab = "Gas Volume (m3/day)"
)

# ==============================
# 4. QQ PLOT
# ==============================
qqnorm(gas,
       main = "QQ Plot Gas Volume")
qqline(gas, col = 2)

# ==============================
# 5. PP PLOT
# ==============================
library(CircStats)
pp.plot(gas, ref.line = TRUE)



#Water cut (%)
library(readxl)
data <- read_excel("/Users/vella/Desktop/ITB/Semester 3/Statistika Dasar/Tubes/clean_oil_well_data.xlsx")

# CEK NAMA KOLOM
names(data)

# MISAL kolomnya bernama 'Water cut (%)'
watercut <- data$`Water cut (%)`
watercut <- as.numeric(watercut)   # pastikan numeric

# ==============================
# 1. HISTOGRAM
# ==============================
hist(
  watercut,
  freq = TRUE,
  main = "Histogram Water Cut (%)",
  xlab = "Water Cut (%)",
  ylab = "Frekuensi"
)

# ==============================
# 2. STEM & LEAF (Batang Daun)
# ==============================
stem(watercut)

# ==============================
# 3. BOX PLOT
# ==============================
boxplot(
  watercut,
  horizontal = TRUE,
  main = "Boxplot Water Cut (%)",
  xlab = "Water Cut (%)"
)

# ==============================
# 4. QQ PLOT
# ==============================
qqnorm(watercut,
       main = "QQ Plot Water Cut (%)")
qqline(watercut, col = 2)

# ==============================
# 5. PP PLOT
# ==============================
library(CircStats)
pp.plot(watercut, ref.line = TRUE)


#Reservoir pressure (atm)
library(readxl)
data <- read_excel("/Users/vella/Desktop/ITB/Semester 3/Statistika Dasar/Tubes/clean_oil_well_data.xlsx")

# CEK NAMA KOLOM
names(data)

# MISAL kolomnya bernama 'Reservoir pressure (atm)'
pressure <- data$`Reservoir pressure (atm)`
pressure <- as.numeric(pressure)   # pastikan numeric

# ==============================
# 1. HISTOGRAM
# ==============================
hist(
  pressure,
  freq = TRUE,
  main = "Histogram Reservoir Pressure (atm)",
  xlab = "Reservoir Pressure (atm)",
  ylab = "Frekuensi"
)

# ==============================
# 2. STEM & LEAF (Batang Daun)
# ==============================
stem(pressure)

# ==============================
# 3. BOX PLOT
# ==============================
boxplot(
  pressure,
  horizontal = TRUE,
  main = "Boxplot Reservoir Pressure (atm)",
  xlab = "Reservoir Pressure (atm)"
)

# ==============================
# 4. QQ PLOT
# ==============================
qqnorm(pressure,
       main = "QQ Plot Reservoir Pressure (atm)")
qqline(pressure, col = 2)

# ==============================
# 5. PP PLOT
# ==============================
library(CircStats)
pp.plot(pressure, ref.line = TRUE)

