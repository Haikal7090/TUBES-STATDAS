library(readxl)
clean_oil_well_data <- read_excel("C:/Users/Asus/Downloads/clean_oil_well_data.xlsx")
View(clean_oil_well_data)

Oil_volume = clean_oil_well_data$`Oil volume (m3/day)`
Gas_Volume = clean_oil_well_data$`Gas volume (m3/day)`
(Oilbar= mean(Oil_volume))
(Gasbar= mean(Gas_Volume))
muOil= 20
muGas= 5000
(sOil= sd(Oil_volume))
(sGas= sd(Gas_Volume))
(nOil= length(Oil_volume))
(nGas= length(Gas_Volume))
alpha1= 0.01
alpha2= 0.05
alpha3= 0.1

#Nilai T tabel dengan alpha 1%, 5%, 10%
(t.upper = qt(alpha1,df= nOil-1))
(t.upper = qt(alpha2,df= nOil-1))
(t.upper = qt(alpha3,df= nOil-1))
(t.lower = qt(alpha1,df= nGas-1))
(t.lower = qt(alpha2,df= nGas-1))
(t.lower = qt(alpha3,df= nGas-1))

#Uji dugaan pada oil volume dengan alpha 1%, 5%, 10%
t.test(Oil_volume, mu=muOil, alternative = "greater",
       conf.level = 0.99)
t.test(Oil_volume, mu=muOil, alternative = "greater",
       conf.level = 0.95)
t.test(Oil_volume, mu=muOil, alternative = "greater",
       conf.level = 0.9)

#Uji dugaan pada gas volume dengan alpha 1%, 5%, 10%
t.test(Gas_Volume, mu=muGas, alternative = "less",
       conf.level = 0.99)
t.test(Gas_Volume, mu=muGas, alternative = "less",
       conf.level = 0.95)
t.test(Gas_Volume, mu=muGas, alternative = "less",
       conf.level = 0.9)