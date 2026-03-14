# Analisis Produksi Sumur № 807 (2013-2021) Dengan Pendekatan Statistika 
![R Language](https://img.shields.io/badge/Language-R-276DC3?logo=r&logoColor=white)
![ARIMA Model](https://img.shields.io/badge/Model-ARIMA-0073B7?style=flat-square)

Repository ini berisi Model Time Series dan Regresi Linear yang digunakan untuk melakukan forecasting produksi pada Sumur № 807 untuk periode tahun 2013-2021. Dataset ini diperoleh dari Kaggle (//www.kaggle.com/datasets/ruslanzalevskikh/oil-well
).
Kami dari Kelompok 8 menggunakan bahasa pemrograman R untuk membangun model-model ini berdasarkan materi yang telah kami pelajari dalam Praktikum Statistika Dasar.

# Ringkasan Proyek
Proyek ini melakukan evaluasi teknis terhadap data produksi harian sumur minyak, meliputi:
1. Statistika Deskriptif: Mengidentifikasi karakteristik sebaran data dan mendeteksi pencilan (outlier).
2. Uji Hipotesis: Menguji rata-rata volume produksi minyak dan gas pada tingkat signifikansi 1%, 5%, dan 10%.
3. Regresi Linear: Menganalisis besarnya pengaruh reservoir pressure dan water cut terhadap volume produksi.
4. Analisis Deret Waktu: Melakukan peramalan (forecasting) tren penurunan produksi menggunakan model ARIMA.

# Temuan Utama
1. Distribusi Data: Volume minyak dan gas menunjukkan karakteristik menceng positif (positively skewed), sementara water cut memiliki kecenderungan menceng negatif yang kuat.
2. Korelasi & Regresi: Tekanan reservoir terbukti menjadi pendorong utama produksi dengan koefisien determinasi R^2 sebesar 0.7431 untuk minyak. Sebaliknya, water cut memiliki hubungan kausalitas negatif yang signifikan.
3. Model Terbaik: Untuk variabel minyak dan gas, model terbaik yang diperoleh adalah ARIMA(1,1,1). Model ini mampu menangkap tren penurunan (decline trend) secara efektif.
