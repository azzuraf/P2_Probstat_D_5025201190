#1. Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas ???? terhadap kadar saturasi oksigen pada manusia. 
#   Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas ????, peneliti mencatat
#   kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
#   diminta melakukan aktivitas ????. Setelah 15 menit, peneliti tersebut mencatat kembali
#   kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
#   mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas ????

x1 <- c(78,75,67,77,70,72,78,74,77)
y1 <- c(100,95,70,90,90,90,89,90,100)

# Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari
# responden ke-3 ketika belum melakukan aktivitas ???? sebanyak 67, dan setelah
# melakukan aktivitas ???? sebanyak 70.

# a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas!
# mencari selisih dari kedua vektor
dif1 <- y1-x1
# mencari standar deviasi dari selisih vektor
standef1 <- sd(dif1)

# b. Carilah nilai t (p-value)
library(BSDA)
t.test(dif1, alternative = 'two.sided', mu=15, sigma.x=standef1)

# c. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
# dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
# aktivitas ???? jika diketahui tingkat signifikansi ???? = 5% serta H0 : "tidak ada
# pengaruh yang signifikan secara statistika dalam hal kadar saturasi
# oksigen , sebelum dan sesudah melakukan aktivitas ????"

qt(p = 0.025, df = 8, lower.tail = FALSE)


#2. Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini,
#   100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh.
#   Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer.

# a. Apakah Anda setuju dengan klaim tersebut?

library(BSDA)
zsum.test(mean.x=23500, sigma.x=3900, n.x=100, alternative="greater", mu=20000)

# b. Jelaskan maksud dari output yang dihasilkan!

# Berdasarkan output yang dihasilkan, diperoleh nilai z hitung = 8.9744
# dan nilai p-value < 2.2e-16. Dari hasil tersebut peneliti dapat menolak hipotesis nol
# dan disimpulkan bahwa terdapat cukup bukti di mana rata-rata secara signifikan lebih besar
# dari 20000. Selain itu, dari output di atas, kita juga peroleh selang
# kepercayaan rata-rata dari sampel adalah 22858.51 atau dapat dinyatakan bahwa dengan selang 
# kepercayaan 95% kita yakin rata-rata weight akan lebih besar sama dengan 22858.51.

# c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!

# p-value adalah probabilitas untuk memperoleh hasil setidaknya sama ekstremnya dengan yang sekarang, 
# dengan asumsi bahwa hipotesis nol benar. p-value adalah sebuah pengukuran yang dapat memberi tahu kita
# seberapa banyak data yang diamati tidak sesuai dengan hipotesis nol. ketika nilai p sangat rendah,
# data kita tidak sesuai dengan hipotesis nol maka kita akan menolak hipotesis nol. Begitu juga sebaliknya.
# Dikarenakan pada data ini p-valuenya sangat kecil maka kita akan menolak hipotesis nol.

#3. Diketahui perusahaan memiliki seorang data analyst ingin memecahkan
#   permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
#   didapatkanlah data berikut dari perusahaan saham tersebut.

#   Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil
#   diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada
#   rata-ratanya (??= 0.05)? Buatlah :
# a. H0 dan H1
# H0: ?? Bali = ?? Bandung
# H1: ?? Bali ??? ?? Bandung

# b. Hitung sample statistik
library(BSDA)
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, mean.y = 2.79, s.y = 1.32, n.y = 27, conf.level = 0.95)

# c. Lakukan uji statistik (df = 2)
sp2 = ((27 - 1)*(1.32)^2) + ((19 - 1)*(1.67)^2)/(27 + 19 - 2)
sp <- sqrt(sp2)
x2 <- (1/27) + (1/19)
x <- sqrt(x2)
t <- (2.79 - 3.64)/(sp*x)

#val: -1.926715

# d. Nilai kritikal
qt(p = 0.025, df = 2, lower.tail = FALSE)

#val: (-4.302653, 4.302653)

# e. Keputusan
# Hasil uji terletak di interval nilai kritikal, maka H0 tidak ditolak/sample 
# tidak memiliki cukup bukti yang bertentangan dengan H0.

# f. Kesimpulan
# Berdasarkan hasil pengujian, diketahui bahwa rata-rata saham Bali dan Bandung adalah sama.

#4. Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
#   ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
#   kucing putih dengan panjangnya masing-masing.
#   Jika : diketahui dataset https://intip.in/datasetprobstat1
#   H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama

# a. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup 2,grup 3). 
# Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
# lihat apakah ada outlier utama dalam homogenitas varians.

df4 <- read.delim("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt")

library(ggpubr)
ggboxplot(df4, x = "Group", y = "Length", 
          color = "Group",
          ylab = "Length", xlab = "Group")

# Tidak ada outlier

# b. carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
# didapatkan?, Apa hipotesis dan kesimpulan yang dapat diambil ?

library(onewaytests)
bartlett.test(Length ~ Group, data = df4)

# c. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
# Grup dan beri nama model tersebut model 1.

Model1 <- lm(formula = Group ~ Length, data = df4)
print(Model1)

# d. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?

summary(Model1)$coefficients[2,4]

#5. Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk
#   mengetahui pengaruh suhu operasi (100??C, 125??C dan 150??C) dan tiga jenis kaca
#   pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan
#   dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil
#   Eksperimen. Dengan data tersebut:

# a. Buatlah plot sederhana untuk visualisasi data

library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

# membaca dan mengecek data
GTL <- read_csv("https://drive.google.com/u/0/uc?id=1aLUOdw_LVJq6VQrQEkuQhZ8FW43FemTJ&export=download")
head(GTL)
str(GTL)

# membuat plot sederhana
qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

# b. Lakukan uji ANOVA dua arah
# membuat variabel sebagai faktor untuk ANOVA
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

# menghitung varians
anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

# c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
# setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

# d. Lakukan uji Tukey
tukey <- TukeyHSD(anova)
print(tukey)

# e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan
# antara uji Anova dan uji Tukey

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)
