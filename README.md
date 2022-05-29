# P2_Probstat_D_5025201190
**Praktikum 1 Probabilitas dan Statistika** <br>
Nama: Azzura Ferliani Ramadhani <br>
NRP : 5025201190 <br>
Kelas: Probstat - D <br>

## Soal 1
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴 <br>
![image](https://user-images.githubusercontent.com/52819640/170873556-7c893511-197d-40ed-90f8-a96566e3eb60.png)<br>
Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah melakukan aktivitas 𝐴 sebanyak 70.<br>

## 1a. 
Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas
```r
# memasukkan data
x1 <- c(78,75,67,77,70,72,78,74,77)
y1 <- c(100,95,70,90,90,90,89,90,100)
# mencari selisih dari kedua vektor
dif1 <- y1-x1
# mencari standar deviasi dari selisih vektor
standef1 <- sd(dif1)
```
![image](https://user-images.githubusercontent.com/52819640/170873891-a94895d1-d3e6-4f8f-9bf6-383f07d7fa2a.png)

## 1b. 
Carilah nilai t (p-value)
```r
library(BSDA)
t.test(dif1, alternative = 'two.sided', mu=15, sigma.x=standef1)
```
![image](https://user-images.githubusercontent.com/52819640/170873998-7662448f-6d5e-4227-8a95-4fff9863fc4a.png)

## 1c. 
Tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
```r
qt(p = 0.025, df = 8, lower.tail = FALSE)
```
![image](https://user-images.githubusercontent.com/52819640/170874154-099b3fcd-e035-4686-b7bf-83e5d4623d07.png)

## Soal 2
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer. _(Kerjakan menggunakan library seperti referensi pada modul)_.
## 2a.
Apakah Anda setuju dengan klaim tersebut?
```r
library(BSDA)
zsum.test(mean.x=23500, sigma.x=3900, n.x=100, alternative="greater", mu=20000)
```
![image](https://user-images.githubusercontent.com/52819640/170874699-f8c54434-83e3-4ff3-b275-8e3f5afbfc91.png) <br>
Setuju, karena hasilnya lebih dari 20.000.

## 2b.
Jelaskan maksud dari output yang dihasilkan!
> Berdasarkan output yang dihasilkan, diperoleh nilai z hitung = 8.9744
> dan nilai p-value < 2.2e-16. Dari hasil tersebut peneliti dapat menolak hipotesis nol
> dan disimpulkan bahwa terdapat cukup bukti di mana rata-rata secara signifikan lebih besar
> dari 20000. Selain itu, dari output di atas, kita juga peroleh selang
> kepercayaan rata-rata dari sampel adalah 22858.51 atau dapat dinyatakan bahwa dengan selang 
> kepercayaan 95% kita yakin rata-rata weight akan lebih besar sama dengan 22858.51.

## 2c.
Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
> p-value adalah probabilitas untuk memperoleh hasil setidaknya sama ekstremnya dengan yang sekarang, 
> dengan asumsi bahwa hipotesis nol benar. p-value adalah sebuah pengukuran yang dapat memberi tahu kita
> seberapa banyak data yang diamati tidak sesuai dengan hipotesis nol. ketika nilai p sangat rendah,
> data kita tidak sesuai dengan hipotesis nol maka kita akan menolak hipotesis nol. Begitu juga sebaliknya.
> Dikarenakan pada data ini p-valuenya sangat kecil maka kita akan menolak hipotesis nol.

## Soal 3
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.
![image](https://user-images.githubusercontent.com/52819640/170875220-d8c9c6af-6109-4783-ae91-6d7deb8703a3.png)<br>
Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)? Buatlah :
## 3a.
H0 dan H1
> H0: μ Bali = μ Bandung <br>
> H1: μ Bali ≠ μ Bandung <br>

## 3b.
Hitung Sampel Statistik
```r
library(BSDA)
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, mean.y = 2.79, s.y = 1.32, n.y = 27, conf.level = 0.95)
```
![image](https://user-images.githubusercontent.com/52819640/170875514-67dac974-fc88-4a60-aef7-05ee5e62bd08.png)<br>

## 3c.
Lakukan Uji Statistik (df =2)
```r
sp2 = ((27 - 1)*(1.32)^2) + ((19 - 1)*(1.67)^2)/(27 + 19 - 2)
sp <- sqrt(sp2)
x2 <- (1/27) + (1/19)
x <- sqrt(x2)
t <- (2.79 - 3.64)/(sp*x)
```
![image](https://user-images.githubusercontent.com/52819640/170875610-0e842938-556f-4185-b8ff-b2ef3b575842.png)<br>

## 3d.
Nilai kritikal
```r
qt(p = 0.025, df = 2, lower.tail = FALSE)
```
![image](https://user-images.githubusercontent.com/52819640/170875794-32b215fb-93c3-41bb-873a-e7e3b60dfd87.png)

## 3e.
Keputusan
> Hasil uji terletak di interval nilai kritikal, maka H0 tidak ditolak/sample 
> tidak memiliki cukup bukti yang bertentangan dengan H0.

## 3f.
Kesimpulan
> Berdasarkan hasil pengujian, diketahui bahwa rata-rata saham Bali dan Bandung adalah sama.

## Soal 4
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan kucing putih dengan panjangnya masing-masing. <br>
Jika : diketahui dataset https://intip.in/datasetprobstat1 <br>
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama <br>
Maka Kerjakan atau Carilah: <br>

## 4a.
Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.
```r
df4 <- read.delim("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt")

library(ggpubr)
ggboxplot(df4, x = "Group", y = "Length", 
          color = "Group",
          ylab = "Length", xlab = "Group")
```
![image](https://user-images.githubusercontent.com/52819640/170876766-f2e37041-9474-4e5b-8970-7d893c75a372.png)<br>
Tidak ada outlier.

## 4b.
Carilah atau periksalah _Homogeneity of variances_ nya , Berapa nilai p yang didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
```r
library(onewaytests)
bartlett.test(Length ~ Group, data = df4)
```
![image](https://user-images.githubusercontent.com/52819640/170876904-daa90607-9a76-4612-9824-8650c2ab16f2.png) <br>

## 4c.
Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.
```r
Model1 <- lm(formula = Group ~ Length, data = df4)
print(Model1)
```
![image](https://user-images.githubusercontent.com/52819640/170877099-d5bfffc1-cde0-40cf-b9cc-5a3d67718fae.png) <br>

## 4d.
Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
```r
summary(Model1)$coefficients[2,4]
```
![image](https://user-images.githubusercontent.com/52819640/170877150-658b174e-bcc1-4f9b-8157-90ac3c84ac4f.png) <br>

## Soal 5
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil Eksperimen. Dengan data tersebut:

## 5a.
Buatlah plot sederhana untuk visualisasi data
```r
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
```
![image](https://user-images.githubusercontent.com/52819640/170877337-e274469e-ce91-4a9b-8701-1c445aa24b72.png)<br>

## 5b.
Lakukan uji ANOVA dua arah
```r
# membuat variabel sebagai faktor untuk ANOVA
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

# menghitung varians
anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)
```
![image](https://user-images.githubusercontent.com/52819640/170877530-92e2e24e-eb49-4cc9-a2ec-bd36a2e6269f.png) <br>

## 5c.
Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
```r
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)
```
![image](https://user-images.githubusercontent.com/52819640/170877626-0230ba0c-cd70-49ff-82b5-ca5af8c1ad30.png)

## 5d.
Lakukan uji Tukey
```r
tukey <- TukeyHSD(anova)
print(tukey)
```
![image](https://user-images.githubusercontent.com/52819640/170877717-6788065e-a284-4443-9907-957436fc74c2.png)

## 5e.
Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey
```r
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)
```
![image](https://user-images.githubusercontent.com/52819640/170877820-ad19d449-3f27-4be5-8aa3-83d054397dd8.png)




