# 🌏 Objek Wisata Pulau Sulawesi - Dashboard Analisis & Prediksi

Dashboard interaktif berbasis **R Shiny** yang dirancang untuk melakukan eksplorasi, segmentasi, dan prediksi destinasi wisata di Pulau Sulawesi. Aplikasi ini menggabungkan teknik Machine Learning (**K-Means Clustering** & **Random Forest**) untuk memberikan wawasan mendalam bagi wisatawan maupun pengelola pariwisata.

## 🚀 Fitur Utama

### 1. 📊 Overview Dashboard
*   **Peta Interaktif**: Visualisasi persebaran destinasi menggunakan Leaflet dengan pengelompokan (clustering) geografis.
*   **Cascading Filter**: Filter cerdas berbasis Provinsi dan Kabupaten yang sinkron secara otomatis.
*   **Visualisasi Data**: Grafik batang untuk kategori wisata populer dan diagram donat untuk proporsi label rekomendasi berdasarkan model Random Forest.
*   **Cari Destinasi**: Fitur pencarian cepat untuk menemukan objek wisata spesifik.

### 2. 📐 Segmentasi K-Means
*   **Analisis Elbow & Silhouette**: Menentukan jumlah cluster (K) yang optimal secara statistik.
*   **Visualisasi Cluster**: Pengelompokan destinasi berdasarkan karakteristik fitur seperti rating, harga, dan jumlah review.

### 3. 🌲 Prediksi Random Forest (RF)
*   **Model Klasifikasi**: Memprediksi label rekomendasi (*Terbaik, Baik, Sedang, Buruk*) berdasarkan fitur-fitur destinasi.
*   **Evaluasi Model**: Menampilkan Confusion Matrix, Precision, Recall, dan tingkat konvergensi OOB Error.
*   **Simulator Prediksi**: Masukkan parameter destinasi (koordinat, rating, harga) untuk mendapatkan rekomendasi instan dari model.

## 🛠️ Teknologi yang Digunakan

*   **R & Shiny**: Framework utama aplikasi.
*   **Shinydashboard**: Struktur UI admin dashboard yang responsif.
*   **Leaflet**: Pemetaan interaktif.
*   **Plotly**: Grafik visualisasi yang interaktif.
*   **RandomForest & Caret**: Implementasi machine learning untuk klasifikasi.
*   **Tidyverse**: Manipulasi dan transformasi data (dplyr, ggplot2, tidyr).

## 📦 Instalasi & Cara Menjalankan

1.  Pastikan Anda telah menginstal [R](https://cran.r-project.org/) dan [RStudio](https://posit.co/download/rstudio-desktop/).
2.  Clone atau unduh repositori ini.
3.  Buka file `app.R` di RStudio.
4.  Instal dependensi yang diperlukan dengan menjalankan perintah berikut di Console R:
    ```r
    install.packages(c("shiny", "shinydashboard", "leaflet", "plotly", "dplyr", "randomForest", "caret", "tidyverse", "DT", "htmlwidgets"))
    ```
5.  Klik tombol **Run App** di pojok kanan atas editor RStudio.

## 📂 Struktur Data
Aplikasi menggunakan dataset `df_clustered` yang berisi informasi:
*   `nama_wisata`, `kabupaten`, `provinsi`
*   `rating`, `jumlah_riview`, `harga_num`
*   `label_rekomendasi` (Target Model RF)
*   `cluster` (Hasil K-Means)

---
*Dibuat untuk tugas Machine Learning - Semester 6.*
