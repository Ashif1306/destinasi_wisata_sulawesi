# 🌏 Objek Wisata Pulau Sulawesi - Dashboard Analisis & Prediksi

Dashboard interaktif berbasis **R Shiny** yang dirancang untuk melakukan eksplorasi, segmentasi, dan prediksi destinasi wisata di Pulau Sulawesi. Aplikasi ini menggabungkan teknik Machine Learning (**K-Means Clustering** & **Random Forest**) untuk memberikan wawasan mendalam bagi wisatawan maupun pengelola pariwisata.

## 🚀 Fitur Utama

### 1. 📊 Overview Dashboard
*   **Peta Interaktif**: Visualisasi persebaran destinasi menggunakan Leaflet.
*   **Cascading Filter**: Filter cerdas berbasis Provinsi dan Kabupaten yang sinkron secara otomatis, dilengkapi dengan pencarian destinasi.
*   **Visualisasi Data**: Menampilkan insight wisata menggunakan metrik yang relevan (Value Boxes, dll).
*   **Cari Destinasi**: Fitur pencarian interaktif yang responsif di Desktop maupun Mobile.

### 2. 📐 Segmentasi K-Means
*   **Analisis Elbow & Silhouette**: Menentukan jumlah cluster (K) yang optimal secara statistik.
*   **Visualisasi Cluster PCA (2D & 3D)**: Interaktivitas tinggi untuk melihat persebaran cluster berdasarkan karakteristik destinasi wisata (rating, jumlah review, harga).
*   **Justifikasi Segmentasi**: Analisis ringkasan klaster seperti Destinasi Premium, Paling Populer, Rating Tertinggi, dan Wisata Terjangkau.

### 3. 🌲 Prediksi Random Forest (RF)
*   **Model Klasifikasi**: Memprediksi label kualitas rekomendasi (*Terbaik, Baik, Sedang, Buruk*) menggunakan skor berbasis Bayesian Average.
*   **Evaluasi Model**: Menampilkan Confusion Matrix, Feature Importance, OOB Error, serta Precision & Recall.
*   **Simulator Prediksi**: Masukkan parameter destinasi (rating, jumlah review, harga, kategori harga) untuk mendapatkan prediksi rekomendasi instan dari model.

### 4. 🤖 Asisten Cerdas (Sylva Chatbot)
*   **Chatbot AI**: Terintegrasi untuk menjawab pertanyaan seputar destinasi wisata secara real-time.

### 5. 📋 Dataset & Manajemen
*   **Tabel Dataset**: Menampilkan data yang telah diproses, diimputasi, dan dicluster, dengan kapabilitas filtering & pencarian penuh.

## 🛠️ Teknologi yang Digunakan

*   **R & Shiny**: Framework utama aplikasi.
*   **Shinydashboard**: Struktur UI admin dashboard yang responsif.
*   **Leaflet**: Pemetaan interaktif.
*   **Plotly**: Grafik visualisasi yang interaktif (2D & 3D).
*   **RandomForest & Caret**: Implementasi machine learning untuk klasifikasi.
*   **Tidyverse**: Manipulasi dan transformasi data.
*   **Supabase**: Sumber data utama yang digunakan dalam backend.

## 📦 Instalasi & Cara Menjalankan

1.  Pastikan telah menginstal [R](https://cran.r-project.org/) dan [RStudio](https://posit.co/download/rstudio-desktop/).
2.  Clone atau unduh repositori ini.
3.  Buka file `app.R` di RStudio.
4.  Instal dependensi yang diperlukan dengan menjalankan perintah berikut di Console R:
    ```r
    install.packages(c("shiny", "shinydashboard", "tidyverse", "leaflet", "randomForest", "caret", "plotly", "DT", "cluster", "httr", "jsonlite", "shinyjs", "shinyWidgets", "shinyanimate", "shinycssloaders"))
    ```
5.  Pastikan juga file `.Renviron` tersedia dengan kredensial Supabase (`SUPABASE_URL` dan `SUPABASE_KEY`).
6.  Klik tombol **Run App** di pojok kanan atas editor RStudio pada file `app.R`.

## 📂 Struktur Data
Aplikasi menggunakan dataset `df_final` yang diambil dari Supabase dan berisi informasi seperti:
*   `nama_wisata`, `kabupaten`, `provinsi`
*   `rating`, `jumlah_riview`, `harga_num`
*   `label_rekomendasi` (Target Model RF berbasis Bayesian Average)
*   `cluster` (Hasil K-Means)

---
Dibuat untuk tugas Machine Learning - Kelompok 8
