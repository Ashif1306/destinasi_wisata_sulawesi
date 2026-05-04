# ============================================================
# Analisis Destinasi Wisata Sulawesi
# Prediksi Rekomendasi (Random Forest) & Segmentasi (K-Means)
# ============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(randomForest)
library(caret)
library(plotly)
library(DT)
library(cluster)
library(httr)
library(jsonlite)
library(shinyjs)
library(shinyWidgets)
library(shinyanimate)
library(shinycssloaders)
library(openxlsx)

source("chatbot.R")

# Muat environment variables dari .Renviron (API keys, dll.)
readRenviron(".Renviron")

# ============================================================
# GLOBAL: Supabase Config & Data Loading
# ============================================================

SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
SUPABASE_KEY <- Sys.getenv("SUPABASE_KEY")
SUPABASE_TABLE <- "tourism_data"

# Fungsi fetch semua data dari Supabase dengan pagination
fetch_supabase <- function() {
  all_rows <- list()
  page_size <- 1000
  offset <- 0

  repeat {
    url <- paste0(
      SUPABASE_URL, "/rest/v1/", SUPABASE_TABLE,
      "?select=*&limit=", page_size, "&offset=", offset
    )
    resp <- GET(
      url,
      add_headers(
        apikey        = SUPABASE_KEY,
        Authorization = paste("Bearer", SUPABASE_KEY),
        Accept        = "application/json"
      )
    )

    if (http_error(resp)) {
      warning("Supabase request error: ", status_code(resp))
      break
    }

    batch <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)

    if (length(batch) == 0 || nrow(batch) == 0) break

    all_rows[[length(all_rows) + 1]] <- batch
    offset <- offset + page_size

    if (nrow(batch) < page_size) break
  }

  if (length(all_rows) == 0) stop("Tidak ada data yang berhasil diambil dari Supabase.")
  bind_rows(all_rows)
}

load_and_process <- function() {
  df <- tryCatch(
    fetch_supabase(),
    error = function(e) {
      message("Gagal mengambil data dari Supabase: ", e$message)
      stop(e)
    }
  )

  # Normalisasi nama kolom (lowercase & trim)
  colnames(df) <- tolower(trimws(colnames(df)))

  # Cleaning: filter jumlah_riview >= 15
  # (wisata dengan review < 15 dianggap belum cukup data untuk analisis)
  # Cek nama kolom untuk menghindari error jika nama berbeda
  if ("jumlah_riview" %in% colnames(df)) {
    df <- df %>% filter(jumlah_riview >= 15)
  } else if ("jumlah_review" %in% colnames(df)) {
    df <- df %>%
      rename(jumlah_riview = jumlah_review) %>%
      filter(jumlah_riview >= 15)
  } else if ("reviews" %in% colnames(df)) {
    df <- df %>%
      rename(jumlah_riview = reviews) %>%
      filter(jumlah_riview >= 15)
  }

  # Cleaning: hapus wisata yang tidak memiliki data harga
  # (NA, string kosong, atau tanda "-")
  if ("harga" %in% colnames(df)) {
    df <- df %>% filter(
      !is.na(harga) &
        trimws(as.character(harga)) != "" &
        trimws(as.character(harga)) != "-"
    )
  }

  # Bersihkan kolom harga menjadi numerik
  df$harga_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", df$harga)))
  df$harga_num[is.na(df$harga_num)] <- 0

  # Bersihkan lat/long
  df$lat <- suppressWarnings(as.numeric(df$lat))
  df$long <- suppressWarnings(as.numeric(df$long))
  df <- df %>% filter(!is.na(lat) & !is.na(long))

  # Bersihkan rating
  df$rating <- suppressWarnings(as.numeric(df$rating))
  df$rating[is.na(df$rating)] <- median(df$rating, na.rm = TRUE)

  # Gunakan 4 kelas kategori_harga langsung dari data asli
  # (Gratis / Murah / Sedang / Mahal)
  # Baris dengan kategori_harga kosong ("-") atau NA ditandai NA
  df$kategori_harga[is.na(df$kategori_harga) | trimws(df$kategori_harga) == "" | trimws(df$kategori_harga) == "-"] <- NA

  # Feature engineering tambahan
  df$log_review <- log1p(df$jumlah_riview)

  df
}

df_raw <- load_and_process()

# ============================================================
# K-MEANS CLUSTERING
# ============================================================

run_kmeans <- function(df, k = 4) {
  # Fitur clustering (fitur MENTAH + StandardScaler):
  #   - rating         : tingkat kepuasan pengunjung (1-5)
  #   - jumlah_riview  : popularitas destinasi (jumlah ulasan)
  #   - harga_num      : harga tiket masuk dalam IDR
  #
  # TIDAK menggunakan log-transform: log1p(harga) meratakan perbedaan
  # skala harga sehingga cluster "Destinasi Premium" larut ke cluster lain.
  # StandardScaler sudah cukup untuk normalisasi antara ketiga fitur.
  #
  # Catatan:
  #   - lat & long TIDAK digunakan: lokasi geografis tidak relevan
  #     untuk segmentasi kualitas/popularitas/harga wisata.

  feat <- df %>%
    select(rating, jumlah_riview, harga_num)

  feat_scaled <- scale(feat)
  set.seed(42)
  km <- kmeans(feat_scaled, centers = k, nstart = 25, iter.max = 100)

  df$cluster <- as.factor(km$cluster)

  list(df = df, km = km, scaled = feat_scaled)
}

# Elbow Method & Silhouette: gunakan fitur SAMA dengan run_kmeans
# (rating, jumlah_riview, harga_num — fitur mentah + StandardScaler)
feat_for_elbow <- scale(df_raw %>% select(rating, jumlah_riview, harga_num))
set.seed(42)

max_k <- 10
wss_values <- numeric(max_k)
sil_values <- numeric(max_k)
dist_matrix <- dist(feat_for_elbow)

for (k in 1:max_k) {
  km_temp <- kmeans(feat_for_elbow, centers = k, nstart = 25, iter.max = 100)
  wss_values[k] <- km_temp$tot.withinss
  if (k > 1) {
    sil <- cluster::silhouette(km_temp$cluster, dist_matrix)
    sil_values[k] <- mean(sil[, 3])
  } else {
    sil_values[k] <- NA
  }
}

# ---- Titik Elbow Visual (k=4) ----
# Formula turunan kedua memberi indikasi k=5 karena mengukur akselerasi
# matematis, bukan siku visual. Secara grafis, penurunan WSS paling tajam
# terjadi di k=4 (penurunan besar k=3→k=4, melambat signifikan k=4→k=5).
# Ditetapkan manual sesuai inspeksi visual kurva.
best_k_elbow <- 4

# ---- Optimal k dari Silhouette (minimum k=3) ----
# k=2 diabaikan: 2 segmen hanya bisa membagi "baik vs buruk",
# tidak cukup granular untuk rekomendasi 4 dimensi wisata.
# Cari k dengan silhouette tertinggi dari k=3 ke atas.
sil_from3 <- sil_values[3:max_k]
best_k_sil <- which.max(sil_from3) + 2  # index mapping: +2 karena mulai dari k=3

# ---- Persen penurunan WSS per step (untuk penjelasan di justifikasi) ----
wss_diff1    <- diff(wss_values)        # selisih WSS antar k (panjang 9)
wss_pct_drop <- round(-wss_diff1 / wss_values[1:(max_k - 1)] * 100, 1)

elbow_df <- data.frame(
  k           = 1:max_k,
  WSS         = wss_values,
  Silhouette  = sil_values
)

kmeans_result <- run_kmeans(df_raw)
df_clustered <- kmeans_result$df %>%
  mutate(provinsi = as.factor(provinsi))

# Cluster labels berdasarkan karakteristik
# Dengan log-transform, avg_harga mentah bisa menyesatkan karena cluster
# yang berisi banyak wisata gratis akan punya avg_harga = 0 meski punya
# beberapa wisata mahal. Digunakan skor komposit multi-dimensi.
cluster_summary <- df_clustered %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    n            = n(),
    avg_rating   = round(mean(rating, na.rm = TRUE), 2),
    avg_reviews  = round(mean(jumlah_riview, na.rm = TRUE), 0),
    avg_harga    = round(mean(harga_num, na.rm = TRUE), 0),
    med_harga    = round(median(harga_num, na.rm = TRUE), 0),
    pct_berbayar = round(mean(harga_num > 0, na.rm = TRUE) * 100, 1),
    .groups      = "drop"
  ) %>%
  mutate(
    # Skor premium: kombinasi harga median + persentase berbayar + rating
    # Dinormalisasi 0-1 per dimensi agar tidak didominasi skala harga
    score_harga   = (med_harga    - min(med_harga))    / (max(med_harga)    - min(med_harga)    + 1),
    score_berbayar= (pct_berbayar - min(pct_berbayar)) / (max(pct_berbayar) - min(pct_berbayar) + 1e-9),
    score_rating  = (avg_rating   - min(avg_rating))   / (max(avg_rating)   - min(avg_rating)   + 1e-9),
    score_reviews = (avg_reviews  - min(avg_reviews))  / (max(avg_reviews)  - min(avg_reviews)  + 1),
    # Skor premium: harga & pct_berbayar bobot tinggi, rating bobot sedang
    score_premium = 0.45 * score_harga + 0.35 * score_berbayar + 0.20 * score_rating,
    label = {
      lbl      <- character(n())
      assigned <- logical(n())

      # 1) Destinasi Premium → skor komposit (harga median + % berbayar + rating) tertinggi
      #    Tidak hanya avg_harga karena bisa terdistorsi cluster besar dgn banyak wisata gratis
      avail <- which(!assigned)
      pick  <- avail[which.max(score_premium[avail])]
      lbl[pick]      <- "\U0001F48E Destinasi Premium"
      assigned[pick] <- TRUE

      # 2) Paling Populer → avg_reviews tertinggi dari yang tersisa
      avail <- which(!assigned)
      pick  <- avail[which.max(avg_reviews[avail])]
      lbl[pick]      <- "\U0001F525 Paling Populer"
      assigned[pick] <- TRUE

      # 3) Rating Tertinggi → avg_rating tertinggi dari yang tersisa
      avail <- which(!assigned)
      pick  <- avail[which.max(avg_rating[avail])]
      lbl[pick]      <- "\u2B50 Rating Tertinggi"
      assigned[pick] <- TRUE

      # 4) Sisa → Wisata Terjangkau (tepat 1 klaster untuk k=4)
      lbl[!assigned] <- "\U0001F33F Wisata Terjangkau"

      lbl
    }
  ) %>%
  select(-starts_with("score_"))  # buang kolom bantu skor dari output

# ============================================================
# RANDOM FOREST – Train & Impute
# ============================================================

train_rf <- function(df) {
  # Target: label_rekomendasi (4 kelas: Terbaik / Baik / Sedang / Buruk)
  # Label ini dihitung dari skor komposit berbasis rating, review,
  # harga, dan kategori harga.
  # Catatan: lat & long TIDAK digunakan — lokasi geografis tidak
  # relevan untuk klasifikasi kualitas/rekomendasi destinasi wisata.
  df_train <- df %>%
    filter(!is.na(label_rekomendasi)) %>%
    mutate(
      label_rekomendasi = factor(label_rekomendasi,
        levels = c("Terbaik", "Baik", "Sedang", "Buruk")
      ),
      kategori_harga = factor(kategori_harga,
        levels = c("Gratis", "Murah", "Sedang", "Mahal")
      )
    ) %>%
    filter(!is.na(kategori_harga)) # hapus baris tanpa kategori_harga

  # Fitur analisis:
  #   - rating             : tingkat kepuasan pengunjung
  #   - jumlah_riview      : popularitas destinasi
  #   - harga_num          : harga tiket masuk (numerik)
  #   - kategori_harga     : kategori harga (Gratis/Murah/Sedang/Mahal)
  features <- c("rating", "jumlah_riview", "harga_num", "kategori_harga")
  df_train <- df_train %>%
    drop_na(all_of(features))

  set.seed(42)
  idx <- createDataPartition(df_train$label_rekomendasi, p = 0.8, list = FALSE)
  train <- df_train[idx, ]
  test <- df_train[-idx, ]

  # Balanced class weights: kelas minoritas mendapat bobot lebih tinggi
  # agar model tidak bias hanya memprediksi kelas mayoritas.
  class_counts <- table(train$label_rekomendasi)
  class_weights <- max(class_counts) / class_counts

  rf_model <- randomForest(
    label_rekomendasi ~ rating + jumlah_riview + harga_num + kategori_harga,
    data        = train,
    ntree       = 500,
    mtry        = 2,
    classwt     = class_weights,
    importance  = TRUE,
    keep.forest = TRUE
  )

  preds <- predict(rf_model, test)
  cm <- confusionMatrix(preds, test$label_rekomendasi)

  list(model = rf_model, cm = cm, train = train, test = test)
}

rf_result <- train_rf(df_clustered)
rf_model <- rf_result$model
rf_cm <- rf_result$cm

# OOB Error Rate per jumlah tree (untuk grafik konvergensi)
oob_df <- data.frame(
  trees     = 1:rf_model$ntree,
  oob_error = rf_model$err.rate[, "OOB"] * 100
)

# Per-Class Precision & Recall dari confusion matrix
per_class_df <- data.frame(
  Class     = rownames(rf_cm$byClass),
  Precision = round(rf_cm$byClass[, "Precision"] * 100, 1),
  Recall    = round(rf_cm$byClass[, "Recall"] * 100, 1)
) %>%
  mutate(Class = gsub("Class: ", "", Class)) %>%
  pivot_longer(cols = c(Precision, Recall), names_to = "Metric", values_to = "Value")

# Dataset final — tidak ada imputasi, menggunakan data asli.
# label_rekomendasi sudah tersedia dari proses feature engineering Python.
df_final <- df_clustered %>%
  mutate(
    kategori_harga = factor(kategori_harga,
      levels = c("Gratis", "Murah", "Sedang", "Mahal")
    ),
    label_rekomendasi = factor(label_rekomendasi,
      levels = c("Terbaik", "Baik", "Sedang", "Buruk")
    )
  )

# ============================================================
# DF_CHATBOT: df_final + pastikan alamat & deskripsi tersedia
# df_final sudah mewarisi kolom-kolom ini dari df_raw melalui
# pipeline: df_raw → df_clustered → df_final
# Tidak perlu join — cukup pastikan kolom ada & isi fallback.
# ============================================================
df_chatbot <- df_final %>%
  mutate(
    alamat = if ("alamat" %in% colnames(df_final)) {
      ifelse(is.na(alamat) | trimws(as.character(alamat)) == "",
        "Alamat belum tersedia.", as.character(alamat)
      )
    } else {
      "Alamat belum tersedia."
    },
    deskripsi_wisata = if ("deskripsi_wisata" %in% colnames(df_final)) {
      ifelse(is.na(deskripsi_wisata) | trimws(as.character(deskripsi_wisata)) == "",
        "Deskripsi belum tersedia.", as.character(deskripsi_wisata)
      )
    } else {
      "Deskripsi belum tersedia."
    }
  )

# ============================================================
# PCA untuk visualisasi K-Means
# Clustering pakai fitur MENTAH (rating, jumlah_riview, harga_num).
# harga_num hanya punya 47 nilai unik → scale() masih menghasilkan
# stripe diagonal di scatter PCA.
# Solusi: setelah scale(), tambahkan jitter kecil (sd=0.15) pada kolom
# harga_num HANYA untuk keperluan tampilan — cluster tidak berubah.
# ============================================================

df_pca_base <- df_final %>%
  filter(!is.na(cluster))

# --- Matriks fitur untuk PCA (identik dengan run_kmeans) ---
pca_feat_scaled <- scale(
  df_pca_base %>%
    select(rating, jumlah_riview, harga_num)
)

# --- Jitter visual pada harga (kolom ke-3) SETELAH scale() ---
# sd=0.15 ≈ 5% range → mengaburkan 47 jalur diskrit tanpa geser cluster
set.seed(123)
pca_feat_jittered <- pca_feat_scaled
pca_feat_jittered[, 3] <- pca_feat_jittered[, 3] +
  rnorm(nrow(pca_feat_jittered), mean = 0, sd = 0.15)

pca_res <- prcomp(pca_feat_jittered, center = FALSE, scale. = FALSE)

df_pca <- data.frame(
  PC1      = pca_res$x[, 1],
  PC2      = pca_res$x[, 2],
  PC3      = pca_res$x[, 3],
  cluster  = df_pca_base$cluster,
  nama     = df_pca_base$nama_wisata,
  provinsi = df_pca_base$provinsi,
  rating   = df_pca_base$rating,
  kategori = df_pca_base$kategori
)

# Warna cluster (Diberi nama agar pemetaan di Plotly konsisten)
cluster_colors <- c("1" = "#6C63FF", "2" = "#FF6584", "3" = "#43C6AC", "4" = "#F7971E")
