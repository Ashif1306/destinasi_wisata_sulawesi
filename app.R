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
library(shinyanimate)
library(shinycssloaders)
library(shinyWidgets)

source("chatbot.R")

# Muat environment variables dari .Renviron (API keys, dll.)
readRenviron(".Renviron")

# ============================================================
# GLOBAL: Supabase Config & Data Loading
# ============================================================

SUPABASE_URL <- "https://bdtaakyfivzekgyldngw.supabase.co"
SUPABASE_KEY <- "sb_publishable_LjvKgQKnGqSOC2gJQzxTow_ABT1Fn8c"
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

  # Cleaning: filter jumlah_riview >= 10
  # Cek nama kolom untuk menghindari error jika nama berbeda
  if ("jumlah_riview" %in% colnames(df)) {
    df <- df %>% filter(jumlah_riview >= 10)
  } else if ("jumlah_review" %in% colnames(df)) {
    df <- df %>%
      rename(jumlah_riview = jumlah_review) %>%
      filter(jumlah_riview >= 10)
  } else if ("reviews" %in% colnames(df)) {
    df <- df %>%
      rename(jumlah_riview = reviews) %>%
      filter(jumlah_riview >= 10)
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
  feat <- df %>% select(rating, jumlah_riview, harga_num, lat, long)
  feat_scaled <- scale(feat)
  set.seed(42)
  km <- kmeans(feat_scaled, centers = k, nstart = 25, iter.max = 100)
  df$cluster <- as.factor(km$cluster)
  list(df = df, km = km, scaled = feat_scaled)
}

# Elbow Method & Silhouette: hitung WSS dan Silhouette untuk k = 1 sampai 10
feat_for_elbow <- scale(df_raw %>% select(rating, jumlah_riview, harga_num, lat, long))
set.seed(42)

wss_values <- numeric(10)
sil_values <- numeric(10)
dist_matrix <- dist(feat_for_elbow)

for (k in 1:10) {
  km_temp <- kmeans(feat_for_elbow, centers = k, nstart = 25, iter.max = 100)
  wss_values[k] <- km_temp$tot.withinss
  if (k > 1) {
    sil <- cluster::silhouette(km_temp$cluster, dist_matrix)
    sil_values[k] <- mean(sil[, 3])
  } else {
    sil_values[k] <- NA
  }
}
elbow_df <- data.frame(k = 1:10, WSS = wss_values, Silhouette = sil_values)

kmeans_result <- run_kmeans(df_raw)
df_clustered <- kmeans_result$df %>%
  mutate(provinsi = as.factor(provinsi))

# Cluster labels berdasarkan karakteristik
cluster_summary <- df_clustered %>%
  group_by(cluster) %>%
  summarise(
    n           = n(),
    avg_rating  = round(mean(rating, na.rm = TRUE), 2),
    avg_reviews = round(mean(jumlah_riview, na.rm = TRUE), 0),
    avg_harga   = round(mean(harga_num, na.rm = TRUE), 0),
    .groups     = "drop"
  ) %>%
  mutate(
    label = {
      lbl <- character(n())
      assigned <- logical(n())

      # 1) Rating Tertinggi → klaster BELUM assigned dengan avg_rating tertinggi
      avail <- which(!assigned)
      pick <- avail[which.max(avg_rating[avail])]
      lbl[pick] <- "⭐ Rating Tertinggi"
      assigned[pick] <- TRUE

      # 2) Paling Populer → klaster BELUM assigned dengan avg_reviews tertinggi
      avail <- which(!assigned)
      pick <- avail[which.max(avg_reviews[avail])]
      lbl[pick] <- "🔥 Paling Populer"
      assigned[pick] <- TRUE

      # 3) Destinasi Premium → klaster BELUM assigned dengan avg_harga tertinggi
      avail <- which(!assigned)
      pick <- avail[which.max(avg_harga[avail])]
      lbl[pick] <- "💎 Destinasi Premium"
      assigned[pick] <- TRUE

      # 4) Sisa klaster → Wisata Terjangkau (dijamin hanya 1 untuk k=4)
      lbl[!assigned] <- "🌿 Wisata Terjangkau"

      lbl
    }
  )

# ============================================================
# RANDOM FOREST – Train & Impute
# ============================================================

train_rf <- function(df) {
  # Target: label_rekomendasi (4 kelas: Terbaik / Baik / Sedang / Buruk)
  # Label ini dihitung dari skor komposit berbasis rating, review,
  # kepadatan spasial (KDE), harga, dan kategori harga.
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
  #   - lat, long          : lokasi geografis
  #   - rating             : tingkat kepuasan pengunjung
  #   - jumlah_riview      : popularitas destinasi
  #   - harga_num          : harga tiket masuk (numerik)
  #   - kategori_harga     : kategori harga (Gratis/Murah/Sedang/Mahal)
  features <- c("lat", "long", "rating", "jumlah_riview", "harga_num", "kategori_harga")
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
    label_rekomendasi ~ lat + long + rating + jumlah_riview + harga_num + kategori_harga,
    data        = train,
    ntree       = 500,
    mtry        = 3,
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
             "Alamat belum tersedia.", as.character(alamat))
    } else {
      "Alamat belum tersedia."
    },
    deskripsi_wisata = if ("deskripsi_wisata" %in% colnames(df_final)) {
      ifelse(is.na(deskripsi_wisata) | trimws(as.character(deskripsi_wisata)) == "",
             "Deskripsi belum tersedia.", as.character(deskripsi_wisata))
    } else {
      "Deskripsi belum tersedia."
    }
  )



# ============================================================
# PCA untuk visualisasi K-Means
# ============================================================

pca_res <- prcomp(scale(df_final %>% select(rating, jumlah_riview, harga_num, lat, long)),
  center = FALSE, scale. = FALSE
)
df_pca <- data.frame(
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  cluster = df_final$cluster,
  nama = df_final$nama_wisata,
  provinsi = df_final$provinsi
)

# Warna cluster
cluster_colors <- c("#6C63FF", "#FF6584", "#43C6AC", "#F7971E")

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  title = "Kelompok 8 - Destinasi Wisata Sulawesi",
  skin = "black",
  dashboardHeader(
    title = tags$span(
      style = "font-size: 16px; font-weight: 700;",
      tags$img(
        src = "https://cdn-icons-png.flaticon.com/512/684/684908.png",
        height = "22px", style = "margin-right:6px;vertical-align:middle;"
      ),
      "Kelompok 8"
    ),
    titleWidth = 260
  ),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar",
      menuItem("📊 Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("🔵 Segmentasi K-Means", tabName = "kmeans", icon = icon("project-diagram")),
      menuItem("🌳 Prediksi RF", tabName = "randomforest", icon = icon("tree")),
      menuItem("📋 Dataset & Filter", tabName = "dataset", icon = icon("table"))
    ),
    tags$div(class = "sidebar-filter", "🔍 Filter Global"),
    selectInput("fil_provinsi", "Provinsi:",
      choices = c("Semua", sort(unique(as.character(df_final$provinsi)))),
      selected = "Semua", multiple = FALSE
    ),
    selectInput("fil_kategori", "Kategori Wisata:",
      choices = c("Semua", sort(unique(df_final$kategori))),
      selected = "Semua", multiple = FALSE
    ),
    tags$div(
      style = "padding:15px;color:#7f8c8d;font-size:11px;text-align:center;margin-top:20px;",
      "Kelompok 8 · 2026", tags$br(),
      tags$em("Machine Learning · Shiny")
    )
  ),
  dashboardBody(
    useShinyjs(),
    withAnim(),
    tags$head(
      tags$style(HTML("
        .main-header { position:fixed !important; width:100% !important; top:0 !important; z-index:1030 !important; }
        .main-sidebar { position:fixed !important; top:50px !important; height:calc(100vh - 50px) !important; overflow-y:auto !important; z-index:1020 !important; }
        .content-wrapper, .right-side { margin-top:50px !important; min-height:calc(100vh - 50px) !important; padding-top:0 !important; }
        .content-wrapper > .content { padding:6px 15px 15px !important; }
        .tab-content, .tab-pane { padding-top:0 !important; margin-top:0 !important; }
        .main-header::after { content: 'Objek Wisata Pulau Sulawesi'; position: fixed; left: 50%; transform: translateX(-50%); color: #FFFFFF; font-size: 18px; font-weight: 700; top: 13px; letter-spacing: 1px; z-index: 1040; pointer-events: none; white-space: nowrap; }
        .skin-black .main-header .logo { background:#0A192F !important; color:#FFFFFF !important; border-right:1px solid #112240; }
        .skin-black .main-header .logo:hover { background:#0A192F !important; }
        .skin-black .main-header .navbar { background:#0A192F !important; }
        .skin-black .main-header .navbar .sidebar-toggle { color: #FFFFFF !important; background-color: transparent !important; }
        .skin-black .main-header .navbar .sidebar-toggle:hover { background-color: rgba(255,255,255,0.1) !important; color: #FFFFFF !important; }
        .skin-black .main-sidebar { background:#112240 !important; }
        /* Hapus jarak besar di atas menu sidebar */
        .main-sidebar, .main-sidebar .sidebar { padding-top: 0 !important; margin-top: 0 !important; }
        .sidebar-menu { margin-top: 0 !important; padding-top: 0 !important; }
        .skin-black .sidebar-menu>li>a { color:#A0ABC0; font-weight:500; transition:all 0.3s cubic-bezier(0.25, 0.8, 0.25, 1); }
        .skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li:hover>a { background:rgba(32,201,151,0.15) !important; color:#20C997 !important; border-left-color:#20C997 !important; }
        .skin-black .sidebar-menu>li:hover>a { padding-left: 25px !important; }
        .skin-black .sidebar-menu>li>a>.fa { color:#20C997; transition:all 0.3s ease; }
        .skin-black .sidebar-menu>li.active>a>.fa { animation: icon-pulse-subtle 2s infinite cubic-bezier(0.4, 0, 0.2, 1); }
        @keyframes icon-pulse-subtle { 0% { transform: scale(1); opacity: 1; } 50% { transform: scale(1.2); opacity: 0.8; } 100% { transform: scale(1); opacity: 1; } }
        .content-wrapper { background:#F4F7F6; }
        .box { border-top:3px solid #20C997; border-radius:8px; box-shadow:0 4px 15px rgba(0,0,0,0.05); }
        .value-box, .small-box { border-radius:8px; }
        h3.box-title { font-weight:700; color:#0A192F; }
        .sidebar-filter { padding:15px 15px 5px; color:#20C997; font-size:12px; font-weight:800; text-transform:uppercase; letter-spacing:1px; }
        .skin-black .sidebar form, .skin-black .sidebar .shiny-input-container { padding:5px 15px; }
        .skin-black .sidebar .control-label { color:#A0ABC0; font-weight:500; }
        .skin-black .sidebar .selectize-input { border-radius:6px !important; border:1px solid #2A3B5C; background:#0A192F; color:#FFFFFF; }
        .skin-black .sidebar .selectize-dropdown { background:#0A192F; color:#FFFFFF; border:1px solid #2A3B5C; }
        .content-wrapper .control-label { color:#0A192F; font-weight:700; }
        .content-wrapper .selectize-input { border-radius:6px !important; border:1px solid #CBD5E1; background:#FFFFFF !important; color:#0A192F !important; }
        .content-wrapper .selectize-input>input { color:#0A192F !important; }
        .content-wrapper .selectize-dropdown { background:#FFFFFF; color:#0A192F; }
        .dataTables_wrapper { font-size:13px; }
        .detail-img { border-radius:10px; width:100%; max-height:300px; object-fit:cover; box-shadow:0 4px 15px rgba(0,0,0,0.15); }
        .detail-desc { text-align:justify; font-size:14px; line-height:1.7; color:#333; padding:5px 0; }
        .detail-meta { font-size:13px; color:#666; margin-bottom:8px; }
        .detail-title { font-size:20px; font-weight:800; color:#0A192F; margin-bottom:10px; }
        .placeholder-img { width:100%; height:260px; border-radius:10px; display:flex; align-items:center; justify-content:center; font-size:60px; color:#fff; }
        /* SIDEBAR COMPACT */
        .skin-black .sidebar-menu>li>a { padding:8px 15px 8px 15px !important; font-size:13px; }
        .skin-black .sidebar-menu { margin-top: 0 !important; padding-top: 0 !important; }
        .sidebar-filter { padding:8px 15px 3px !important; }
        .skin-black .sidebar form, .skin-black .sidebar .shiny-input-container { padding:2px 12px !important; }
        .skin-black .sidebar .shiny-input-container { margin-bottom:2px !important; }
        .small-box { border-radius:10px !important; transition:transform 0.28s cubic-bezier(0.34,1.56,0.64,1),box-shadow 0.28s ease,filter 0.28s ease !important; cursor:default; overflow:hidden; position:relative; }
        .small-box.bg-purple { box-shadow:0 4px 18px rgba(108,99,255,0.25) !important; }
        .small-box.bg-purple:hover { transform:translateY(-7px) scale(1.04) !important; box-shadow:0 18px 40px rgba(108,99,255,0.45) !important; filter:brightness(1.08) !important; }
        .small-box.bg-purple:hover .icon { animation:spin-slow 1.2s linear infinite; }
        .small-box.bg-yellow { box-shadow:0 4px 18px rgba(247,151,30,0.2) !important; }
        .small-box.bg-yellow:hover { transform:translateY(-5px) scale(1.03) !important; box-shadow:0 0 0 4px rgba(255,215,0,0.35),0 0 28px 6px rgba(255,193,7,0.5),0 14px 35px rgba(247,151,30,0.4) !important; animation:glow-pulse 1.5s ease-in-out infinite !important; }
        .small-box.bg-green { box-shadow:0 4px 18px rgba(39,174,96,0.2) !important; }
        .small-box.bg-green::before { content:''; position:absolute; top:0; left:-100%; width:60%; height:100%; background:linear-gradient(120deg,transparent,rgba(255,255,255,0.22),transparent); transition:none; pointer-events:none; z-index:2; }
        .small-box.bg-green:hover { transform:translateY(-5px) scale(1.03) !important; box-shadow:0 16px 36px rgba(39,174,96,0.4) !important; }
        .small-box.bg-green:hover::before { animation:shimmer-sweep 0.75s ease forwards; }
        .small-box.bg-aqua { box-shadow:0 4px 18px rgba(0,188,212,0.2) !important; outline:2px solid transparent; outline-offset:0px; transition:transform 0.28s cubic-bezier(0.34,1.56,0.64,1),box-shadow 0.28s ease,outline-color 0.3s ease,outline-offset 0.3s ease !important; }
        .small-box.bg-aqua:hover { transform:translateY(-5px) scale(1.03) !important; box-shadow:0 16px 36px rgba(0,188,212,0.45) !important; outline-color:rgba(0,229,255,0.7); outline-offset:5px; }
        .small-box.bg-aqua:hover .icon { animation:wiggle 0.6s ease; }
        .small-box.bg-orange { box-shadow:0 4px 18px rgba(230,126,34,0.2) !important; }
        .small-box.bg-orange:hover { transform:translateY(-5px) scale(1.03) !important; box-shadow:0 16px 36px rgba(230,126,34,0.4) !important; filter:brightness(1.07) !important; }
        @keyframes glow-pulse { 0%,100% { box-shadow:0 0 0 4px rgba(255,215,0,0.35),0 0 28px 6px rgba(255,193,7,0.5),0 14px 35px rgba(247,151,30,0.4); } 50% { box-shadow:0 0 0 8px rgba(255,215,0,0.2),0 0 40px 12px rgba(255,193,7,0.65),0 18px 40px rgba(247,151,30,0.55); } }
        @keyframes shimmer-sweep { 0% { left:-100%; opacity:1; } 100% { left:150%; opacity:1; } }
        @keyframes spin-slow { from { transform:rotate(0deg); } to { transform:rotate(360deg); } }
        @keyframes wiggle { 0%,100% { transform:rotate(0deg); } 20% { transform:rotate(-12deg); } 40% { transform:rotate(12deg); } 60% { transform:rotate(-8deg); } 80% { transform:rotate(6deg); } }
        @keyframes card-fade-in { from { opacity:0; transform:translateY(18px); } to { opacity:1; transform:translateY(0); } }
        @keyframes border-run { 0% { background-position:0% 50%; } 100% { background-position:200% 50%; } }
        .dual-chart-box { animation:card-fade-in 0.5s cubic-bezier(0.22,1,0.36,1) both; }
        .dual-chart-box:nth-child(1) { animation-delay:0.05s; }
        .dual-chart-box:nth-child(2) { animation-delay:0.15s; }
        .dual-chart-box .box { transition:transform 0.3s cubic-bezier(0.34,1.56,0.64,1),box-shadow 0.3s ease !important; position:relative; overflow:hidden; }
        .dual-chart-box .box:hover { transform:translateY(-6px) scale(1.015) !important; }
        .dual-chart-box.chart-bar-box .box { box-shadow:0 4px 18px rgba(108,99,255,0.12) !important; }
        .dual-chart-box.chart-bar-box .box:hover { box-shadow:0 18px 42px rgba(108,99,255,0.32) !important; }
        .dual-chart-box.chart-bar-box .box::after { content:''; position:absolute; top:0; left:0; right:0; height:3px; background:linear-gradient(90deg,#6C63FF,#43C6AC,#6C63FF,#a0c4ff,#6C63FF); background-size:200% 100%; opacity:0; transition:opacity 0.3s ease; }
        .dual-chart-box.chart-bar-box .box:hover::after { opacity:1; animation:border-run 1.8s linear infinite; }
        .dual-chart-box.chart-pie-box .box { box-shadow:0 4px 18px rgba(39,174,96,0.12) !important; }
        .dual-chart-box.chart-pie-box .box:hover { box-shadow:0 18px 42px rgba(39,174,96,0.32) !important; }
        .dual-chart-box.chart-pie-box .box::after { content:''; position:absolute; top:0; left:0; right:0; height:3px; background:linear-gradient(90deg,#27ae60,#f39c12,#e74c3c,#9b59b6,#27ae60); background-size:200% 100%; opacity:0; transition:opacity 0.3s ease; }
        .dual-chart-box.chart-pie-box .box:hover::after { opacity:1; animation:border-run 2s linear infinite; }
        #anim_akurasi_box .small-box { background-color: #2ecc71 !important; color: #fff !important; }
        #anim_kappa_box .small-box { background-color: #3498db !important; color: #fff !important; box-shadow: 0 4px 18px rgba(52,152,219,0.2) !important; transition: transform 0.28s cubic-bezier(0.34,1.56,0.64,1), box-shadow 0.28s ease !important; }
        #anim_kappa_box .small-box:hover { transform: translateY(-5px) scale(1.03) !important; box-shadow: 0 16px 36px rgba(52,152,219,0.45) !important; filter: brightness(1.08) !important; }
        #anim_kappa_box .small-box:hover .icon { animation: wiggle 0.6s ease; }
        .hidden-anim-box { visibility: hidden; }
        /* Style & Animasi Interaktif Input Simulator */
        #sim_input_panel .form-control {
          border-radius: 10px !important;
          border: 2px solid #e2e8f0;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
        #sim_input_panel .form-control:focus {
          border-color: #20C997;
          box-shadow: 0 0 0 4px rgba(32, 201, 151, 0.25);
          transform: translateY(-2px);
        }
      ")),
      # ---- Script: JS handler untuk update dropdown Kabupaten ----
      tags$script(HTML("
        Shiny.addCustomMessageHandler('updateKabupaten', function(data) {
          var sel = document.getElementById('ov_kabupaten');
          if (!sel) return;
          sel.innerHTML = '';
          data.choices.forEach(function(c) {
            var opt = document.createElement('option');
            opt.value = c; opt.text = c;
            sel.appendChild(opt);
          });
          sel.value = 'Semua';
          $(sel).trigger('change');
        });
      "))
    ),
    sylva_chatbot_dependencies(),
    tabItems(
      # ========== TAB 1: OVERVIEW ==========
      tabItem(
        tabName = "overview",
        # ---- Top Filter Bar ----
        fluidRow(
          column(3, selectInput("ov_provinsi", "🌏 Provinsi:",
            choices = c("Semua" = "Semua", sort(unique(as.character(df_final$provinsi)))),
            selected = "Semua"
          )),
          column(
            3,
            tags$label("📍 Kabupaten:"),
            tags$select(
              id = "ov_kabupaten",
              class = "form-control",
              style = "width:100%;",
              tags$option(value = "Semua", "Semua")
            )
          ),
          column(3, selectInput("ov_kategori", "🏷️ Kategori Wisata:",
            choices = c("Semua" = "Semua", sort(unique(df_final$kategori))),
            selected = "Semua"
          )),
          column(3, selectizeInput("ov_search", "🔍 Cari Destinasi:",
            choices = NULL,
            options = list(
              placeholder = "Ketik nama wisata...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          ))
        ),
        # ---- Value Boxes ----
        fluidRow(
          valueBoxOutput("vb_total", width = 3),
          valueBoxOutput("vb_rating", width = 3),
          valueBoxOutput("vb_gratis", width = 3),
          valueBoxOutput("vb_reviews", width = 3)
        ),
        # ---- Detail Panel (muncul saat wisata dipilih) ----
        uiOutput("detail_panel"),
        # ---- Peta ----
        fluidRow(
          box(
            title = "🗺️ Peta Destinasi Wisata Sulawesi", width = 12,
            status = "primary", solidHeader = TRUE, height = 520,
            leafletOutput("map_overview", height = 460)
          )
        ),
        # ---- Dual Charts (Sembunyi saat pencarian aktif) ----
        uiOutput("dual_charts_panel")
      ),

      # ========== TAB 2: K-MEANS ==========
      tabItem(
        tabName = "kmeans",
        fluidRow(
          box(
            title = "📐 Metode Elbow (WSS)", width = 6,
            status = "warning", solidHeader = TRUE, height = 420,
            plotlyOutput("plot_elbow", height = 360)
          ),
          box(
            title = "📈 Metode Silhouette", width = 6,
            status = "warning", solidHeader = TRUE, height = 420,
            plotlyOutput("plot_silhouette", height = 360)
          )
        ),
        fluidRow(
          box(
            title = "📋 Ringkasan Karakteristik Klaster", width = 12,
            status = "info", solidHeader = TRUE,
            DTOutput("tbl_cluster_summary"),
            tags$hr(),
            tags$p(
              style = "font-size:12px;color:#555;",
              "Jika tidak ditemukan 'siku' (elbow) atau puncak silhouette yang jelas, kita menggunakan k=4 berdasarkan pertimbangan praktis untuk membagi segmentasi menjadi 4 kategori destinasi."
            )
          )
        ),
        fluidRow(
          box(
            title = "🔵 Visualisasi Klaster PCA (2D)", width = 8,
            status = "primary", solidHeader = TRUE, height = 530,
            plotlyOutput("plot_pca", height = 470)
          ),
          box(
            title = "📊 Distribusi Klaster per Provinsi", width = 4,
            status = "success", solidHeader = TRUE, height = 530,
            plotlyOutput("plot_cluster_provinsi", height = 470)
          )
        )
      ),

      # ========== TAB 3: RANDOM FOREST ==========
      tabItem(
        tabName = "randomforest",
        fluidRow(
          column(width = 4, tags$div(id = "anim_akurasi_box", class = "hidden-anim-box", valueBoxOutput("ib_accuracy", width = NULL))),
          column(width = 4, tags$div(id = "anim_kappa_box", class = "hidden-anim-box", valueBoxOutput("ib_kappa", width = NULL))),
          column(width = 4, tags$div(id = "anim_trees_box", class = "hidden-anim-box", valueBoxOutput("ib_ntree", width = NULL)))
        ),
        fluidRow(
          box(
            title = "📉 Confusion Matrix", width = 6,
            status = "primary", solidHeader = TRUE, height = 420,
            withSpinner(plotlyOutput("plot_cm", height = 360), type = 1, color = "#64748b", size = 0.7)
          ),
          box(
            title = "🌟 Feature Importance", width = 6,
            status = "info", solidHeader = TRUE, height = 420,
            plotlyOutput("plot_importance", height = 360)
          )
        ),
        fluidRow(
          box(
            title = "📈 OOB Error Rate vs Jumlah Trees", width = 6,
            status = "warning", solidHeader = TRUE, height = 400,
            plotlyOutput("plot_oob", height = 340)
          ),
          box(
            title = "🎯 Precision & Recall per Kelas", width = 6,
            status = "danger", solidHeader = TRUE, height = 400,
            plotlyOutput("plot_perclass", height = 340)
          )
        ),
        fluidRow(
          box(
            title = "🔮 Simulator Prediksi Rekomendasi Wisata", width = 12,
            status = "success", solidHeader = TRUE,
            tags$p(
              style = "font-size:12px;color:#888;margin-bottom:8px;",
              "ℹ️ Prediksi 4 kelas (Terbaik / Baik / Sedang / Buruk) berdasarkan lokasi, rating, jumlah review, harga, dan kategori harga."
            ),
            tags$div(
              id = "sim_input_panel",
              fluidRow(
                column(2, numericInput("sim_lat", "Latitude", value = -5.14, step = 0.01)),
                column(2, numericInput("sim_long", "Longitude", value = 119.41, step = 0.01)),
                column(2, numericInput("sim_rating", "Rating", value = 4.3, min = 1, max = 5, step = 0.1)),
                column(2, numericInput("sim_review", "Jumlah Review", value = 1000, min = 10)),
                column(2, numericInput("sim_harga", "Harga (Rp)", value = 10000, min = 0)),
                column(2, shinyjs::disabled(textInput("sim_kat_harga", "Kategori Harga", value = "Sedang")))
              )
            ),
            fluidRow(
              column(2, actionButton("btn_predict", "🔮 Prediksi",
                class = "btn-success", style = "margin-top:5px;width:100%;font-weight:700;"
              )),
              column(
                10,
                tags$div(
                  id = "sim_result",
                  style = "font-size:20px;font-weight:700;margin-top:5px;padding:10px 20px;
                             border-radius:10px;background:#f8f9fa;",
                  withSpinner(uiOutput("pred_result"), type = 1, color = "#64748b", size = 0.5)
                )
              )
            )
          )
        )
      ),

      # ========== TAB 4: DATASET ==========
      tabItem(
        tabName = "dataset",
        fluidRow(
          box(
            title = "📋 Dataset Lengkap (Sudah Diimputasi & Dicluster)", width = 12,
            status = "primary", solidHeader = TRUE,
            DTOutput("tbl_full")
          )
        )
      )
    ),

    sylva_chatbot_ui()
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  # ============================================================
  # SIDEBAR FILTER (untuk tab K-Means, RF, Dataset)
  # ============================================================
  df_filtered <- reactive({
    d <- df_final
    if (length(input$fil_provinsi) > 0 && input$fil_provinsi != "Semua") d <- d %>% filter(provinsi == input$fil_provinsi)
    if (length(input$fil_kategori) > 0 && input$fil_kategori != "Semua") d <- d %>% filter(kategori == input$fil_kategori)
    d
  })

  # ============================================================
  # OVERVIEW: Dynamic Kabupaten & Reactive Data
  # ============================================================

  # Server-side selectize: populate search choices
  updateSelectizeInput(session, "ov_search",
    choices = c(" " = "", setNames(df_final$nama_wisata, df_final$nama_wisata)),
    server = TRUE
  )

  # Cascading dropdown: kirim pilihan Kabupaten ke browser via JavaScript
  # Menggunakan sendCustomMessage untuk bypass semua sistem reaktif Shiny
  # yang terbukti tidak andal untuk kasus ini
  observeEvent(input$ov_provinsi,
    {
      provinsi_val <- input$ov_provinsi

      if (is.null(provinsi_val) || provinsi_val == "Semua") {
        kab_choices <- sort(unique(as.character(df_final$kabupaten)))
      } else {
        kab_choices <- sort(unique(as.character(
          df_final$kabupaten[df_final$provinsi == provinsi_val]
        )))
      }

      kab_choices <- kab_choices[!is.na(kab_choices) & nzchar(kab_choices)]

      # Kirim langsung ke JavaScript di browser — 100% andal
      session$sendCustomMessage("updateKabupaten", list(
        choices = c("Semua", kab_choices)
      ))
    },
    ignoreInit = TRUE
  )

  # Reactive: apakah ada wisata yg dicari?
  selected_wisata <- reactive({
    search_val <- input$ov_search
    if (!is.null(search_val) && nzchar(search_val)) {
      df_final %>%
        filter(nama_wisata == search_val) %>%
        slice(1)
    } else {
      NULL
    }
  })

  # Satu reactive dataframe untuk semua output Overview
  df_overview <- reactive({
    d <- df_final
    if (length(input$ov_provinsi) > 0 && input$ov_provinsi != "Semua") d <- d %>% filter(provinsi == input$ov_provinsi)
    if (length(input$ov_kabupaten) > 0 && input$ov_kabupaten != "Semua") d <- d %>% filter(kabupaten == input$ov_kabupaten)
    if (length(input$ov_kategori) > 0 && input$ov_kategori != "Semua") d <- d %>% filter(kategori == input$ov_kategori)
    d
  })

  # ---- VALUE BOXES (berubah saat wisata dipilih) ----
  output$vb_total <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(
        value    = sel$rating,
        subtitle = "Rating Destinasi",
        icon     = icon("star"),
        color    = "yellow"
      )
    } else {
      valueBox(
        value    = nrow(df_overview()),
        subtitle = "Total Destinasi Wisata",
        icon     = icon("map-marker-alt"),
        color    = "purple"
      )
    }
  })

  output$vb_rating <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(
        value    = format(sel$jumlah_riview, big.mark = "."),
        subtitle = "Jumlah Review",
        icon     = icon("comments"),
        color    = "aqua"
      )
    } else {
      valueBox(
        value    = round(mean(df_overview()$rating, na.rm = TRUE), 2),
        subtitle = "Rata-rata Rating",
        icon     = icon("star"),
        color    = "yellow"
      )
    }
  })

  output$vb_gratis <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(
        value    = as.character(sel$kategori_harga),
        subtitle = paste0("Harga: Rp ", format(sel$harga_num, big.mark = ".")),
        icon     = icon("tag"),
        color    = "orange"
      )
    } else {
      n_total <- nrow(df_overview())
      n_gratis <- sum(df_overview()$kategori_harga == "Gratis", na.rm = TRUE)
      pct <- ifelse(n_total > 0, round(n_gratis / n_total * 100, 1), 0)
      valueBox(
        value    = paste0(pct, "%"),
        subtitle = paste0("Wisata Gratis (", n_gratis, " dari ", n_total, ")"),
        icon     = icon("gift"),
        color    = "green"
      )
    }
  })

  output$vb_reviews <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(
        value    = paste0("Klaster ", sel$cluster),
        subtitle = "Segmen K-Means",
        icon     = icon("project-diagram"),
        color    = "purple"
      )
    } else {
      total_reviews <- sum(df_overview()$jumlah_riview, na.rm = TRUE)
      valueBox(
        value    = format(total_reviews, big.mark = ".", decimal.mark = ","),
        subtitle = "Total Interaksi Review",
        icon     = icon("comments"),
        color    = "aqua"
      )
    }
  })

  # ---- DETAIL PANEL (gambar + deskripsi) ----
  output$detail_panel <- renderUI({
    sel <- selected_wisata()
    if (is.null(sel) || nrow(sel) == 0) {
      return(NULL)
    }

    img_url <- as.character(sel$url_image)
    has_image <- !is.na(img_url) && nzchar(img_url) && img_url != "-"

    placeholder_colors <- c(
      "Wisata Alam" = "linear-gradient(135deg, #43C6AC, #191654)",
      "Wisata Budaya & Sejarah" = "linear-gradient(135deg, #F7971E, #FFD200)",
      "Wisata Religi" = "linear-gradient(135deg, #667eea, #764ba2)",
      "Wisata Kota / Landmark" = "linear-gradient(135deg, #0F2027, #2C5364)",
      "Wisata Hiburan" = "linear-gradient(135deg, #ee0979, #ff6a00)"
    )
    placeholder_icons <- c(
      "Wisata Alam" = "🌿",
      "Wisata Budaya & Sejarah" = "🏛️",
      "Wisata Religi" = "🕌",
      "Wisata Kota / Landmark" = "🏙️",
      "Wisata Hiburan" = "🎡"
    )
    kat <- as.character(sel$kategori)
    bg <- if (kat %in% names(placeholder_colors)) placeholder_colors[[kat]] else "linear-gradient(135deg, #6C63FF, #3F3D56)"
    ico <- if (kat %in% names(placeholder_icons)) placeholder_icons[[kat]] else "📍"

    if (has_image) {
      img_tag <- tags$img(
        src = img_url,
        class = "detail-img",
        referrerpolicy = "no-referrer",
        onerror = paste0(
          "this.onerror=null; this.style.display='none'; ",
          "this.parentNode.querySelector('.fallback-placeholder').style.display='flex';"
        )
      )
      fallback_tag <- div(
        class = "placeholder-img fallback-placeholder",
        style = paste0("background:", bg, "; display:none;"),
        ico
      )
      image_block <- div(img_tag, fallback_tag)
    } else {
      image_block <- div(
        class = "placeholder-img",
        style = paste0("background:", bg, ";"),
        ico
      )
    }

    desc_text <- as.character(sel$deskripsi_wisata)
    if (is.na(desc_text) || desc_text == "" || desc_text == "-") {
      desc_text <- "Deskripsi untuk destinasi ini belum tersedia."
    }

    fluidRow(
      box(
        title = paste0("📸 Detail: ", sel$nama_wisata), width = 12,
        status = "primary", solidHeader = TRUE,
        fluidRow(
          column(5, image_block),
          column(
            7,
            div(class = "detail-title", sel$nama_wisata),
            div(
              class = "detail-meta",
              paste0(
                "📍 ", sel$kabupaten, ", ", sel$provinsi,
                "  |  🏷️ ", sel$kategori,
                "  |  ⭐ ", sel$rating,
                "  |  💬 ", format(sel$jumlah_riview, big.mark = "."), " review"
              )
            ),
            div(class = "detail-desc", desc_text),
            actionButton("btn_reset_search", "Kembali ke Overview",
              icon = icon("arrow-left"),
              class = "btn-warning btn-sm",
              style = "float: right; margin-top: 15px; font-weight: bold;"
            )
          )
        )
      )
    )
  })

  # Event untuk tombol Reset
  observeEvent(input$btn_reset_search, {
    updateSelectizeInput(session, "ov_search", selected = " ")
  })

  # Reset search secara otomatis jika user mengganti filter (Provinsi, Kabupaten, atau Kategori)
  observeEvent(c(input$ov_provinsi, input$ov_kabupaten, input$ov_kategori),
    {
      # Kosongkan pencarian jika filter berubah agar peta/valuebox mengikuti filter
      if (!is.null(input$ov_search) && nzchar(trimws(input$ov_search))) {
        updateSelectizeInput(session, "ov_search", selected = " ")
      }
    },
    ignoreInit = TRUE
  )

  # ---- PETA LEAFLET (base map rendered once) ----
  # Warna berdasarkan Cluster K-Means
  pal_cluster <- colorFactor(
    palette = c("#6C63FF", "#FF6584", "#43C6AC", "#F7971E"),
    domain  = sort(unique(df_final$cluster))
  )

  output$map_overview <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 121.5, lat = -1.5, zoom = 6) %>%
      addLegend("bottomright",
        pal = pal_cluster,
        values = sort(unique(df_final$cluster)),
        title = "Cluster Wisata", opacity = 0.9
      )
  })

  # Update markers + flyTo ketika filter berubah
  observe({
    sel <- selected_wisata()

    # 2. Logika Reaktif "Single Focus": Jika ada pencarian, d_map HANYA 1 titik
    if (!is.null(sel) && nrow(sel) > 0) {
      d_map <- sel
    } else {
      d_map <- df_overview() %>% filter(!is.na(lat) & !is.na(long))
    }

    proxy <- leafletProxy("map_overview", data = d_map) %>%
      clearMarkerClusters() %>%
      clearMarkers()

    if (nrow(d_map) > 0) {
      proxy %>% addCircleMarkers(
        lng = ~long, lat = ~lat,
        radius = 7, weight = 1, fillOpacity = 0.8,
        color = "#ffffff", # border color
        fillColor = ~ pal_cluster(as.character(cluster)),
        popup = ~ paste0(
          "<b>", nama_wisata, "</b><br>",
          "📍 ", kabupaten, ", ", provinsi, "<br>",
          "⭐ Rating: ", rating, " | 💬 ", jumlah_riview, " review<br>",
          "💰 ", kategori_harga, " (Rp ", format(harga_num, big.mark = "."), ")"
        ),
        # Hanya gunakan cluster jika lebih dari 1 data
        clusterOptions = if (nrow(d_map) > 1) markerClusterOptions() else NULL
      )
    }

    # Logika Zoom (Prioritas Auto-Zoom)
    if (!is.null(sel) && nrow(sel) > 0 && !is.na(sel$lat) && !is.na(sel$long)) {
      proxy %>% flyTo(lng = sel$long, lat = sel$lat, zoom = 15)
    } else if (length(input$ov_kabupaten) > 0 && input$ov_kabupaten != "Semua" && nrow(d_map) > 0) {
      proxy %>% flyToBounds(lng1 = min(d_map$long), lat1 = min(d_map$lat), lng2 = max(d_map$long), lat2 = max(d_map$lat))
    } else if (length(input$ov_provinsi) > 0 && input$ov_provinsi != "Semua" && nrow(d_map) > 0) {
      proxy %>% flyToBounds(lng1 = min(d_map$long), lat1 = min(d_map$lat), lng2 = max(d_map$long), lat2 = max(d_map$lat))
    } else {
      proxy %>% flyTo(lng = 121.44, lat = -1.43, zoom = 6)
    }
  })

  # ---- DUAL CHARTS PANEL (Conditionally displayed) ----
  output$dual_charts_panel <- renderUI({
    sel <- selected_wisata()
    # Jika ADA wisata yang dipilih (pencarian aktif), sembunyikan chart
    if (!is.null(sel) && nrow(sel) > 0) {
      return(NULL)
    }

    # Jika tidak ada pencarian, tampilkan chart
    fluidRow(
      div(
        class = "dual-chart-box chart-bar-box col-sm-7",
        box(
          title = "📊 Jumlah Wisata per Kategori", width = 12,
          status = "info", solidHeader = TRUE, height = 440,
          plotlyOutput("bar_kategori", height = 380)
        )
      ),
      div(
        class = "dual-chart-box chart-pie-box col-sm-5",
        box(
          title = "🍩 Proporsi Label Rekomendasi (Prediksi RF)", width = 12,
          status = "success", solidHeader = TRUE, height = 440,
          plotlyOutput("pie_rekomendasi", height = 380)
        )
      )
    )
  })

  # ---- BAR CHART: Kategori Wisata ----
  output$bar_kategori <- renderPlotly({
    d <- df_overview() %>%
      count(kategori, name = "n") %>%
      arrange(desc(n)) %>%
      head(10) %>%
      # Potong label panjang agar tidak overflow ke tengah
      mutate(label_short = ifelse(
        nchar(kategori) > 22,
        paste0(substr(kategori, 1, 20), "…"),
        kategori
      ))

    # Warna gradasi per ranking
    n_rows <- nrow(d)
    bar_pal <- colorRampPalette(c("#818cf8", "#4338ca"))(n_rows)

    plot_ly(d,
      x = ~n, y = ~ reorder(label_short, n), type = "bar",
      orientation = "h",
      marker = list(
        color   = bar_pal,
        line    = list(color = "rgba(255,255,255,0.3)", width = 1),
        opacity = 1
      ),
      hovertemplate = paste0(
        "<b>%{customdata}</b><br>",
        "Jumlah: <b>%{x}</b> destinasi",
        "<extra></extra>"
      ),
      customdata = ~kategori # nama asli untuk tooltip
    ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "rgba(248,249,250,0.55)",
        xaxis = list(
          title     = "Jumlah Destinasi",
          color     = "#333",
          gridcolor = "rgba(200,200,200,0.4)",
          zeroline  = FALSE,
          # Beri ruang 15% di kanan agar bar terpanjang tidak terpotong
          range     = c(0, max(d$n) * 1.15)
        ),
        yaxis = list(
          title = "",
          color = "#444",
          tickfont = list(size = 12, color = "#333"),
          automargin = TRUE
        ),
        margin = list(l = 10, r = 25, t = 10, b = 45),
        showlegend = FALSE,
        hoverlabel = list(
          bgcolor     = "#0A192F",
          font        = list(color = "#fff", size = 13),
          bordercolor = "#6C63FF"
        )
      ) %>%
      {
        plotly::config(.,
          displayModeBar = FALSE,
          responsive     = TRUE
        )
      }
  })

  # ---- PIE CHART: Proporsi Label Rekomendasi (hasil prediksi RF) ----
  output$pie_rekomendasi <- renderPlotly({
    d <- df_overview() %>%
      count(label_rekomendasi, name = "n") %>%
      arrange(label_rekomendasi)
    pie_colors <- c(
      "Terbaik" = "#27ae60",
      "Baik"    = "#3498db",
      "Sedang"  = "#f39c12",
      "Buruk"   = "#e74c3c"
    )
    total <- sum(d$n)
    n_slices <- nrow(d)

    plot_ly(d,
      labels = ~label_rekomendasi,
      values = ~n,
      type = "pie",
      hole = 0.48,
      marker = list(
        colors = pie_colors[as.character(d$label_rekomendasi)],
        line   = list(color = "#ffffff", width = 2.5)
      ),
      # pull = 0 (default) — diatur dinamis via JS
      pull = rep(0, n_slices),
      textinfo = "label+percent",
      textfont = list(size = 13, color = "#fff"),
      insidetextorientation = "radial",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Jumlah: <b>%{value}</b> destinasi<br>",
        "Proporsi: <b>%{percent}</b>",
        "<extra></extra>"
      ),
      rotation = -90,
      direction = "clockwise"
    ) %>%
      layout(
        paper_bgcolor = "transparent",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.05, y = -0.08,
          font = list(size = 12)
        ),
        annotations = list(list(
          text = paste0("<b>", total, "</b><br><span style='font-size:10px'>destinasi</span>"),
          x = 0.5, y = 0.5,
          font = list(size = 16, color = "#0A192F"),
          showarrow = FALSE
        )),
        hoverlabel = list(
          bgcolor     = "#0A192F",
          font        = list(color = "#fff", size = 13),
          bordercolor = "#20C997"
        )
      ) %>%
      {
        plotly::config(.,
          displayModeBar = FALSE,
          responsive     = TRUE
        )
      } %>%
      # Efek pull interaktif: slice yang di-hover akan timbul keluar
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;
          var nSlices = gd.data[0].labels.length;
          var basePull = new Array(nSlices).fill(0);

          gd.on('plotly_hover', function(evt) {
            var idx = evt.points[0].pointNumber;
            var pull = basePull.slice();
            pull[idx] = 0.1;  // tarik slice ke luar 10%
            Plotly.restyle(gd, { pull: [pull] }, [0]);
          });

          gd.on('plotly_unhover', function() {
            Plotly.restyle(gd, { pull: [basePull] }, [0]);
          });
        }
      ")
  })

  # ---- PCA PLOT ----
  output$plot_pca <- renderPlotly({
    d <- df_pca
    if (input$fil_provinsi != "Semua") {
      idx <- df_final$provinsi == input$fil_provinsi
      d <- d[idx, ]
    }
    plot_ly(d,
      x = ~PC1, y = ~PC2, color = ~cluster,
      colors = cluster_colors,
      type = "scatter", mode = "markers",
      text = ~ paste0(nama, "<br>", provinsi),
      hoverinfo = "text",
      marker = list(size = 7, opacity = 0.75)
    ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "#f8f9fa",
        xaxis = list(title = "PC1", zeroline = FALSE),
        yaxis = list(title = "PC2", zeroline = FALSE),
        legend = list(title = list(text = "Klaster"))
      )
  })

  # ---- CLUSTER SUMMARY TABLE ----
  output$tbl_cluster_summary <- renderDT({
    cs <- cluster_summary %>%
      left_join(
        df_final %>% group_by(cluster) %>%
          summarise(top_provinsi = names(which.max(table(provinsi))), .groups = "drop"),
        by = "cluster"
      ) %>%
      select(
        Klaster = cluster, Label = label, N = n,
        `Avg Rating` = avg_rating, `Avg Review` = avg_reviews,
        `Avg Harga` = avg_harga, `Dominan` = top_provinsi
      )

    datatable(cs,
      options = list(dom = "t", pageLength = 10),
      rownames = FALSE,
      class = "compact stripe"
    ) %>%
      formatCurrency("Avg Harga", currency = "Rp ", digits = 0, mark = ",")
  })

  # ---- CLUSTER PER PROVINSI ----
  output$plot_cluster_provinsi <- renderPlotly({
    d <- df_final %>%
      group_by(provinsi, cluster) %>%
      summarise(n = n(), .groups = "drop")
    plot_ly(d,
      x = ~provinsi, y = ~n, color = ~cluster,
      colors = cluster_colors,
      type = "bar"
    ) %>%
      layout(
        barmode = "stack",
        paper_bgcolor = "transparent",
        plot_bgcolor = "#f8f9fa",
        xaxis = list(title = "Provinsi", tickangle = -30),
        yaxis = list(title = "Jumlah Destinasi"),
        legend = list(title = list(text = "Klaster"))
      )
  })

  # ---- ELBOW PLOT ----
  output$plot_elbow <- renderPlotly({
    plot_ly(elbow_df,
      x = ~k, y = ~WSS, type = "scatter", mode = "lines+markers",
      line = list(color = "#6C63FF", width = 3),
      marker = list(size = 10, color = "#6C63FF"),
      name = "WSS"
    ) %>%
      add_trace(
        x = 4, y = elbow_df$WSS[4], type = "scatter", mode = "markers",
        marker = list(
          size = 16, color = "#e94560", symbol = "diamond",
          line = list(color = "#fff", width = 2)
        ),
        name = "k = 4 (Praktis)",
        hoverinfo = "text",
        text = paste0("Dipilih k = 4\nWSS = ", round(elbow_df$WSS[4], 1))
      ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "#f8f9fa",
        xaxis = list(title = "Jumlah Klaster (k)", dtick = 1),
        yaxis = list(title = "Within-Cluster Sum of Squares (WSS)"),
        showlegend = FALSE,
        annotations = list(
          list(
            x = 4, y = elbow_df$WSS[4], text = "⬅ k=4",
            showarrow = TRUE, arrowhead = 2, ax = 40, ay = -40,
            font = list(size = 13, color = "#e94560", family = "Arial Black")
          )
        )
      )
  })

  # ---- SILHOUETTE PLOT ----
  output$plot_silhouette <- renderPlotly({
    plot_ly(elbow_df %>% filter(k > 1),
      x = ~k, y = ~Silhouette, type = "scatter", mode = "lines+markers",
      line = list(color = "#43C6AC", width = 3),
      marker = list(size = 10, color = "#43C6AC"),
      name = "Silhouette"
    ) %>%
      add_trace(
        x = 4, y = elbow_df$Silhouette[4], type = "scatter", mode = "markers",
        marker = list(
          size = 16, color = "#e94560", symbol = "diamond",
          line = list(color = "#fff", width = 2)
        ),
        name = "k = 4 (Praktis)",
        hoverinfo = "text",
        text = paste0("Dipilih k = 4\nSilhouette = ", round(elbow_df$Silhouette[4], 3))
      ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "#f8f9fa",
        xaxis = list(title = "Jumlah Klaster (k)", dtick = 1),
        yaxis = list(title = "Rata-rata Nilai Silhouette"),
        showlegend = FALSE,
        annotations = list(
          list(
            x = 4, y = elbow_df$Silhouette[4], text = "⬅ k=4",
            showarrow = TRUE, arrowhead = 2, ax = 40, ay = -40,
            font = list(size = 13, color = "#e94560", family = "Arial Black")
          )
        )
      )
  })

  # ---- INFO BOXES RF ----
  output$ib_accuracy <- renderValueBox({
    valueBox(paste0(round(rf_cm$overall["Accuracy"] * 100, 1), "%"), "Akurasi Model",
      icon = icon("bullseye"), color = "green"
    )
  })
  output$ib_kappa <- renderValueBox({
    valueBox(round(rf_cm$overall["Kappa"], 3), "Kappa",
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$ib_ntree <- renderValueBox({
    valueBox(rf_model$ntree, "Jumlah Trees",
      icon = icon("tree"), color = "purple"
    )
  })

  # ---- ANIMASI VALUE BOXES RF (Staggered: 0ms / 500ms / 1000ms) ----
  observeEvent(input$sidebar,
    {
      if (input$sidebar == "randomforest") {
        # Reset semua box ke state tersembunyi dulu, agar animasi terulang setiap masuk tab
        shinyjs::addClass(id = "anim_akurasi_box", class = "hidden-anim-box")
        shinyjs::addClass(id = "anim_kappa_box", class = "hidden-anim-box")
        shinyjs::addClass(id = "anim_trees_box", class = "hidden-anim-box")

        # Box 1 – Akurasi: muncul langsung (0ms)
        shinyjs::delay(50, {
          shinyjs::removeClass(id = "anim_akurasi_box", class = "hidden-anim-box")
          startAnim(session, "anim_akurasi_box", "fadeInDown")
        })

        # Box 2 – Kappa: muncul setelah 500ms
        shinyjs::delay(500, {
          shinyjs::removeClass(id = "anim_kappa_box", class = "hidden-anim-box")
          startAnim(session, "anim_kappa_box", "fadeInDown")
        })

        # Box 3 – Trees: muncul setelah 1000ms
        shinyjs::delay(1000, {
          shinyjs::removeClass(id = "anim_trees_box", class = "hidden-anim-box")
          startAnim(session, "anim_trees_box", "fadeInDown")
        })
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(c(input$btn_predict, input$fil_provinsi, input$fil_kategori),
    {
      req(input$sidebar == "randomforest")
      startAnim(session, "anim_akurasi_box", "pulse")
    },
    ignoreInit = TRUE
  )

  # ---- CONFUSION MATRIX HEATMAP (animasi: diagonal dulu → menyamping) ----
  output$plot_cm <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi
    input$fil_kategori

    cm_table <- as.data.frame(rf_cm$table)

    # Susun level label agar urutan sumbu konsisten
    labels <- levels(rf_result$train$label_rekomendasi)

    plot_ly(
      x = labels, y = labels,
      # Mulai dengan matrix nol (semua sel tersembunyi)
      z = matrix(0, nrow = length(labels), ncol = length(labels)),
      type = "heatmap",
      colorscale = list(
        c(0, "#f0f4ff"),
        c(0.5, "#818cf8"),
        c(1, "#6C63FF")
      ),
      zmin = 0,
      zmax = max(cm_table$Freq),
      text = matrix("", nrow = length(labels), ncol = length(labels)),
      texttemplate = "%{text}",
      hoverinfo = "x+y+z",
      showscale = FALSE
    ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(title = "Aktual", tickfont = list(size = 12)),
        yaxis = list(title = "Prediksi", tickfont = list(size = 12))
      ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      # Inject animasi via JavaScript: diagonal muncul dulu, lalu sweep ke off-diagonal
      htmlwidgets::onRender(sprintf(
        "
        function(el, x) {
          var gd = el;

          // Data confusion matrix yang sesungguhnya (dari R)
          var labels  = %s;
          var n       = labels.length;
          var rawFreq = %s;  // flat array row-major

          // Susun menjadi matrix n x n
          var fullZ = [];
          var fullT = [];
          for (var r = 0; r < n; r++) {
            var rowZ = [], rowT = [];
            for (var c = 0; c < n; c++) {
              rowZ.push(rawFreq[r * n + c]);
              rowT.push(String(rawFreq[r * n + c]));
            }
            fullZ.push(rowZ);
            fullT.push(rowT);
          }

          // Fungsi utilitas: buat matrix nol
          function zeroMatrix(n) {
            return Array.from({length: n}, function() {
              return new Array(n).fill(0);
            });
          }
          function emptyMatrix(n) {
            return Array.from({length: n}, function() {
              return new Array(n).fill('');
            });
          }

          // ── FASE 1: Muncul diagonal (prediksi benar) satu per satu ──
          var delay = 200;
          for (var d = 0; d < n; d++) {
            (function(diag, t) {
              setTimeout(function() {
                var Z = zeroMatrix(n), T = emptyMatrix(n);
                // Isi sel diagonal yang sudah 'muncul' sampai index diag
                for (var k = 0; k <= diag; k++) {
                  Z[k][k] = fullZ[k][k];
                  T[k][k] = fullT[k][k];
                }
                Plotly.restyle(gd, { z: [Z], text: [T] });
              }, t);
            })(d, delay * (d + 1));
          }

          // ── FASE 2: Sweep menyamping — isi sel off-diagonal baris per baris ──
          var phase2Start = delay * (n + 1) + 100;
          for (var step = 0; step < n; step++) {
            (function(row, t) {
              setTimeout(function() {
                var Z = zeroMatrix(n), T = emptyMatrix(n);
                // Pertahankan semua diagonal
                for (var k = 0; k < n; k++) {
                  Z[k][k] = fullZ[k][k];
                  T[k][k] = fullT[k][k];
                }
                // Isi off-diagonal di baris 0..row
                for (var r = 0; r <= row; r++) {
                  for (var c = 0; c < n; c++) {
                    Z[r][c] = fullZ[r][c];
                    T[r][c] = fullT[r][c];
                  }
                }
                Plotly.restyle(gd, { z: [Z], text: [T] });
              }, t);
            })(step, phase2Start + 280 * (step + 1));
          }
        }
      ",
        # Inject data dari R → JSON untuk JS
        jsonlite::toJSON(labels),
        jsonlite::toJSON(
          as.vector(t(
            matrix(cm_table$Freq,
              nrow = length(labels),
              ncol = length(labels),
              dimnames = list(labels, labels)
            )[labels, labels]
          ))
        )
      ))
  })


  # ---- FEATURE IMPORTANCE ----
  output$plot_importance <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi
    input$fil_kategori

    imp <- as.data.frame(importance(rf_model)) %>%
      rownames_to_column("Feature") %>%
      arrange(desc(MeanDecreaseGini))

    plot_ly(imp,
      x = ~MeanDecreaseGini, y = ~ reorder(Feature, MeanDecreaseGini),
      type = "bar", orientation = "h",
      marker = list(
        color = colorRampPalette(c("#43C6AC", "#1a9e89"))(nrow(imp)),
        line  = list(color = "rgba(255,255,255,0.2)", width = 0.5)
      )
    ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title     = "Mean Decrease Gini",
          rangemode = "nonnegative", # sumbu tidak melebar ke < 0
          range     = c(0, max(imp$MeanDecreaseGini) * 1.12)
        ),
        yaxis = list(title = ""),
        # Transisi pertumbuhan batang saat plot pertama dimuat
        transition = list(
          duration = 1000,
          easing   = "exp-out"
        ),
        frame = list(duration = 1000, redraw = FALSE)
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          // Ambil nilai asli sumbu X lalu set ke 0 agar bar tumbuh dari kiri
          var gd = el;
          var original_x = gd.data[0].x.slice();
          var zero_x = original_x.map(function() { return 0; });

          // Set semua bar ke 0 terlebih dahulu
          Plotly.restyle(gd, {x: [zero_x]}, [0]);

          // Kemudian animasikan bar ke nilai aslinya
          setTimeout(function() {
            Plotly.animate(gd, {
              data: [{ x: original_x }]
            }, {
              transition: { duration: 1000, easing: 'exp-out' },
              frame:      { duration: 1000, redraw: false }
            });
          }, 200);
        }
      ") %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ---- OOB ERROR RATE PLOT ----
  output$plot_oob <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi
    input$fil_kategori

    plot_ly(oob_df,
      x = ~trees, y = ~oob_error,
      type = "scatter", mode = "lines",
      line = list(color = "#F7971E", width = 2.5),
      fill = "tozeroy",
      fillcolor = "rgba(247,151,30,0.15)",
      hovertemplate = "Trees: %{x}<br>OOB Error: %{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "#f8f9fa",
        xaxis = list(
          title = "Jumlah Trees",
          color = "#333"
        ),
        yaxis = list(
          title = "OOB Error Rate (%)",
          color = "#333",
          rangemode = "tozero"
        ),
        annotations = list(list(
          x = rf_model$ntree * 0.75,
          y = tail(oob_df$oob_error, 1) * 1.5,
          text = paste0("OOB Akhir: ", round(tail(oob_df$oob_error, 1), 1), "%"),
          showarrow = FALSE,
          font = list(size = 13, color = "#F7971E", family = "Arial Black")
        )),
        margin = list(t = 10, b = 50, l = 60, r = 20)
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;
          var original_y = gd.data[0].y.slice();
          // Set awal Y ke 0 agar garis tumbuh dari bawah
          var zero_y = original_y.map(function() { return 0; });

          Plotly.restyle(gd, {y: [zero_y]}, [0]);

          setTimeout(function() {
            Plotly.animate(gd, {
              data: [{ y: original_y }]
            }, {
              transition: { duration: 1000, easing: 'cubic-in-out' },
              frame:      { duration: 1000, redraw: false }
            });
          }, 200);
        }
      ") %>%
      {
        plotly::config(., displayModeBar = FALSE)
      }
  })

  # ---- PER-CLASS PRECISION & RECALL PLOT ----
  output$plot_perclass <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi
    input$fil_kategori

    metric_colors <- c(Precision = "#6C63FF", Recall = "#43C6AC")
    plot_ly(per_class_df,
      x = ~Class, y = ~Value, color = ~Metric,
      colors = metric_colors,
      type = "bar",
      hovertemplate = "%{x}<br>%{fullData.name}: <b>%{y:.1f}%</b><extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        paper_bgcolor = "transparent",
        plot_bgcolor = "#f8f9fa",
        xaxis = list(title = "Label Rekomendasi", color = "#333"),
        yaxis = list(
          title = "Nilai (%)",
          color = "#333",
          range = c(0, 110),
          rangemode = "nonnegative"
        ),
        legend = list(
          orientation = "h",
          x = 0.25, y = 1.12
        ),
        margin = list(t = 40, b = 50, l = 60, r = 20)
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;

          // Grafik ini memiliki multiple traces karena di-group (Precision & Recall)
          var n_traces = gd.data.length;
          var original_y = [];
          var zero_y = [];
          var traces_idx = [];

          for (var i = 0; i < n_traces; i++) {
            var y_arr = gd.data[i].y.slice();
            original_y.push({ y: y_arr });
            zero_y.push(y_arr.map(function() { return 0; }));
            traces_idx.push(i);
          }

          // Set semua nilai Y ke 0 terlebih dahulu agar bar tersembunyi di bawah
          Plotly.restyle(gd, {y: zero_y}, traces_idx);

          // Animasikan ke nilai aslinya bersamaan
          setTimeout(function() {
            Plotly.animate(gd, {
              data: original_y,
              traces: traces_idx
            }, {
              transition: { duration: 1000, easing: 'elastic-in-out' },
              frame:      { duration: 1000, redraw: false }
            });
          }, 200);
        }
      ") %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ---- SIMULATOR PREDIKSI ----

  # Auto-update Kategori Harga berdasarkan input harga
  observeEvent(input$sim_harga, {
    h <- input$sim_harga
    kat <- "Gratis"
    if (is.na(h) || h <= 0) {
      kat <- "Gratis"
    } else if (h >= 1 && h <= 9999) {
      kat <- "Murah"
    } else if (h >= 10000 && h <= 19999) {
      kat <- "Sedang"
    } else if (h >= 20000) {
      kat <- "Mahal"
    }
    updateTextInput(session, "sim_kat_harga", value = kat)
  })

  output$pred_result <- renderUI({
    tags$span(style = "color:#888;", "👆 Isi form lalu klik Prediksi")
  })

  observeEvent(input$btn_predict, {
    # Validasi Input Kosong
    if (is.na(input$sim_lat) || is.na(input$sim_long) || is.na(input$sim_rating) || is.na(input$sim_review) || is.na(input$sim_harga)) {
      startAnim(session, "sim_input_panel", "shake")
      showNotification("Semua kolom input harus diisi!", type = "error")
      return()
    }

    # Animasi Tombol & Disable sementara
    shinyjs::disable("btn_predict")
    startAnim(session, "btn_predict", "pulse")

    # Kosongkan hasil (trigger spinner)
    output$pred_result <- renderUI({
      HTML("")
    })

    # Simulasi Loading
    Sys.sleep(0.8)

    new_data <- data.frame(
      lat = input$sim_lat,
      long = input$sim_long,
      rating = input$sim_rating,
      jumlah_riview = input$sim_review,
      harga_num = input$sim_harga,
      kategori_harga = factor(input$sim_kat_harga,
        levels = levels(rf_result$train$kategori_harga)
      )
    )
    pred <- predict(rf_model, new_data)
    colors <- c(Terbaik = "#27ae60", Baik = "#3498db", Sedang = "#f39c12", Buruk = "#e74c3c")
    icons <- c(Terbaik = "⭐", Baik = "👍", Sedang = "👌", Buruk = "⚠️")
    descs <- c(
      Terbaik = "Destinasi ini diprediksi TERBAIK untuk dikunjungi!",
      Baik    = "Destinasi ini diprediksi BAIK untuk dikunjungi.",
      Sedang  = "Destinasi ini diprediksi SEDANG — cukup layak dikunjungi.",
      Buruk   = "Destinasi ini diprediksi BURUK — pertimbangkan alternatif lain."
    )
    p_char <- as.character(pred)
    col <- colors[p_char]
    ico <- icons[p_char]
    dsc <- descs[p_char]

    # Sweet Alert untuk semua kategori
    if (p_char == "Terbaik") {
      sendSweetAlert(session, title = "Luar Biasa! 🎉✨", text = "Destinasi Terbaik Ditemukan! Sangat direkomendasikan.", type = "success")
    } else if (p_char == "Baik") {
      sendSweetAlert(session, title = "Bagus! 👍", text = "Destinasi ini diprediksi BAIK untuk dikunjungi.", type = "info")
    } else if (p_char == "Sedang") {
      sendSweetAlert(session, title = "Cukup Menarik 👌", text = "Destinasi ini masuk kategori SEDANG, bisa jadi alternatif pilihan.", type = "info")
    } else if (p_char == "Buruk") {
      sendSweetAlert(session, title = "Pertimbangkan Lagi ⚠️", text = "Destinasi ini diprediksi BURUK, sebaiknya cari opsi lain.", type = "warning")
    }

    # Render Hasil Sequential
    output$pred_result <- renderUI({
      tags$div(
        tags$div(id = "sim_res_main", style = paste0("color:", col, ";"), paste0(ico, " ", p_char)),
        tags$div(id = "sim_res_desc", class = "hidden-anim-box", style = "font-size:14px;color:#666;margin-top:5px;", dsc)
      )
    })

    # Jalankan animasi reveal
    shinyjs::delay(300, {
      shinyjs::removeClass(id = "sim_res_desc", class = "hidden-anim-box")
      startAnim(session, "sim_res_desc", "fadeInUp")
      shinyjs::enable("btn_predict")
    })
  })

  # ---- FULL TABLE ----
  output$tbl_full <- renderDT({
    d <- df_filtered() %>%
      select(
        Nama = nama_wisata,
        Kategori = kategori,
        Kabupaten = kabupaten,
        Provinsi = provinsi,
        Rating = rating,
        Review = jumlah_riview,
        `Harga (Rp)` = harga_num,
        `Kat. Harga` = kategori_harga,
        `Label Rekomendasi` = label_rekomendasi,
        Klaster = cluster
      )
    datatable(d,
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      class = "compact stripe hover"
    ) %>%
      formatCurrency("Harga (Rp)", currency = "Rp ", digits = 0, mark = ",")
  })

  # ============================================================
  # SYLVA CHATBOT – Delegasi Server
  # ============================================================
  sylva_chatbot_server(input, output, session, df_chatbot, cluster_summary, rf_cm)
}

# ============================================================
# RUN
# ============================================================
shinyApp(ui = ui, server = server)
