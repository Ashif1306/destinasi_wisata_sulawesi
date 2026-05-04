# ============================================================
# UI — Analisis Destinasi Wisata Sulawesi
# ============================================================

ui <- dashboardPage(
  title = "Kelompok 8 - Destinasi Wisata Sulawesi",
  skin = "black",
  dashboardHeader(
    title = tags$span(
      style = "font-size: 18px; font-weight: 700; letter-spacing: 0.5px;",
      tags$img(
        src = "https://cdn-icons-png.flaticon.com/512/684/684908.png",
        height = "22px", style = "margin-right:6px;vertical-align:middle;"
      ),
      "Wisata Sulawesi"
    ),
    titleWidth = 260,
    # Tombol Search di navbar kanan (hanya tampil di mobile)
    tags$li(
      class = "dropdown",
      id = "mobile-search-toggle",
      tags$a(
        href = "#",
        onclick = "toggleMobileSearch(); return false;",
        icon("search")
      )
    )
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
    sliderInput("fil_rating", HTML("<i class='fa fa-star' style='color:#F7971E;margin-right:5px;'></i> Minimal Rating:"),
      min = 1.0, max = 5.0, value = c(1.0, 5.0), step = 0.1, ticks = FALSE
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
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js"),
      # Embed daftar nama wisata untuk autocomplete client-side
      tags$script(HTML(paste0(
        "var wisataNames = ",
        jsonlite::toJSON(sort(unique(as.character(df_final$nama_wisata)))),
        ";"
      )))
    ),
    # ---- Full-width Mobile Search Panel (fixed, di atas segalanya) ----
    tags$div(
      id = "mobile-search-panel",
      # Baris input
      tags$div(
        id = "mobile-search-panel-bar",
        tags$button(
          id      = "mobile-search-back",
          onclick = "closeMobileSearch(); return false;",
          icon("arrow-left")
        ),
        tags$input(
          id             = "mobile_search_native",
          type           = "text",
          class          = "mobile-search-native-input",
          placeholder    = "Cari destinasi wisata...",
          autocomplete   = "off",
          autocorrect    = "off",
          autocapitalize = "none",
          spellcheck     = "false"
        ),
        tags$button(
          id      = "mobile-search-clear",
          onclick = "clearMobileSearch(); return false;",
          icon("times")
        )
      ),
      # Dropdown suggestion list
      tags$ul(id = "mobile-search-suggestions")
    ),
    # Overlay gelap + blur di belakang search panel
    tags$div(id = "mobile-search-overlay", onclick = "closeMobileSearch(); return false;"),
    sylva_chatbot_dependencies(),
    tabItems(
      # ========== TAB 1: OVERVIEW ==========
      tabItem(
        tabName = "overview",
        # ---- Top Filter Bar ----
        fluidRow(
          style = "overflow: visible !important;",
          div(
            class = "col-xs-6 col-sm-3",
            selectInput("ov_provinsi", "🌏 Provinsi:",
              choices = c("Semua" = "Semua", setNames(sort(unique(as.character(df_final$provinsi))), sort(unique(as.character(df_final$provinsi))))),
              selected = "Semua",
              width = "100%"
            )
          ),
          div(
            class = "col-xs-6 col-sm-3",
            selectInput("ov_kabupaten", "📍 Kabupaten:",
              choices = c("Semua" = "Semua"),
              selected = "Semua",
              width = "100%"
            )
          ),
          div(
            class = "col-xs-12 col-sm-3",
            selectInput("ov_kategori", "🏷️ Kategori Wisata:",
              choices = c("Semua" = "Semua", setNames(sort(unique(as.character(df_final$kategori))), sort(unique(as.character(df_final$kategori))))),
              selected = "Semua",
              width = "100%"
            )
          ),
          # Selectize untuk DESKTOP saja (disembunyikan di mobile via CSS)
          div(
            class = "col-xs-12 col-sm-3",
            selectizeInput("ov_search", "🔍 Cari Destinasi:",
              choices = c(" " = "", setNames(df_final$nama_wisata, df_final$nama_wisata)),
              selected = "",
              options = list(
                placeholder = "Ketik nama wisata...",
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          )
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
            uiOutput("kmeans_justification")
          )
        ),
        fluidRow(
          box(
            title = tags$div(
              class = "pca-card-header",
              tags$span(class = "pca-card-title", "🔵 Visualisasi Klaster PCA"),
              tags$div(
                class = "pca-dim-toggle",
                tags$button(
                  id = "btn_pca_2d",
                  class = "pca-dim-btn active",
                  "2D",
                  onclick = "setPcaDim('2D')"
                ),
                tags$button(
                  id = "btn_pca_3d",
                  class = "pca-dim-btn",
                  "3D",
                  onclick = "setPcaDim('3D')"
                )
              )
            ),
            width = 8, status = "primary", solidHeader = TRUE, height = 550,
            div(
              class = "pca-plot-wrapper",
              div(
                id = "pca_2d_panel", class = "pca-panel active",
                plotlyOutput("plot_pca_2d", height = 490)
              ),
              div(
                id = "pca_3d_panel", class = "pca-panel",
                plotlyOutput("plot_pca_3d", height = 490)
              )
            )
          ),
          box(
            title = "📊 Distribusi Klaster per Provinsi", width = 4,
            status = "success", solidHeader = TRUE, height = 550,
            plotlyOutput("plot_cluster_provinsi", height = 490)
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
              "ℹ️ Prediksi 4 kelas (Terbaik / Baik / Sedang / Buruk) berdasarkan rating, jumlah review, harga, dan kategori harga."
            ),
            tags$div(
              id = "sim_input_panel",
              fluidRow(
                column(3, numericInput("sim_rating", "Rating", value = 4.3, min = 1, max = 5, step = 0.1)),
                column(3, numericInput("sim_review", "Jumlah Review", value = 1000, min = 10)),
                column(3, numericInput("sim_harga", "Harga (Rp)", value = 10000, min = 0)),
                column(3, shinyjs::disabled(textInput("sim_kat_harga", "Kategori Harga", value = "Sedang")))
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
        # --- Baris toolbar download ---
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-toolbar",
              div(
                class = "dl-toolbar-left",
                tags$span(
                  class = "dl-info-text",
                  icon("database"), " Data aktif mengikuti filter Provinsi & Kategori Wisata di sidebar kiri."
                )
              ),
              div(
                class = "dl-toolbar-right",
                actionButton("btn_dl_modal",
                  label = tagList(icon("download"), " Unduh Data"),
                  class = "btn-dl-main"
                )
              )
            )
          )
        ),
        # --- Modal download (tersembunyi, ditampilkan via JS) ---
        div(
          id = "dl_modal_overlay", class = "dl-modal-overlay",
          div(
            class = "dl-modal-box",
            div(
              class = "dl-modal-header",
              tags$span(icon("download"), " Opsi Unduh Data"),
              actionButton("btn_dl_close", label = icon("times"), class = "btn-dl-close")
            ),
            div(
              class = "dl-modal-body",
              
              # ========== SECTION 1: FILTER DATA (dengan dropdown lanjutan) ==========
              div(
                class = "dl-section",
                tags$p(class = "dl-section-title", icon("filter"), " Filter Data"),
                div(class = "dl-check-all-wrapper",
                  checkboxInput("dl_check_all", "Pilih Semua Kolom", value = TRUE)
                ),
                tags$hr(class = "dl-divider-sm"),
                div(class = "dl-filter-grid",
                  
                  # Kategori
                  div(class = "dl-filter-item",
                    div(class = "dl-filter-label",
                      checkboxInput("dl_col_kategori", "Kategori Wisata", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.dl_col_kategori == true",
                      pickerInput("dl_opt_kategori", NULL,
                        choices = sort(unique(as.character(df_final$kategori))),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `none-selected-text` = "Semua Kategori",
                                       `select-all-text` = "Pilih Semua", `deselect-all-text` = "Kosongkan"),
                        width = "100%")
                    )
                  ),
                  
                  # Klaster
                  div(class = "dl-filter-item",
                    div(class = "dl-filter-label",
                      checkboxInput("dl_col_klaster", "Klaster", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.dl_col_klaster == true",
                      pickerInput("dl_opt_klaster", NULL,
                        choices = sort(unique(df_final$cluster)),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `none-selected-text` = "Semua Klaster",
                                       `select-all-text` = "Pilih Semua", `deselect-all-text` = "Kosongkan"),
                        width = "100%")
                    )
                  ),
                  
                  # Provinsi
                  div(class = "dl-filter-item",
                    div(class = "dl-filter-label",
                      checkboxInput("dl_col_provinsi", "Provinsi", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.dl_col_provinsi == true",
                      pickerInput("dl_opt_provinsi", NULL,
                        choices = sort(unique(as.character(df_final$provinsi))),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `none-selected-text` = "Semua Provinsi",
                                       `select-all-text` = "Pilih Semua", `deselect-all-text` = "Kosongkan",
                                       `live-search` = TRUE),
                        width = "100%")
                    )
                  ),
                  
                  # Label Rekomendasi
                  div(class = "dl-filter-item",
                    div(class = "dl-filter-label",
                      checkboxInput("dl_col_label", "Label Rekomendasi", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.dl_col_label == true",
                      pickerInput("dl_opt_label", NULL,
                        choices = sort(unique(as.character(df_final$label_rekomendasi))),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `none-selected-text` = "Semua Label",
                                       `select-all-text` = "Pilih Semua", `deselect-all-text` = "Kosongkan"),
                        width = "100%")
                    )
                  )
                )
              ),
              
              tags$hr(class = "dl-divider"),
              
              # ========== SECTION 2: PILIH KOLOM (checkbox biasa, tanpa dropdown) ==========
              div(
                class = "dl-section",
                tags$p(class = "dl-section-title", icon("columns"), " Pilih Kolom Lainnya"),
                div(class = "dl-simple-grid",
                  div(class = "dl-simple-item", checkboxInput("dl_col_nama",     "Nama Wisata",    value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_kabupaten","Kabupaten",       value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_rating",   "Rating",          value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_review",   "Jumlah Review",   value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_harga",    "Harga (Rp)",      value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_katharga", "Kategori Harga",  value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_alamat",   "Alamat",          value = TRUE)),
                  div(class = "dl-simple-item", checkboxInput("dl_col_deskripsi","Deskripsi Wisata",value = TRUE))
                )
              ),
              
              tags$hr(class = "dl-divider"),
              # --- Format file ---
              div(
                class = "dl-section",
                tags$p(class = "dl-section-title", icon("file-alt"), " Format File"),
                div(
                  class = "dl-format-row",
                  downloadButton("dl_csv", label = " Unduh CSV", class = "btn-dl-format"),
                  downloadButton("dl_excel", label = " Unduh Excel", class = "btn-dl-format btn-dl-excel")
                )
              )
            )
          )
        ),
        # --- Tabel utama (tanpa tombol DT bawaan) ---
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
