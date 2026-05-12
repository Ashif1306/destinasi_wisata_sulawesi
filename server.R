# ============================================================
# SERVER — Analisis Destinasi Wisata Sulawesi
# ============================================================

server <- function(input, output, session) {
  
  # ============================================================
  # SIDEBAR FILTER (untuk tab K-Means, RF, Dataset)
  # ============================================================
  df_filtered <- reactive({
    d <- df_final
    if (length(input$fil_provinsi) > 0 && input$fil_provinsi != "Semua") d <- d %>% filter(provinsi == input$fil_provinsi)
    if (length(input$fil_kategori) > 0 && input$fil_kategori != "Semua") d <- d %>% filter(kategori == input$fil_kategori)
    if (length(input$fil_rating) == 2) d <- d %>% filter(rating >= input$fil_rating[1] & rating <= input$fil_rating[2])
    d
  })
  
  # Reactive untuk data PCA (sinkron mutlak dengan df_filtered)
  df_pca_filtered <- reactive({
    valid_names <- df_filtered()$nama_wisata
    df_pca %>% filter(nama %in% valid_names)
  })
  
  # Efek Pulse pada UI saat Rating diubah (Interaktivitas Tambahan)
  observeEvent(input$fil_rating, {
    shinyjs::runjs("$('.small-box').addClass('rating-update-pulse'); setTimeout(function(){ $('.small-box').removeClass('rating-update-pulse'); }, 400);")
  }, ignoreInit = TRUE)
  
  # ============================================================
  # OVERVIEW
  # ============================================================
  
  # Catatan: ov_search sudah diinisialisasi dengan choices lengkap di ui.R
  # (choices=NULL dihapus agar Shiny tidak memasang server-side AJAX callback)
  
  
  # Cascading dropdown: Menggunakan JavaScript murni untuk update Selectize
  # Ini adalah solusi mutlak untuk bug Shiny Selectize yang macet (cache nyangkut)
  observeEvent(input$ov_provinsi, {
    provinsi_val <- input$ov_provinsi
    if (is.null(provinsi_val) || provinsi_val == "Semua") {
      kab_choices <- sort(unique(as.character(df_final$kabupaten)))
    } else {
      kab_choices <- sort(unique(as.character(
        df_final$kabupaten[df_final$provinsi == provinsi_val]
      )))
    }
    kab_choices <- kab_choices[!is.na(kab_choices) & nzchar(kab_choices)]
    
    # Kirim data pilihan ke browser via websocket (JavaScript handler)
    session$sendCustomMessage("updateKabupatenJS", list(
      choices = kab_choices
    ))
  }, ignoreInit = FALSE)
  
  # ---- MOBILE SEARCH: terima nilai dari native <input> via Shiny.setInputValue ----
  # input$mobile_search_query: partial text (dari custom.js debounce)
  # input$ov_search           : exact name (dari selectize desktop, hanya update saat dipilih)
  
  selected_wisata <- reactive({
    mob  <- trimws(input$mobile_search_query %||% "")
    desk <- trimws(input$ov_search %||% "")
    
    if (nzchar(mob)) {
      # Mobile: exact match saja — panel detail hanya tampil saat nama cocok persis.
      res <- df_final %>% filter(tolower(nama_wisata) == tolower(mob)) %>% slice(1)
      if (nrow(res) > 0) res else NULL
    } else if (nzchar(desk) && desk != " ") {
      res <- df_final %>% filter(nama_wisata == desk) %>% slice(1)
      if (nrow(res) > 0) res else NULL
    } else {
      NULL
    }
  })
  
  # ---- BUG FIX #1: Auto-redirect ke Overview saat mobile search memilih destinasi ----
  # Saat user ada di tab lain (K-Means / RF / Dataset) dan memilih wisata dari
  # mobile search panel, otomatis pindah ke tab Overview agar detail panel tampil.
  observeEvent(input$mobile_search_query, {
    mob <- trimws(input$mobile_search_query %||% "")
    if (!nzchar(mob)) return()
    # Cek apakah ada hasil exact match
    res <- df_final %>% filter(tolower(nama_wisata) == tolower(mob)) %>% slice(1)
    if (nrow(res) > 0) {
      # Pindah ke tab Overview agar detail panel muncul
      updateTabItems(session, "sidebar", selected = "overview")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # df_overview: filter biasa (TANPA mobile search agar UI tidak nge-freeze saat mengetik)
  df_overview <- reactive({
    d <- df_final
    if (length(input$ov_provinsi)  > 0 && input$ov_provinsi  != "Semua") d <- d %>% filter(provinsi  == input$ov_provinsi)
    if (length(input$ov_kabupaten) > 0 && input$ov_kabupaten != "Semua") d <- d %>% filter(kabupaten == input$ov_kabupaten)
    if (length(input$ov_kategori)  > 0 && input$ov_kategori  != "Semua") d <- d %>% filter(kategori  == input$ov_kategori)
    # Filter Global dari sidebar
    if (length(input$fil_rating) == 2) d <- d %>% filter(rating >= input$fil_rating[1] & rating <= input$fil_rating[2])
    d
  })
  
  # ---- VALUE BOXES ----
  output$vb_total <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(value = sel$rating, subtitle = "Rating Destinasi", icon = icon("star"), color = "yellow")
    } else {
      valueBox(value = nrow(df_overview()), subtitle = "Total Destinasi Wisata", icon = icon("map-marker-alt"), color = "purple")
    }
  })
  
  output$vb_rating <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(value = formatC(sel$jumlah_riview, format = "d", big.mark = "."), subtitle = "Jumlah Review", icon = icon("comments"), color = "aqua")
    } else {
      valueBox(value = round(mean(df_overview()$rating, na.rm = TRUE), 2), subtitle = "Rata-rata Rating", icon = icon("star"), color = "yellow")
    }
  })
  
  output$vb_gratis <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(value = as.character(sel$kategori_harga), subtitle = paste0("Harga: Rp ", formatC(sel$harga_num, format = "d", big.mark = ".")), icon = icon("tag"), color = "orange")
    } else {
      n_total <- nrow(df_overview()); n_gratis <- sum(df_overview()$kategori_harga == "Gratis", na.rm = TRUE)
      pct <- ifelse(n_total > 0, round(n_gratis / n_total * 100, 1), 0)
      valueBox(value = paste0(pct, "%"), subtitle = paste0("Wisata Gratis (", n_gratis, " dari ", n_total, ")"), icon = icon("gift"), color = "green")
    }
  })
  
  output$vb_reviews <- renderValueBox({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      valueBox(value = paste0("Klaster ", sel$cluster), subtitle = "Segmen K-Means", icon = icon("project-diagram"), color = "purple")
    } else {
      total_reviews <- sum(df_overview()$jumlah_riview, na.rm = TRUE)
      valueBox(value = formatC(total_reviews, format = "d", big.mark = "."), subtitle = "Total Interaksi Review", icon = icon("comments"), color = "aqua")
    }
  })
  
  # ---- DETAIL PANEL ----
  output$detail_panel <- renderUI({
    sel <- selected_wisata()
    if (is.null(sel) || nrow(sel) == 0) return(NULL)
    
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
      "Wisata Alam" = "\U0001F33F",
      "Wisata Budaya & Sejarah" = "\U0001F3DB\uFE0F",
      "Wisata Religi" = "\U0001F54C",
      "Wisata Kota / Landmark" = "\U0001F3D9\uFE0F",
      "Wisata Hiburan" = "\U0001F3A1"
    )
    kat <- as.character(sel$kategori)
    bg  <- if (kat %in% names(placeholder_colors)) placeholder_colors[[kat]] else "linear-gradient(135deg, #6C63FF, #3F3D56)"
    ico <- if (kat %in% names(placeholder_icons)) placeholder_icons[[kat]] else "\U0001F4CD"
    
    if (has_image) {
      img_tag <- tags$img(src = img_url, class = "detail-img", referrerpolicy = "no-referrer",
                          onerror = paste0("this.onerror=null; this.style.display='none'; this.parentNode.querySelector('.fallback-placeholder').style.display='flex';"))
      fallback_tag <- div(class = "placeholder-img fallback-placeholder", style = paste0("background:", bg, "; display:none;"), ico)
      image_block <- div(img_tag, fallback_tag)
    } else {
      image_block <- div(class = "placeholder-img", style = paste0("background:", bg, ";"), ico)
    }
    
    desc_text <- as.character(sel$deskripsi_wisata)
    if (is.na(desc_text) || desc_text == "" || desc_text == "-") desc_text <- "Deskripsi untuk destinasi ini belum tersedia."
    
    fluidRow(box(
      title = paste0("\U0001F4F8 Detail: ", sel$nama_wisata), width = 12,
      status = "primary", solidHeader = TRUE,
      fluidRow(
        column(5, image_block),
        column(7,
               div(class = "detail-title", sel$nama_wisata),
               div(class = "detail-meta", paste0(
                 "\U0001F4CD ", sel$kabupaten, ", ", sel$provinsi,
                 "  |  \U0001F3F7\uFE0F ", sel$kategori,
                 "  |  \u2B50 ", sel$rating,
                 "  |  \U0001F4AC ", formatC(sel$jumlah_riview, format = "d", big.mark = "."), " review"
               )),
               div(class = "detail-desc", desc_text),
               actionButton("btn_reset_search", "Kembali ke Overview",
                            icon = icon("arrow-left"), class = "btn-warning btn-sm",
                            style = "float: right; margin-top: 15px; font-weight: bold;")
        )
      )
    ))
  })
  
  observeEvent(input$btn_reset_search, {
    updateSelectizeInput(session, "ov_search", selected = " ")
    # Reset mobile search query juga agar tampilan mobile kembali ke overview
    session$sendCustomMessage("resetMobileSearch", list())
  })
  
  
  # ---- BUG FIX #2: Reset detail mode saat filter overview atau sidebar diubah ----
  # Ketika user sedang melihat detail wisata via mobile search, lalu mengubah
  # filter (Provinsi / Kabupaten / Kategori / filter sidebar), tampilan harus
  # kembali ke mode overview umum — persis seperti perilaku di desktop.
  observeEvent(
    c(input$ov_provinsi, input$ov_kabupaten, input$ov_kategori,
      input$fil_provinsi, input$fil_kategori, input$fil_rating),
    {
      # Reset desktop search
      if (!is.null(input$ov_search) && nzchar(trimws(input$ov_search))) {
        updateSelectizeInput(session, "ov_search", selected = " ")
      }
      # Reset mobile search — ini yang menyebabkan detail panel tidak hilang sebelumnya
      mob <- trimws(input$mobile_search_query %||% "")
      if (nzchar(mob)) {
        session$sendCustomMessage("resetMobileSearch", list())
      }
    },
    ignoreInit = TRUE
  )
  
  # ---- PETA LEAFLET ----
  pal_cluster <- colorFactor(
    palette = c("#6C63FF", "#FF6584", "#43C6AC", "#F7971E"),
    domain  = sort(unique(df_final$cluster))
  )
  
  output$map_overview <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 121.5, lat = -1.5, zoom = 6) %>%
      addLegend("bottomright", pal = pal_cluster, values = sort(unique(df_final$cluster)),
                title = "Cluster Wisata", opacity = 0.9)
  })
  
  observe({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) {
      d_map <- sel
    } else {
      d_map <- df_overview() %>% filter(!is.na(lat) & !is.na(long))
    }
    proxy <- leafletProxy("map_overview", data = d_map) %>% clearMarkerClusters() %>% clearMarkers()
    if (nrow(d_map) > 0) {
      proxy %>% addCircleMarkers(lng = ~long, lat = ~lat, radius = 7, weight = 1, fillOpacity = 0.8,
                                 color = "#ffffff", fillColor = ~pal_cluster(as.character(cluster)),
                                 popup = ~paste0("<b>", nama_wisata, "</b><br>",
                                                 "\U0001F4CD ", kabupaten, ", ", provinsi, "<br>",
                                                 "\u2B50 Rating: ", rating, " | \U0001F4AC ", jumlah_riview, " review<br>",
                                                 "\U0001F4B0 ", kategori_harga, " (Rp ", formatC(harga_num, format = "d", big.mark = "."), ")"),
                                 clusterOptions = if (nrow(d_map) > 1) markerClusterOptions() else NULL)
    }
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
  
  # ---- DUAL CHARTS PANEL ----
  output$dual_charts_panel <- renderUI({
    sel <- selected_wisata()
    if (!is.null(sel) && nrow(sel) > 0) return(NULL)
    fluidRow(
      div(class = "dual-chart-box chart-bar-box col-sm-7",
        box(title = "\U0001F4CA Jumlah Wisata per Kategori", width = 12,
          status = "info", solidHeader = TRUE, height = 440, 
          div(class = "chart-desktop-only", plotlyOutput("bar_kategori_desktop", height = 380)),
          div(class = "chart-mobile-only", plotlyOutput("bar_kategori_mobile", height = 380))
        )
      ),
      div(class = "dual-chart-box chart-pie-box col-sm-5",
        box(title = "\U0001F369 Proporsi Label Rekomendasi (Prediksi RF)", width = 12,
          status = "success", solidHeader = TRUE, height = 440, plotlyOutput("pie_rekomendasi", height = 380))
      )
    )
  })

  # ---- BAR CHART: Kategori Wisata (DESKTOP) ----
  output$bar_kategori_desktop <- renderPlotly({
    d <- df_overview() %>% count(kategori, name = "n") %>% arrange(desc(n)) %>% head(10) %>%
      mutate(
        label_short = sub("^Wisata ", "", kategori),
        label_short = ifelse(nchar(label_short) > 16, paste0(substr(label_short, 1, 14), "\u2026"), label_short)
      )
    n_rows  <- nrow(d)
    bar_pal <- colorRampPalette(c("#818cf8", "#4338ca"))(n_rows)
    
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~n, y = ~reorder(label_short, n), type = "bar", orientation = "h",
      marker = list(color = bar_pal, line = list(color = "rgba(255,255,255,0.3)", width = 1), opacity = 1),
      hovertemplate = paste0("<b>%{customdata}</b><br>Jumlah: <b>%{x}</b> destinasi<extra></extra>"),
      customdata = ~kategori) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "rgba(248,249,250,0.55)",
        xaxis = list(title = "Jumlah Destinasi", color = "#333", gridcolor = "rgba(200,200,200,0.4)",
          zeroline = FALSE, range = c(0, max(d$n, na.rm = TRUE) * 1.15), tickangle = 0),
        yaxis = list(title = "", color = "#444", tickfont = list(size = 11, color = "#333"), automargin = TRUE),
        margin = list(l = 10, r = 25, t = 10, b = 45), showlegend = FALSE,
        hoverlabel = list(bgcolor = "#0A192F", font = list(color = "#fff", size = 13), bordercolor = "#6C63FF")) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ---- BAR CHART: Kategori Wisata (MOBILE VERTIKAL) ----
  output$bar_kategori_mobile <- renderPlotly({
    d <- df_overview() %>% count(kategori, name = "n") %>% arrange(desc(n)) %>% head(10) %>%
      mutate(
        label_short = sub("^Wisata ", "", kategori),
        label_short = ifelse(nchar(label_short) > 16, paste0(substr(label_short, 1, 14), "\u2026"), label_short)
      )
    n_rows  <- nrow(d)
    bar_pal <- colorRampPalette(c("#818cf8", "#4338ca"))(n_rows)
    
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~reorder(label_short, -n), y = ~n, type = "bar", orientation = "v",
      marker = list(color = bar_pal, line = list(color = "rgba(255,255,255,0.3)", width = 1), opacity = 1),
      hovertemplate = paste0("<b>%{customdata}</b><br>Jumlah: <b>%{y}</b> destinasi<extra></extra>"),
      customdata = ~kategori) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "rgba(248,249,250,0.55)",
        xaxis = list(title = "", color = "#333", tickfont = list(size = 10, color = "#333"), automargin = TRUE, tickangle = -45),
        yaxis = list(title = "", color = "#444", gridcolor = "rgba(200,200,200,0.4)", zeroline = FALSE, range = c(0, max(d$n, na.rm = TRUE) * 1.15)),
        margin = list(l = 30, r = 10, t = 10, b = 10), showlegend = FALSE,
        hoverlabel = list(bgcolor = "#0A192F", font = list(color = "#fff", size = 13), bordercolor = "#6C63FF")) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- PIE CHART: Proporsi Label Rekomendasi ----
  output$pie_rekomendasi <- renderPlotly({
    d <- df_overview() %>% count(label_rekomendasi, name = "n") %>% arrange(label_rekomendasi)
    pie_colors <- c("Terbaik" = "#27ae60", "Baik" = "#3498db", "Sedang" = "#f39c12", "Buruk" = "#e74c3c")
    total    <- sum(d$n)
    n_slices <- nrow(d)
    plot_ly(d, labels = ~label_rekomendasi, values = ~n, type = "pie", hole = 0.48,
            marker = list(colors = pie_colors[as.character(d$label_rekomendasi)], line = list(color = "#ffffff", width = 2.5)),
            pull = rep(0, n_slices), textinfo = "label+percent",
            textfont = list(size = 13, color = "#fff"), insidetextorientation = "radial",
            hovertemplate = paste0("<b>%{label}</b><br>Jumlah: <b>%{value}</b> destinasi<br>Proporsi: <b>%{percent}</b><extra></extra>"),
            rotation = -90, direction = "clockwise") %>%
      layout(paper_bgcolor = "transparent", showlegend = TRUE,
             legend = list(orientation = "h", x = 0.05, y = -0.08, font = list(size = 12)),
             annotations = list(list(
               text = paste0("<b>", total, "</b><br><span style='font-size:10px'>destinasi</span>"),
               x = 0.5, y = 0.5, font = list(size = 16, color = "#0A192F"), showarrow = FALSE)),
             hoverlabel = list(bgcolor = "#0A192F", font = list(color = "#fff", size = 13), bordercolor = "#20C997")) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;
          var nSlices = gd.data[0].labels.length;
          var basePull = new Array(nSlices).fill(0);
          gd.on('plotly_hover', function(evt) {
            var idx = evt.points[0].pointNumber;
            var pull = basePull.slice();
            pull[idx] = 0.1;
            Plotly.restyle(gd, { pull: [pull] }, [0]);
          });
          gd.on('plotly_unhover', function() {
            Plotly.restyle(gd, { pull: [basePull] }, [0]);
          });
        }
      ")
  })
  
  # ============================================================
  # K-MEANS
  # ============================================================
  
  # ---- PCA 2D PLOT ----
  output$plot_pca_2d <- renderPlotly({
    d <- df_pca_filtered()
    req(nrow(d) > 0)
    
    clust_labels <- c("1" = "Klaster 1", "2" = "Klaster 2", "3" = "Klaster 3", "4" = "Klaster 4")
    
    plot_ly(d, x = ~PC1, y = ~PC2, color = ~cluster, colors = cluster_colors,
            type = "scatter", mode = "markers",
            text = ~paste0("<b>", nama, "</b><br>",
                           "\U0001F4CD ", provinsi, "<br>",
                           "Klaster: <b>", cluster, "</b><br>",
                           "PC1: ", round(PC1, 3), " | PC2: ", round(PC2, 3)),
            hoverinfo = "text",
            marker = list(size = 8, opacity = 0.82,
                          line = list(color = "rgba(255,255,255,0.5)", width = 0.8))) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "rgba(248,249,250,0.6)",
        xaxis = list(
          title = list(text = "Principal Component 1 (PC1)", font = list(size = 12, color = "#555")),
          zeroline = TRUE, zerolinecolor = "rgba(150,150,150,0.4)", zerolinewidth = 1,
          gridcolor = "rgba(200,200,200,0.35)", tickfont = list(size = 11)
        ),
        yaxis = list(
          title = list(text = "Principal Component 2 (PC2)", font = list(size = 12, color = "#555")),
          zeroline = TRUE, zerolinecolor = "rgba(150,150,150,0.4)", zerolinewidth = 1,
          gridcolor = "rgba(200,200,200,0.35)", tickfont = list(size = 11)
        ),
        legend = list(
          title = list(text = "<b>Klaster</b>"),
          orientation = "h", x = 0.01, y = -0.15,
          font = list(size = 11)
        ),
        margin = list(l = 50, r = 20, t = 15, b = 60),
        hoverlabel = list(
          bgcolor  = "#0A192F",
          font     = list(color = "#fff", size = 12),
          bordercolor = "#6C63FF"
        )
      ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- PCA 3D PLOT ----
  output$plot_pca_3d <- renderPlotly({
    d <- df_pca_filtered()
    req(nrow(d) > 0)
    
    plot_ly(d, x = ~PC1, y = ~PC2, z = ~PC3,
            color = ~cluster, colors = cluster_colors,
            type = "scatter3d", mode = "markers",
            text = ~paste0("<b>", nama, "</b><br>",
                           "\U0001F4CD ", provinsi, "<br>",
                           "Klaster: <b>", cluster, "</b><br>",
                           "PC1: ", round(PC1, 3),
                           " | PC2: ", round(PC2, 3),
                           " | PC3: ", round(PC3, 3)),
            hoverinfo = "text",
            marker = list(
              size    = 4.5,
              opacity = 0.85,
              line    = list(color = "rgba(255,255,255,0.4)", width = 0.5)
            )) %>%
      layout(
        paper_bgcolor = "transparent",
        scene = list(
          bgcolor = "rgba(248,249,250,0.0)",
          xaxis = list(
            title      = "PC1",
            gridcolor  = "rgba(180,180,180,0.35)",
            showbackground = TRUE,
            backgroundcolor = "rgba(240,244,255,0.55)",
            tickfont   = list(size = 10)
          ),
          yaxis = list(
            title      = "PC2",
            gridcolor  = "rgba(180,180,180,0.35)",
            showbackground = TRUE,
            backgroundcolor = "rgba(240,244,255,0.55)",
            tickfont   = list(size = 10)
          ),
          zaxis = list(
            title      = "PC3",
            gridcolor  = "rgba(180,180,180,0.35)",
            showbackground = TRUE,
            backgroundcolor = "rgba(240,244,255,0.55)",
            tickfont   = list(size = 10)
          ),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 0.9)
          )
        ),
        legend = list(
          title = list(text = "<b>Klaster</b>"),
          font  = list(size = 11),
          x = 0.01, y = 0.98
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        hoverlabel = list(
          bgcolor     = "#0A192F",
          font        = list(color = "#fff", size = 12),
          bordercolor = "#43C6AC"
        )
      ) %>%
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE, responsive = TRUE)
  })
  
  output$tbl_cluster_summary <- renderDT({
    cs <- cluster_summary %>%
      left_join(df_final %>% group_by(cluster) %>%
                  summarise(top_provinsi = names(which.max(table(provinsi))), .groups = "drop"), by = "cluster") %>%
      select(Klaster = cluster, Label = label, N = n, `Avg Rating` = avg_rating,
             `Avg Review` = avg_reviews, `Avg Harga` = avg_harga, `Dominan` = top_provinsi)
    datatable(cs, options = list(dom = "t", pageLength = 10), rownames = FALSE, class = "compact stripe") %>%
      formatCurrency("Avg Harga", currency = "Rp ", digits = 0, mark = ",")
  })
  
  output$plot_cluster_provinsi <- renderPlotly({
    d <- df_final %>% group_by(provinsi, cluster) %>% summarise(n = n(), .groups = "drop")
    plot_ly(d, x = ~provinsi, y = ~n, color = ~cluster, colors = cluster_colors, type = "bar") %>%
      layout(barmode = "stack", paper_bgcolor = "transparent", plot_bgcolor = "#f8f9fa",
             xaxis = list(title = "Provinsi", tickangle = -30),
             yaxis = list(title = "Jumlah Destinasi"),
             legend = list(title = list(text = "Klaster")))
  })
  
  output$plot_elbow <- renderPlotly({
    # --- WSS Line ---
    p <- plot_ly() %>%
      add_trace(data = elbow_df, x = ~k, y = ~WSS, type = "scatter", mode = "lines+markers",
                line = list(color = "#6C63FF", width = 3),
                marker = list(size = 10, color = "#6C63FF"),
                name = "WSS",
                hovertemplate = "k = %{x}<br>WSS = %{y:,.0f}<extra></extra>")

    # --- Highlight area: diminishing returns setelah k=4 (shading) ---
    k_cutoff <- 4
    p <- p %>% add_trace(
      x = c(elbow_df$k[k_cutoff:nrow(elbow_df)], rev(elbow_df$k[k_cutoff:nrow(elbow_df)])),
      y = c(elbow_df$WSS[k_cutoff:nrow(elbow_df)], rep(min(elbow_df$WSS) * 0.95, nrow(elbow_df) - k_cutoff + 1)),
      type = "scatter", mode = "none",
      fill = "toself", fillcolor = "rgba(108,99,255,0.08)",
      name = "Diminishing Returns", showlegend = FALSE, hoverinfo = "skip"
    )

    # --- Marker titik elbow dari analisis akselerasi ---
    p <- p %>% add_trace(
      x = best_k_elbow, y = elbow_df$WSS[best_k_elbow],
      type = "scatter", mode = "markers",
      marker = list(size = 16, color = "#43C6AC", symbol = "star",
                    line = list(color = "#fff", width = 2)),
      name = paste0("Elbow (k=", best_k_elbow, ")"),
      hoverinfo = "text",
      text = paste0("Elbow Point (Akselerasi Maks)\nk = ", best_k_elbow,
                    "\nWSS = ", round(elbow_df$WSS[best_k_elbow], 1))
    )

    # --- Marker k=4 (pilihan akhir) ---
    p <- p %>% add_trace(
      x = 4, y = elbow_df$WSS[4],
      type = "scatter", mode = "markers",
      marker = list(size = 16, color = "#e94560", symbol = "diamond",
                    line = list(color = "#fff", width = 2)),
      name = "k = 4 (Dipilih)",
      hoverinfo = "text",
      text = paste0("Dipilih k = 4\nWSS = ", round(elbow_df$WSS[4], 1),
                    "\nPenurunan WSS setelah k=4: <", wss_pct_drop[4], "%")
    )

    # --- Garis vertikal tipis di k=4 ---
    p <- p %>% layout(
      paper_bgcolor = "transparent", plot_bgcolor = "#f8f9fa",
      xaxis = list(title = "Jumlah Klaster (k)", dtick = 1),
      yaxis = list(title = "Within-Cluster Sum of Squares (WSS)"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.01, y = -0.18, font = list(size = 10)),
      shapes = list(
        list(type = "line", x0 = 4, x1 = 4,
             y0 = min(elbow_df$WSS) * 0.95, y1 = max(elbow_df$WSS) * 1.02,
             line = list(color = "#e94560", width = 1.5, dash = "dash"))
      ),
      annotations = list(
        list(x = 4.3, y = elbow_df$WSS[4],
             text = paste0("<b>k=4 dipilih</b><br>",
                           "<span style='font-size:10px'>Penurunan WSS melambat<br>setelah titik ini</span>"),
             showarrow = TRUE, arrowhead = 2, ax = 60, ay = -50,
             font = list(size = 11, color = "#e94560"),
             bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#e94560", borderpad = 4),
        list(x = best_k_elbow, y = elbow_df$WSS[best_k_elbow] * 1.05,
             text = paste0("⭐ Elbow k=", best_k_elbow),
             showarrow = FALSE,
             font = list(size = 10, color = "#43C6AC", family = "Arial Black"))
      )
    ) %>% plotly::config(displayModeBar = FALSE)
    p
  })

  output$plot_silhouette <- renderPlotly({
    sil_df <- elbow_df %>% filter(k > 1)

    # --- Warna bar: k=2 abu (dikecualikan), best_k_sil hijau, k=4 merah, lainnya teal ---
    bar_colors <- ifelse(sil_df$k == 2,           "#b0b8c1",
                  ifelse(sil_df$k == best_k_sil,  "#27ae60",
                  ifelse(sil_df$k == 4,            "#e94560",
                                                   "#43C6AC")))
    bar_opacity <- ifelse(sil_df$k == 2, 0.45, 1)

    p <- plot_ly(sil_df, x = ~k, y = ~Silhouette, type = "bar",
                 marker = list(color = bar_colors, opacity = bar_opacity,
                               line = list(color = "rgba(255,255,255,0.5)", width = 1)),
                 hovertemplate = "k = %{x}<br>Silhouette = %{y:.4f}<extra></extra>",
                 name = "Silhouette")

    # --- Line overlay hanya dari k=3 ke atas ---
    sil_df_from3 <- sil_df %>% filter(k >= 3)
    p <- p %>% add_trace(data = sil_df_from3, x = ~k, y = ~Silhouette,
                         type = "scatter", mode = "lines+markers",
                         line = list(color = "rgba(67,198,172,0.5)", width = 2),
                         marker = list(size = 8, color = "#43C6AC"),
                         name = "Trend (k>=3)", showlegend = FALSE,
                         hoverinfo = "skip")

    # --- Annotations ---
    annotations_list <- list(
      # Keterangan k=2 dikecualikan
      list(x = 2, y = elbow_df$Silhouette[2] * 1.01,
           text = "<i>k=2 tidak<br>dipertimbangkan</i>",
           showarrow = FALSE, yanchor = "bottom",
           font = list(size = 9, color = "#888")),
      # Optimal dari k>=3
      list(x = best_k_sil, y = elbow_df$Silhouette[best_k_sil],
           text = paste0("Terbaik (k>=3): k=", best_k_sil,
                         "<br>Sil=", round(elbow_df$Silhouette[best_k_sil], 4)),
           showarrow = TRUE, arrowhead = 2, ax = -55, ay = -40,
           font = list(size = 11, color = "#27ae60"),
           bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#27ae60", borderpad = 4)
    )

    # Anotasi k=4 hanya jika bukan sama dengan best_k_sil
    if (best_k_sil != 4) {
      sil_diff_pct <- round(
        (elbow_df$Silhouette[best_k_sil] - elbow_df$Silhouette[4]) /
          elbow_df$Silhouette[best_k_sil] * 100, 1
      )
      annotations_list[[3]] <- list(
        x = 4, y = elbow_df$Silhouette[4],
        text = paste0("<b>k=4 dipilih</b><br>",
                      "<span style='font-size:10px'>Sil=", round(elbow_df$Silhouette[4], 4),
                      "<br>Selisih hanya ", sil_diff_pct, "% dari optimal</span>"),
        showarrow = TRUE, arrowhead = 2, ax = 55, ay = -45,
        font = list(size = 11, color = "#e94560"),
        bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#e94560", borderpad = 4
      )
    }

    p <- p %>% layout(
      paper_bgcolor = "transparent", plot_bgcolor = "#f8f9fa",
      xaxis = list(title = "Jumlah Klaster (k)", dtick = 1),
      yaxis = list(title = "Rata-rata Nilai Silhouette",
                   range = c(min(sil_df$Silhouette) * 0.9, max(sil_df$Silhouette) * 1.1)),
      showlegend = FALSE,
      annotations = annotations_list
    ) %>% plotly::config(displayModeBar = FALSE)
    p
  })

  # ---- JUSTIFIKASI PEMILIHAN k=4 (rendered server-side agar bisa akses variabel global) ----
  output$kmeans_justification <- renderUI({
    sil_diff_txt <- ""
    if (best_k_sil != 4) {
      sil_diff_val <- round(
        (elbow_df$Silhouette[best_k_sil] - elbow_df$Silhouette[4]) /
          elbow_df$Silhouette[best_k_sil] * 100, 1
      )
      sil_diff_txt <- paste0(
        " \u2014 selisih hanya ", sil_diff_val, "% dari nilai optimal k=", best_k_sil, "."
      )
    } else {
      sil_diff_txt <- " \u2014 ini merupakan nilai silhouette optimal dari k\u22653."
    }

    tags$div(
      style = "background:#f0f4ff;border-left:4px solid #6C63FF;padding:12px 16px;border-radius:6px;margin-top:10px;",
      tags$p(
        style = "font-size:13px;color:#333;margin:0 0 6px;",
        tags$b("\U0001F4D0 Justifikasi Pemilihan k = 4:")
      ),
      tags$ul(
        style = "font-size:12px;color:#555;margin:0;padding-left:18px;",
        tags$li(HTML(paste0(
          "<b>Metode Elbow (WSS):</b> Kurva WSS membentuk siku yang jelas di <b>k=4</b> \u2014 ",
          "penurunan WSS dari k=3 ke k=4 masih besar (>", wss_pct_drop[3], "%), ",
          "sedangkan dari k=4 ke k=5 hanya ", wss_pct_drop[4], "% ",
          "(melambat signifikan). Ini mengonfirmasi k=4 sebagai titik ",
          "<i>diminishing returns</i> yang sesungguhnya."
        ))),
        tags$li(HTML(paste0(
          "<b>Metode Silhouette:</b> k=2 dikecualikan dari pertimbangan karena hanya menghasilkan ",
          "dua segmen kasar ('baik' vs 'buruk') yang tidak cukup granular untuk rekomendasi wisata. ",
          "Dari k\u22653, nilai silhouette tertinggi ada di <b>k=", best_k_sil,
          "</b> (skor: ", round(elbow_df$Silhouette[best_k_sil], 4),
          "). Skor silhouette k=4 adalah <b>", round(elbow_df$Silhouette[4], 4),
          "</b>", sil_diff_txt,
          " Klaster k=4 tetap kompak dan terpisah dengan baik."
        ))),
        tags$li(HTML(paste0(
          "<b>Interpretabilitas Bisnis:</b> Empat klaster menghasilkan segmentasi yang bermakna \u2014 ",
          "\u2B50 Rating Tertinggi, \U0001F525 Paling Populer, \U0001F48E Destinasi Premium, dan \U0001F33F Wisata Terjangkau ",
          "\u2014 yang mencakup dimensi kualitas, popularitas, dan harga secara seimbang. ",
          "k=3 terlalu kasar (satu dimensi hilang), k=5+ terlalu fragmented untuk rekomendasi yang actionable."
        )))
      )
    )
  })

  # ============================================================
  # RANDOM FOREST
  # ============================================================
  
  output$ib_accuracy <- renderValueBox({
    valueBox(paste0(round(rf_cm$overall["Accuracy"] * 100, 1), "%"), "Akurasi Model", icon = icon("bullseye"), color = "green")
  })
  output$ib_kappa <- renderValueBox({
    valueBox(round(rf_cm$overall["Kappa"], 3), "Kappa", icon = icon("balance-scale"), color = "blue")
  })
  output$ib_ntree <- renderValueBox({
    valueBox(rf_model$ntree, "Jumlah Trees", icon = icon("tree"), color = "purple")
  })
  
  # ---- ANIMASI VALUE BOXES RF (Staggered) ----
  observeEvent(input$sidebar, {
    if (input$sidebar == "randomforest") {
      shinyjs::addClass(id = "anim_akurasi_box", class = "hidden-anim-box")
      shinyjs::addClass(id = "anim_kappa_box",   class = "hidden-anim-box")
      shinyjs::addClass(id = "anim_trees_box",   class = "hidden-anim-box")
      shinyjs::delay(50,   { shinyjs::removeClass(id = "anim_akurasi_box", class = "hidden-anim-box"); startAnim(session, "anim_akurasi_box", "fadeInDown") })
      shinyjs::delay(500,  { shinyjs::removeClass(id = "anim_kappa_box",   class = "hidden-anim-box"); startAnim(session, "anim_kappa_box",   "fadeInDown") })
      shinyjs::delay(1000, { shinyjs::removeClass(id = "anim_trees_box",   class = "hidden-anim-box"); startAnim(session, "anim_trees_box",   "fadeInDown") })
    }
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$btn_predict, input$fil_provinsi, input$fil_kategori), {
    req(input$sidebar == "randomforest")
    startAnim(session, "anim_akurasi_box", "pulse")
  }, ignoreInit = TRUE)
  
  # ---- CONFUSION MATRIX HEATMAP ----
  output$plot_cm <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi; input$fil_kategori
    cm_table <- as.data.frame(rf_cm$table)
    labels   <- levels(rf_result$train$label_rekomendasi)
    plot_ly(x = labels, y = labels,
            z = matrix(0, nrow = length(labels), ncol = length(labels)),
            type = "heatmap",
            colorscale = list(c(0, "#f0f4ff"), c(0.5, "#818cf8"), c(1, "#6C63FF")),
            zmin = 0, zmax = max(cm_table$Freq),
            text = matrix("", nrow = length(labels), ncol = length(labels)),
            texttemplate = "%{text}", hoverinfo = "x+y+z", showscale = FALSE) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             xaxis = list(title = "Aktual", tickfont = list(size = 12)),
             yaxis = list(title = "Prediksi", tickfont = list(size = 12))) %>%
      plotly::config(displayModeBar = FALSE) %>%
      htmlwidgets::onRender(sprintf("
        function(el, x) {
          var gd = el;
          var labels  = %s;
          var n       = labels.length;
          var rawFreq = %s;
          var fullZ = [], fullT = [];
          for (var r = 0; r < n; r++) {
            var rowZ = [], rowT = [];
            for (var c = 0; c < n; c++) { rowZ.push(rawFreq[r * n + c]); rowT.push(String(rawFreq[r * n + c])); }
            fullZ.push(rowZ); fullT.push(rowT);
          }
          function zeroMatrix(n) { return Array.from({length: n}, function() { return new Array(n).fill(0); }); }
          function emptyMatrix(n) { return Array.from({length: n}, function() { return new Array(n).fill(''); }); }
          var delay = 200;
          for (var d = 0; d < n; d++) {
            (function(diag, t) {
              setTimeout(function() {
                var Z = zeroMatrix(n), T = emptyMatrix(n);
                for (var k = 0; k <= diag; k++) { Z[k][k] = fullZ[k][k]; T[k][k] = fullT[k][k]; }
                Plotly.restyle(gd, { z: [Z], text: [T] });
              }, t);
            })(d, delay * (d + 1));
          }
          var phase2Start = delay * (n + 1) + 100;
          for (var step = 0; step < n; step++) {
            (function(row, t) {
              setTimeout(function() {
                var Z = zeroMatrix(n), T = emptyMatrix(n);
                for (var k = 0; k < n; k++) { Z[k][k] = fullZ[k][k]; T[k][k] = fullT[k][k]; }
                for (var r = 0; r <= row; r++) {
                  for (var c = 0; c < n; c++) { Z[r][c] = fullZ[r][c]; T[r][c] = fullT[r][c]; }
                }
                Plotly.restyle(gd, { z: [Z], text: [T] });
              }, t);
            })(step, phase2Start + 280 * (step + 1));
          }
        }
      ",
                                    jsonlite::toJSON(labels),
                                    jsonlite::toJSON(as.vector(t(matrix(cm_table$Freq,
                                                                        nrow = length(labels), ncol = length(labels),
                                                                        dimnames = list(labels, labels))[labels, labels])))
      ))
  })
  
  # ---- FEATURE IMPORTANCE ----
  output$plot_importance <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi; input$fil_kategori
    imp <- as.data.frame(importance(rf_model)) %>%
      rownames_to_column("Feature") %>% arrange(desc(MeanDecreaseGini))
    plot_ly(imp, x = ~MeanDecreaseGini, y = ~reorder(Feature, MeanDecreaseGini),
            type = "bar", orientation = "h",
            marker = list(color = colorRampPalette(c("#43C6AC", "#1a9e89"))(nrow(imp)),
                          line = list(color = "rgba(255,255,255,0.2)", width = 0.5))) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             xaxis = list(title = "Mean Decrease Gini", rangemode = "nonnegative",
                          range = c(0, max(imp$MeanDecreaseGini) * 1.12)),
             yaxis = list(title = ""),
             transition = list(duration = 1000, easing = "exp-out"),
             frame = list(duration = 1000, redraw = FALSE)) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;
          var original_x = gd.data[0].x.slice();
          var zero_x = original_x.map(function() { return 0; });
          Plotly.restyle(gd, {x: [zero_x]}, [0]);
          setTimeout(function() {
            Plotly.animate(gd, { data: [{ x: original_x }] },
              { transition: { duration: 1000, easing: 'exp-out' }, frame: { duration: 1000, redraw: false } });
          }, 200);
        }
      ") %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ---- OOB ERROR RATE PLOT ----
  output$plot_oob <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi; input$fil_kategori
    plot_ly(oob_df, x = ~trees, y = ~oob_error, type = "scatter", mode = "lines",
            line = list(color = "#F7971E", width = 2.5),
            fill = "tozeroy", fillcolor = "rgba(247,151,30,0.15)",
            hovertemplate = "Trees: %{x}<br>OOB Error: %{y:.1f}%<extra></extra>") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "#f8f9fa",
             xaxis = list(title = "Jumlah Trees", color = "#333"),
             yaxis = list(title = "OOB Error Rate (%)", color = "#333", rangemode = "tozero"),
             annotations = list(list(
               x = rf_model$ntree * 0.75, y = tail(oob_df$oob_error, 1) * 1.5,
               text = paste0("OOB Akhir: ", round(tail(oob_df$oob_error, 1), 1), "%"),
               showarrow = FALSE, font = list(size = 13, color = "#F7971E", family = "Arial Black"))),
             margin = list(t = 10, b = 50, l = 60, r = 20)) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;
          var original_y = gd.data[0].y.slice();
          var zero_y = original_y.map(function() { return 0; });
          Plotly.restyle(gd, {y: [zero_y]}, [0]);
          setTimeout(function() {
            Plotly.animate(gd, { data: [{ y: original_y }] },
              { transition: { duration: 1000, easing: 'cubic-in-out' }, frame: { duration: 1000, redraw: false } });
          }, 200);
        }
      ") %>%
      plotly::config(., displayModeBar = FALSE)
  })
  
  # ---- PER-CLASS PRECISION & RECALL PLOT ----
  output$plot_perclass <- renderPlotly({
    req(input$sidebar == "randomforest")
    input$fil_provinsi; input$fil_kategori
    metric_colors <- c(Precision = "#6C63FF", Recall = "#43C6AC")
    plot_ly(per_class_df, x = ~Class, y = ~Value, color = ~Metric, colors = metric_colors,
            type = "bar",
            hovertemplate = "%{x}<br>%{fullData.name}: <b>%{y:.1f}%</b><extra></extra>") %>%
      layout(barmode = "group", paper_bgcolor = "transparent", plot_bgcolor = "#f8f9fa",
             xaxis = list(title = "Label Rekomendasi", color = "#333"),
             yaxis = list(title = "Nilai (%)", color = "#333", range = c(0, 110), rangemode = "nonnegative"),
             legend = list(orientation = "h", x = 0.25, y = 1.12),
             margin = list(t = 40, b = 50, l = 60, r = 20)) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el;
          var n_traces = gd.data.length;
          var original_y = [], zero_y = [], traces_idx = [];
          for (var i = 0; i < n_traces; i++) {
            var y_arr = gd.data[i].y.slice();
            original_y.push({ y: y_arr });
            zero_y.push(y_arr.map(function() { return 0; }));
            traces_idx.push(i);
          }
          Plotly.restyle(gd, {y: zero_y}, traces_idx);
          setTimeout(function() {
            Plotly.animate(gd, { data: original_y, traces: traces_idx },
              { transition: { duration: 1000, easing: 'elastic-in-out' }, frame: { duration: 1000, redraw: false } });
          }, 200);
        }
      ") %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ---- SIMULATOR PREDIKSI ----
  observeEvent(input$sim_harga, {
    h <- input$sim_harga
    kat <- if (is.na(h) || h <= 0) "Gratis" else if (h >= 1 && h <= 9999) "Murah" else if (h >= 10000 && h <= 19999) "Sedang" else "Mahal"
    updateTextInput(session, "sim_kat_harga", value = kat)
  })
  
  output$pred_result <- renderUI({ tags$span(style = "color:#888;", "\U0001F446 Isi form lalu klik Prediksi") })
  
  observeEvent(input$btn_predict, {
    if (is.na(input$sim_rating) || is.na(input$sim_review) || is.na(input$sim_harga)) {
      startAnim(session, "sim_input_panel", "shake")
      showNotification("Semua kolom input harus diisi!", type = "error")
      return()
    }
    shinyjs::disable("btn_predict")
    startAnim(session, "btn_predict", "pulse")
    output$pred_result <- renderUI({ HTML("") })
    Sys.sleep(0.8)
    new_data <- data.frame(
      rating = input$sim_rating,
      jumlah_riview = input$sim_review, harga_num = input$sim_harga,
      kategori_harga = factor(input$sim_kat_harga, levels = levels(rf_result$train$kategori_harga))
    )
    pred   <- predict(rf_model, new_data)
    colors <- c(Terbaik = "#27ae60", Baik = "#3498db", Sedang = "#f39c12", Buruk = "#e74c3c")
    icons  <- c(Terbaik = "\u2B50", Baik = "\U0001F44D", Sedang = "\U0001F44C", Buruk = "\u26A0\uFE0F")
    descs  <- c(
      Terbaik = "Destinasi ini diprediksi TERBAIK untuk dikunjungi!",
      Baik    = "Destinasi ini diprediksi BAIK untuk dikunjungi.",
      Sedang  = "Destinasi ini diprediksi SEDANG \u2014 cukup layak dikunjungi.",
      Buruk   = "Destinasi ini diprediksi BURUK \u2014 pertimbangkan alternatif lain."
    )
    p_char <- as.character(pred)
    col    <- colors[p_char]; ico <- icons[p_char]; dsc <- descs[p_char]
    if (p_char == "Terbaik") sendSweetAlert(session, title = "Luar Biasa! \U0001F389\u2728", text = "Destinasi Terbaik Ditemukan! Sangat direkomendasikan.", type = "success")
    else if (p_char == "Baik")   sendSweetAlert(session, title = "Bagus! \U0001F44D", text = "Destinasi ini diprediksi BAIK untuk dikunjungi.", type = "info")
    else if (p_char == "Sedang") sendSweetAlert(session, title = "Cukup Menarik \U0001F44C", text = "Destinasi ini masuk kategori SEDANG, bisa jadi alternatif pilihan.", type = "info")
    else if (p_char == "Buruk")  sendSweetAlert(session, title = "Pertimbangkan Lagi \u26A0\uFE0F", text = "Destinasi ini diprediksi BURUK, sebaiknya cari opsi lain.", type = "warning")
    output$pred_result <- renderUI({
      tags$div(
        tags$div(id = "sim_res_main", style = paste0("color:", col, ";"), paste0(ico, " ", p_char)),
        tags$div(id = "sim_res_desc", class = "hidden-anim-box", style = "font-size:14px;color:#666;margin-top:5px;", dsc)
      )
    })
    shinyjs::delay(300, {
      shinyjs::removeClass(id = "sim_res_desc", class = "hidden-anim-box")
      startAnim(session, "sim_res_desc", "fadeInUp")
      shinyjs::enable("btn_predict")
    })
  })
  
  # ============================================================
  # DATASET — Tabel & Download Interaktif
  # ============================================================

  # Helper: terapkan filter lanjutan & pilihan kolom untuk unduhan
  build_dl_data <- reactive({
    d <- df_final

    # Filter lanjutan jika kolom terkait dicentang & ada nilai filter yang dipilih
    if (isTRUE(input$dl_col_kategori) && !is.null(input$dl_opt_kategori)) {
      d <- d %>% filter(kategori %in% input$dl_opt_kategori)
    }
    if (isTRUE(input$dl_col_provinsi) && !is.null(input$dl_opt_provinsi)) {
      d <- d %>% filter(provinsi %in% input$dl_opt_provinsi)
    }
    if (isTRUE(input$dl_col_klaster) && !is.null(input$dl_opt_klaster)) {
      d <- d %>% filter(cluster %in% input$dl_opt_klaster)
    }
    if (isTRUE(input$dl_col_label) && !is.null(input$dl_opt_label)) {
      d <- d %>% filter(label_rekomendasi %in% input$dl_opt_label)
    }

    # Peta nama kolom asli → label tampilan
    col_map <- list(
      nama      = c("nama_wisata",      "Nama Wisata"),
      kategori  = c("kategori",         "Kategori"),
      kabupaten = c("kabupaten",        "Kabupaten"),
      provinsi  = c("provinsi",         "Provinsi"),
      rating    = c("rating",           "Rating"),
      review    = c("jumlah_riview",    "Jumlah Review"),
      harga     = c("harga_num",        "Harga (Rp)"),
      katharga  = c("kategori_harga",   "Kategori Harga"),
      label     = c("label_rekomendasi","Label Rekomendasi"),
      klaster   = c("cluster",          "Klaster"),
      alamat    = c("alamat",           "Alamat"),
      deskripsi = c("deskripsi_wisata", "Deskripsi Wisata")
    )

    # Kumpulkan kolom yang dicentang
    selected_cols  <- c()
    selected_names <- c()
    for (key in names(col_map)) {
      input_id <- paste0("dl_col_", key)
      val <- input[[input_id]]
      if (!is.null(val) && isTRUE(val)) {
        raw_col <- col_map[[key]][1]
        lbl_col <- col_map[[key]][2]
        if (raw_col %in% colnames(d)) {
          selected_cols  <- c(selected_cols,  raw_col)
          selected_names <- c(selected_names, lbl_col)
        }
      }
    }

    if (length(selected_cols) == 0 || nrow(d) == 0) {
      return(data.frame(Info = "Tidak ada data yang sesuai filter atau kolom tidak dipilih."))
    }

    out <- d %>% select(all_of(selected_cols))
    colnames(out) <- selected_names
    out
  })

  # Tampilkan tabel (tanpa tombol DT bawaan)
  output$tbl_full <- renderDT({
    d <- df_filtered() %>%
      select(Nama = nama_wisata, Kategori = kategori, Kabupaten = kabupaten,
             Provinsi = provinsi, Rating = rating, Review = jumlah_riview,
             `Harga (Rp)` = harga_num, `Kat. Harga` = kategori_harga,
             `Label Rekomendasi` = label_rekomendasi, Klaster = cluster)
    datatable(d,
              options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
              rownames = FALSE, class = "compact stripe hover") %>%
      formatCurrency("Harga (Rp)", currency = "Rp ", digits = 0, mark = ",")
  })

  # Download CSV
  output$dl_csv <- downloadHandler(
    filename = function() { paste0("wisata_custom_", format(Sys.Date(), "%Y%m%d"), ".csv") },
    content = function(file) {
      write.csv(build_dl_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # Download Excel
  output$dl_excel <- downloadHandler(
    filename = function() { paste0("wisata_custom_", format(Sys.Date(), "%Y%m%d"), ".xlsx") },
    content = function(file) {
      library(openxlsx)
      wb <- createWorkbook()
      addWorksheet(wb, "Data Wisata")
      writeData(wb, "Data Wisata", build_dl_data())
      hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#0A192F",
                        halign = "center", textDecoration = "Bold", border = "Bottom")
      addStyle(wb, "Data Wisata", hs, rows = 1, cols = 1:ncol(build_dl_data()), gridExpand = TRUE)
      setColWidths(wb, "Data Wisata", cols = 1:ncol(build_dl_data()), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Toggle buka modal
  observeEvent(input$btn_dl_modal, {
    session$sendCustomMessage("toggleDlModal", list(show = TRUE))
  })

  # Toggle tutup modal
  observeEvent(input$btn_dl_close, {
    session$sendCustomMessage("toggleDlModal", list(show = FALSE))
  })

  # Pilih Semua Kolom (mengupdate child checkboxes)
  observeEvent(input$dl_check_all, {
    # Hanya lakukan update jika checkbox ditekan oleh user (bukan dari sync JS)
    # Kita andalkan script JS yang mengubah `dl_check_all`
    # Namun update dari JS sudah mengubah checked state di DOM.
    # Jika Shiny trigger event ini, kita update ke semua anaknya.
    # Hindari infinite loop dengan menggunakan ignoreInit.
    cols <- c("nama","kategori","kabupaten","provinsi","rating",
              "review","harga","katharga","label","klaster","alamat","deskripsi")
    val <- isTRUE(input$dl_check_all)
    for (col in cols) updateCheckboxInput(session, paste0("dl_col_", col), value = val)
  }, ignoreInit = TRUE)

  
  # ============================================================
  # SYLVA CHATBOT – Delegasi Server
  # ============================================================
  sylva_chatbot_server(input, output, session, df_chatbot, cluster_summary, rf_cm)
}

# shinyApp() TIDAK dipanggil di sini.
# Struktur multi-file (global.R + ui.R + server.R) dikelola
# otomatis oleh Shiny — jalankan via shiny::runApp() atau
# klik Run App saat membuka app.R / server.R di RStudio.
