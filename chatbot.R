# ============================================================
# SYLVA CHATBOT MODULE
# ============================================================

# 1. Dependensi CSS & JS
sylva_chatbot_dependencies <- function() {
  tagList(
    tags$style(HTML("
      /* ====== SYLVA CHATBOT ====== */
      #sylva-fab {
        position: fixed; bottom: 28px; right: 28px; z-index: 9999;
        width: 60px; height: 60px; border-radius: 50%;
        background: linear-gradient(135deg, #20C997, #0A192F);
        border: none; cursor: pointer;
        box-shadow: 0 0 0 0 rgba(32,201,151,0.5);
        animation: sylva-pulse 2.5s infinite;
        display: flex; align-items: center; justify-content: center;
        font-size: 26px; transition: transform 0.2s ease;
      }
      #sylva-fab:hover { transform: scale(1.12); }
      @keyframes sylva-pulse {
        0%   { box-shadow: 0 0 0 0 rgba(32,201,151,0.55); }
        70%  { box-shadow: 0 0 0 14px rgba(32,201,151,0); }
        100% { box-shadow: 0 0 0 0 rgba(32,201,151,0); }
      }
      #sylva-panel {
        position: fixed; bottom: 100px; right: 28px; z-index: 9998;
        width: 390px; height: 560px;
        background: rgba(10,25,47,0.97);
        backdrop-filter: blur(18px);
        border: 1px solid rgba(32,201,151,0.35);
        border-radius: 18px;
        box-shadow: 0 24px 60px rgba(0,0,0,0.55);
        display: flex; flex-direction: column;
        transform: translateY(30px) scale(0.96); opacity: 0;
        pointer-events: none;
        transition: all 0.32s cubic-bezier(0.34,1.56,0.64,1);
      }
      #sylva-panel.open {
        transform: translateY(0) scale(1); opacity: 1;
        pointer-events: all;
      }
      #sylva-header {
        padding: 14px 18px; display: flex; align-items: center;
        border-bottom: 1px solid rgba(32,201,151,0.2);
        border-radius: 18px 18px 0 0;
        background: linear-gradient(90deg,rgba(32,201,151,0.12),rgba(10,25,47,0));
      }
      #sylva-header .sylva-avatar {
        width: 40px; height: 40px; border-radius: 50%;
        background: linear-gradient(135deg,#20C997,#0A192F);
        display: flex; align-items: center; justify-content: center;
        font-size: 20px; margin-right: 12px; flex-shrink: 0;
      }
      #sylva-header .sylva-title { flex: 1; }
      #sylva-header .sylva-title h4 {
        margin:0; color:#20C997; font-size:15px; font-weight:800;
        letter-spacing:1px;
      }
      #sylva-header .sylva-title p {
        margin:0; color:#64748b; font-size:11px;
      }
      #sylva-header .sylva-status {
        width:9px; height:9px; border-radius:50%;
        background:#20C997; flex-shrink:0;
        box-shadow: 0 0 6px #20C997;
        animation: blink-status 2s infinite;
      }
      @keyframes blink-status {
        0%,100%{ opacity:1; } 50%{ opacity:0.3; }
      }
      #sylva-close {
        background: none; border: none; color: #64748b;
        font-size: 18px; cursor: pointer; padding: 0 0 0 10px;
        line-height:1; transition: color 0.2s;
      }
      #sylva-close:hover { color: #ff6584; }
      #sylva-messages {
        flex: 1; overflow-y: auto; padding: 14px 14px 6px;
        display: flex; flex-direction: column; gap: 10px;
        scroll-behavior: smooth;
      }
      #sylva-messages::-webkit-scrollbar { width: 4px; }
      #sylva-messages::-webkit-scrollbar-track { background: transparent; }
      #sylva-messages::-webkit-scrollbar-thumb { background: rgba(32,201,151,0.3); border-radius:4px; }
      .sylva-bubble {
        max-width: 85%; padding: 10px 14px;
        border-radius: 16px; font-size: 13px; line-height: 1.6;
        animation: bubble-in 0.3s cubic-bezier(0.34,1.56,0.64,1);
        word-break: break-word; white-space: pre-wrap;
      }
      .sylva-bubble.bot strong { color: #20C997; font-weight: 600; }
      @keyframes bubble-in {
        from { opacity:0; transform:translateY(10px) scale(0.96); }
        to   { opacity:1; transform:translateY(0) scale(1); }
      }
      .sylva-msg-row {
        display: flex; width: 100%;
        margin-bottom: 10px;
      }
      .sylva-msg-row.user-row {
        justify-content: flex-end;
      }
      .sylva-msg-row.bot-row {
        justify-content: flex-start;
      }
      .sylva-bubble.bot {
        background: rgba(32,201,151,0.12);
        border: 1px solid rgba(32,201,151,0.25);
        color: #e2e8f0;
        border-radius: 16px 16px 16px 4px;
      }
      .sylva-bubble.user {
        background: linear-gradient(135deg,#20C997,#0db27a);
        color: #0A192F;
        border-radius: 16px 16px 4px 16px; font-weight: 500;
      }
      .sylva-bubble.typing {
        background: rgba(32,201,151,0.08);
        border: 1px solid rgba(32,201,151,0.15);
        color: #64748b;
        border-radius: 16px 16px 16px 4px;
      }
      .sylva-dots span {
        display:inline-block; width:7px; height:7px;
        border-radius:50%; background:#20C997; margin:0 2px;
        animation: dot-bounce 1.2s infinite;
      }
      .sylva-dots span:nth-child(2){ animation-delay:0.2s; }
      .sylva-dots span:nth-child(3){ animation-delay:0.4s; }
      @keyframes dot-bounce {
        0%,80%,100%{ transform:translateY(0); }
        40%{ transform:translateY(-8px); }
      }
      #sylva-input-area {
        padding: 10px 14px 14px;
        border-top: 1px solid rgba(32,201,151,0.15);
        display: flex; gap: 8px; align-items: flex-end;
      }
      #sylva_input {
        flex:1; background: rgba(255,255,255,0.06);
        border: 1px solid rgba(32,201,151,0.25);
        border-radius: 12px; padding: 9px 13px;
        color: #e2e8f0; font-size: 13px; resize: none;
        max-height: 90px; outline: none; font-family: inherit;
        transition: border-color 0.2s;
      }
      #sylva_input:focus { border-color: #20C997; }
      #sylva_input::placeholder { color: #475569; }
      #sylva-send-btn {
        width: 40px; height: 40px; border-radius: 50%;
        background: linear-gradient(135deg,#20C997,#0db27a);
        border: none; cursor: pointer;
        display:flex; align-items:center; justify-content:center;
        font-size: 16px; flex-shrink:0;
        transition: transform 0.2s, opacity 0.2s;
        box-shadow: 0 4px 12px rgba(32,201,151,0.35);
      }
      #sylva-send-btn:hover { transform: scale(1.1); }
      #sylva-send-btn:disabled { opacity:0.4; cursor:not-allowed; transform:none; }
      .sylva-welcome {
        text-align:center; padding: 10px 6px;
        color: #475569; font-size: 12px;
      }
      .sylva-welcome .sylva-wave {
        font-size:32px; display:block; margin-bottom:6px;
      }
      /* ====== END SYLVA ====== */
    ")),
    tags$script(HTML("
      // ====== SYLVA CHATBOT JS ======
      function sylvaToggle() {
        var p = document.getElementById('sylva-panel');
        p.classList.toggle('open');
        if (p.classList.contains('open')) {
          setTimeout(function(){ sylvaScrollBottom(); }, 50);
        }
      }
      function sylvaScrollBottom() {
        var m = document.getElementById('sylva-messages');
        if (m) m.scrollTop = m.scrollHeight;
      }
      function sylvaSend() {
        var inp = document.getElementById('sylva_input');
        var val = inp ? inp.value.trim() : '';
        if (!val) return;
        Shiny.setInputValue('sylva_user_msg', val, {priority: 'event'});
        inp.value = '';
        inp.style.height = 'auto';
      }
      document.addEventListener('keydown', function(e) {
        var inp = document.getElementById('sylva_input');
        if (inp && document.activeElement === inp) {
          if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault(); sylvaSend();
          }
        }
      });
      // Auto-grow textarea
      document.addEventListener('input', function(e) {
        if (e.target && e.target.id === 'sylva_input') {
          e.target.style.height = 'auto';
          e.target.style.height = Math.min(e.target.scrollHeight, 90) + 'px';
        }
      });
      // Shiny message: scroll to bottom after new message
      Shiny.addCustomMessageHandler('sylvaScroll', function(x) {
        setTimeout(sylvaScrollBottom, 80);
      });
      Shiny.addCustomMessageHandler('sylvaDisableBtn', function(x) {
        var b = document.getElementById('sylva-send-btn');
        if (b) b.disabled = x.disabled;
      });
      // Tampilkan pesan user & loading SEGERA via DOM (sebelum Shiny flush)
      function sylvaMd(txt) {
        return txt
          .replace(/\\*\\*(.+?)\\*\\*/g, '<strong>$1</strong>')
          .replace(/\\*(.+?)\\*/g, '<em>$1</em>')
          .replace(/^[-•]\\s+/gm, '\u2022 ');
      }
      Shiny.addCustomMessageHandler('sylvaAddUserBubble', function(d) {
        var m = document.getElementById('sylva-messages');
        if (!m) return;
        var old = m.querySelector('.sylva-temp-user');
        if (old) old.remove();
        var row = document.createElement('div');
        row.className = 'sylva-msg-row user-row sylva-temp-user';
        var bbl = document.createElement('div');
        bbl.className = 'sylva-bubble user'; bbl.textContent = d.text;
        row.appendChild(bbl); m.appendChild(row);
        m.scrollTop = m.scrollHeight;
      });
      Shiny.addCustomMessageHandler('sylvaShowLoading', function(d) {
        var m = document.getElementById('sylva-messages');
        if (!m) return;
        if (m.querySelector('#sylva-temp-typing')) return;
        var row = document.createElement('div');
        row.id = 'sylva-temp-typing';
        row.className = 'sylva-msg-row bot-row';
        row.innerHTML = '<div class=\\'sylva-bubble typing\\'><span class=\\'sylva-dots\\'><span></span><span></span><span></span></span></div>';
        m.appendChild(row); m.scrollTop = m.scrollHeight;
      });
      Shiny.addCustomMessageHandler('sylvaHideLoading', function(d) {
        var el = document.getElementById('sylva-temp-typing');
        if (el) el.remove();
        var t = document.querySelector('.sylva-temp-user');
        if (t) t.remove();
      });
      // Render markdown di bot bubbles setelah Shiny update DOM
      function sylvaRenderMd() {
        document.querySelectorAll('.sylva-bubble.bot').forEach(function(el) {
          if (el.dataset.mdDone) return;
          el.dataset.mdDone = '1';
          el.innerHTML = sylvaMd(el.textContent
            .replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'));
        });
      }
      Shiny.addCustomMessageHandler('sylvaRenderMd', function(d) {
        setTimeout(sylvaRenderMd, 120);
      });
      // ====== END SYLVA JS ======
    "))
  )
}

# 2. Struktur HTML Chatbot
sylva_chatbot_ui <- function() {
  tagList(
    tags$div(
      id = "sylva-fab",
      onclick = "sylvaToggle()",
      "🤖"
    ),
    tags$div(
      id = "sylva-panel",
      # Header
      tags$div(
        id = "sylva-header",
        tags$div(class = "sylva-avatar", "🌿"),
        tags$div(
          class = "sylva-title",
          tags$h4("SYLVA"),
          tags$p("Sulawesi Intelligent Landscape & Visitor Analyst")
        ),
        tags$div(class = "sylva-status"),
        tags$button(id = "sylva-close", onclick = "sylvaToggle()", "✕")
      ),
      # Messages area
      tags$div(
        id = "sylva-messages",
        tags$div(
          class = "sylva-welcome",
          tags$span(class = "sylva-wave", "🌺"),
          tags$strong(style = "color:#20C997;", "Halo! Saya SYLVA"),
          tags$br(),
          "Asisten analitik pariwisata Sulawesi Anda.",
          tags$br(),
          "Tanyakan apa saja tentang destinasi, klaster, atau prediksi!"
        ),
        uiOutput("sylva_messages_ui")
      ),
      # Input area
      tags$div(
        id = "sylva-input-area",
        tags$textarea(
          id = "sylva_input",
          placeholder = "Tanya SYLVA... (Enter untuk kirim)",
          rows = "1"
        ),
        tags$button(
          id = "sylva-send-btn",
          onclick = "sylvaSend()",
          "➤"
        )
      )
    )
  )
}

# 3. Server Logic Chatbot
sylva_chatbot_server <- function(input, output, session, df_final, cluster_summary, rf_cm) {
  OPENAI_KEY <- Sys.getenv("OPENAI_API_KEY")

  # ---------------------------------------------------------------
  # STATE: Memori Ganda (Dual Memory) — mencegah Context Bleed
  # ---------------------------------------------------------------
  chat_state <- reactiveValues(
    history       = list(),
    loading       = FALSE,
    mem_kabupaten = "",   # kabupaten terakhir yang dibahas
    mem_destinasi = "",   # destinasi spesifik terakhir yang dibahas
    user_name     = ""    # nama user jika disebutkan di awal percakapan
  )

  # ---------------------------------------------------------------
  # TAHAP 1-3: ENTITY DETECTOR (Detektif Konteks)
  # Urutan prioritas:
  #   1. Nama destinasi spesifik (Global Search) → update KEDUA memori
  #   2. Nama kabupaten baru → update kabupaten, HAPUS memori destinasi
  #   3. Obrolan lanjutan → gunakan memori yang ada
  # Mengembalikan list(type, kab_filter, dest_row)
  # ---------------------------------------------------------------
  detect_entity <- function(user_text, d_base) {
    txt <- tolower(trimws(user_text))

    # ---- TAHAP 1: Cari nama destinasi secara global ----
    # Pecah pesan jadi kata-kata kunci (min 4 huruf, hapus stopword umum)
    sw <- c("berapa","jumlah","daftar","sebutkan","berikan","tampilkan",
            "wisata","destinasi","tempat","kabupaten","kota","provinsi",
            "yang","untuk","dari","pada","dalam","dengan","populer",
            "terbaik","bagus","murah","mahal","sedang","gratis","rekomendasi",
            "saja","kalau","bisa","tolong","kasih","mana","atas","bawah",
            "bagaimana","ceritakan","jelaskan","gimana","adalah","tentang",
            "dimana","lokasinya","harganya","tiket","masuk","berapa","rating")
    words <- unique(unlist(strsplit(txt, "\\W+")))
    words <- words[nchar(words) >= 4]
    words <- setdiff(words, sw)

    # Hitung skor kecocokan tiap destinasi dengan pesan user (hanya berdasarkan pesan TERBARU)
    if (length(words) > 0) {
      dest_haystack <- tolower(d_base$nama_wisata)
      scores <- sapply(seq_len(nrow(d_base)), function(i) {
        sum(sapply(words, function(w) grepl(w, dest_haystack[i], fixed = TRUE)))
      })
      best_idx   <- which.max(scores)
      best_score <- max(scores)

      # Threshold: minimal 2 kata cocok, ATAU 1 kata cocok dari nama yang sangat unik (>=8 huruf)
      long_words  <- words[nchar(words) >= 7]
      long_match  <- if (length(long_words) > 0) {
        any(sapply(long_words, function(w) grepl(w, dest_haystack[best_idx], fixed = TRUE)))
      } else FALSE

      if (best_score >= 2 || (best_score >= 1 && long_match)) {
        return(list(
          type     = "destinasi",
          dest_row = d_base[best_idx, ],
          kab_filter = d_base[best_idx, "kabupaten"]
        ))
      }
    }

    # ---- TAHAP 2: Cari nama kabupaten/provinsi baru ----
    kab_list <- unique(d_base$kabupaten)
    kab_hit  <- kab_list[sapply(tolower(kab_list), function(k) grepl(k, txt, fixed = TRUE))]
    if (length(kab_hit) > 0) {
      return(list(
        type       = "kabupaten",
        dest_row   = NULL,
        kab_filter = kab_hit[1]
      ))
    }

    prov_list <- unique(d_base$provinsi)
    prov_hit  <- prov_list[sapply(tolower(prov_list), function(p) grepl(p, txt, fixed = TRUE))]
    if (length(prov_hit) > 0) {
      return(list(
        type       = "provinsi",
        dest_row   = NULL,
        kab_filter = NULL,
        prov_filter = prov_hit[1]
      ))
    }

    # ---- TAHAP 3a: List request (minta daftar/data) menggunakan memori ----
    # Jika user minta list/daftar, kita TETAP harus kirim data (bukan prompt ringan)
    list_signals <- c("apa aja", "ada apa", "apa saja", "daftar", "sebutkan",
                      "tampilkan", "rekomen", "kasih tau", "list", "lihat",
                      "wisatanya", "tempatnya", "destinasinya", "mana saja",
                      "apa yang", "yang ada", "yang menarik", "tunjukkan",
                      # sinyal terkait alamat & deskripsi
                      "alamat", "lokasinya", "dimana", "di mana", "lokasi lengkap",
                      "cara ke", "rute", "jalan ke", "akses", "deskripsi",
                      "ceritakan", "jelaskan", "info tentang", "keterangan")
    is_list_req <- any(sapply(list_signals, function(s) grepl(s, txt, fixed = TRUE)))
    if (is_list_req) {
      return(list(type = "list_followup", dest_row = NULL, kab_filter = NULL))
    }

    # ---- TAHAP 3b: Obrolan lanjutan murni (chitchat) ----
    return(list(type = "followup", dest_row = NULL, kab_filter = NULL))
  }

  # ---------------------------------------------------------------
  # PROMPT BUILDER: Mode FULL DATA (dengan fokus entitas yang terdeteksi)
  # ---------------------------------------------------------------
  build_data_prompt <- function(chat_history, d_base, entity) {
    message("[SYLVA][data] Entity type: ", entity$type)

    # --- Susun katalog berdasarkan entitas yang terdeteksi ---
    d_labels <- cluster_summary$label[match(d_base$cluster, cluster_summary$cluster)]

    if (entity$type == "destinasi" && !is.null(entity$dest_row)) {
      # Tempatkan destinasi yang ditemukan di baris PERTAMA, diikuti wisata serupa (kab/kategori sama)
      target_kab <- entity$dest_row$kabupaten
      target_kat <- entity$dest_row$kategori
      same_kab   <- d_base[d_base$kabupaten == target_kab & d_base$nama_wisata != entity$dest_row$nama_wisata, ]
      same_kat   <- d_base[d_base$kategori  == target_kat & d_base$kabupaten != target_kab, ]
      d_focus    <- rbind(entity$dest_row, head(same_kab, 20), head(same_kat, 15))
      d_focus    <- d_focus[!duplicated(d_focus$nama_wisata), ]
      d_ord      <- head(d_focus, 40)

    } else if (entity$type %in% c("kabupaten", "list_followup") && !is.null(entity$kab_filter)) {
      # HANYA tampilkan destinasi dari kabupaten ini — TANPA kontaminasi kabupaten lain
      d_kab <- d_base[tolower(d_base$kabupaten) == tolower(entity$kab_filter), ]
      d_ord <- d_kab  # strict: no cross-kabupaten entries

    } else {
      # Followup: gunakan katalog umum terurut popularitas
      d_ord <- d_base[order(-d_base$rating * log10(d_base$jumlah_riview + 10)), ]
      d_ord <- head(d_ord, 60)
    }

    d_ord        <- d_ord[!is.na(d_ord$nama_wisata), ]
    d_ord_labels <- cluster_summary$label[match(d_ord$cluster, cluster_summary$cluster)]
    harga_lbl    <- ifelse(!is.na(d_ord$harga_num) & d_ord$harga_num > 0,
                           paste0("Rp", round(d_ord$harga_num / 1000), "rb"), "Gratis")

    # Ambil alamat & deskripsi dengan fallback aman
    alamat_col <- if ("alamat" %in% colnames(d_ord))
      ifelse(is.na(d_ord$alamat) | d_ord$alamat == "", "Alamat belum tersedia.", d_ord$alamat)
    else rep("Alamat belum tersedia.", nrow(d_ord))

    desk_col <- if ("deskripsi_wisata" %in% colnames(d_ord))
      ifelse(is.na(d_ord$deskripsi_wisata) | d_ord$deskripsi_wisata == "", "Deskripsi belum tersedia.", d_ord$deskripsi_wisata)
    else rep("Deskripsi belum tersedia.", nrow(d_ord))

    catalog <- paste0(
      d_ord$nama_wisata, "|", d_ord$kabupaten, "|", d_ord$provinsi, "|",
      d_ord$kategori, "|", d_ord_labels, "|", d_ord$label_rekomendasi,
      "|R:", d_ord$rating, "|Rev:", d_ord$jumlah_riview, "|", harga_lbl,
      "|ALAMAT:", alamat_col,
      "|DESC:", substr(desk_col, 1, 150),  # batasi 150 karakter agar hemat token
      collapse = "\n"
    )

    # --- Pre-computed stats dari SELURUH dataset ---
    stat_kab   <- d_base %>% count(kabupaten, name = "n") %>% arrange(desc(n))
    stat_prov  <- d_base %>% count(provinsi,  name = "n") %>% arrange(desc(n))
    stat_kat   <- d_base %>% count(kategori,  name = "n") %>% arrange(desc(n))
    stat_label <- d_base %>% count(label_rekomendasi, name = "n") %>% arrange(desc(n))

    kab_txt  <- paste0("- ", stat_kab$kabupaten, ": ", stat_kab$n,   " wisata", collapse = "\n")
    prov_txt <- paste0("- ", stat_prov$provinsi,  ": ", stat_prov$n,  " wisata", collapse = "\n")
    kat_txt  <- paste0("- ", stat_kat$kategori,   ": ", stat_kat$n,   " wisata", collapse = "\n")
    lbl_txt  <- paste0("- ", stat_label$label_rekomendasi, ": ", stat_label$n, " wisata", collapse = "\n")

    rf_acc       <- round(rf_cm$overall["Accuracy"] * 100, 1)
    klaster_info <- paste0(
      "K", cluster_summary$cluster, "=", cluster_summary$label,
      "(n:", cluster_summary$n, ",avgR:", cluster_summary$avg_rating,
      ",avgRev:", cluster_summary$avg_reviews, ")",
      collapse = " | "
    )

    # --- Pinned Facts: angka pasti untuk entitas yang ditanya ---
    latest_msg   <- tolower(tail(sapply(chat_history, function(x) x$content), 1))
    kab_hits     <- stat_kab$kabupaten[sapply(tolower(stat_kab$kabupaten),
                      function(k) grepl(k, latest_msg, fixed = TRUE))]
    prov_hits    <- stat_prov$provinsi[sapply(tolower(stat_prov$provinsi),
                      function(p) grepl(p, latest_msg, fixed = TRUE))]
    pinned_lines <- c()
    for (k in kab_hits)  pinned_lines <- c(pinned_lines, paste0("!! FAKTA MUTLAK: ", k, " = TEPAT ", stat_kab$n[stat_kab$kabupaten == k], " wisata !!"))
    for (p in prov_hits) pinned_lines <- c(pinned_lines, paste0("!! FAKTA MUTLAK: ", p, " = TEPAT ", stat_prov$n[stat_prov$provinsi  == p], " wisata !!"))

    # Jika destinasi terdeteksi, pin lokasi aslinya agar tidak salah kabupaten
    if (entity$type == "destinasi" && !is.null(entity$dest_row)) {
      pinned_lines <- c(pinned_lines,
        paste0("!! FAKTA LOKASI: ", entity$dest_row$nama_wisata,
               " berada di ", entity$dest_row$kabupaten, ", ",
               entity$dest_row$provinsi, " — BUKAN di kabupaten lain !!"))
    }

    pinned_block <- if (length(pinned_lines) > 0) {
      paste0("=== FAKTA WAJIB (PRIORITAS TERTINGGI, TIDAK BOLEH DIABAIKAN) ===\n",
             paste(pinned_lines, collapse = "\n"), "\n",
             "==========================================================\n\n")
    } else ""

    sys <- paste0(
      pinned_block,
      "Kamu adalah SYLVA - asisten analitik pariwisata Sulawesi yang cerdas, hangat, dan asyik!\n",
      "Kamu punya akses ke database & hasil analisis ML pariwisata Sulawesi.\n\n",
      "### STATISTIK DATASET RESMI (sumber kebenaran untuk angka jumlah) ###\n",
      "PERINGATAN: Katalog di bawah adalah SAMPEL. JANGAN hitung jumlah dari sana!\n",
      "Total destinasi: ", nrow(d_base), "\n\n",
      "Per Provinsi:\n", prov_txt, "\n\n",
      "Per Kabupaten/Kota:\n", kab_txt, "\n\n",
      "Per Kategori:\n", kat_txt, "\n\n",
      "Label RF:\n", lbl_txt, "\n\n",
      "### HASIL ANALISIS ML ###\n",
      "Random Forest: akurasi ", rf_acc, "% (4 kelas: Terbaik/Baik/Sedang/Buruk)\n",
      "K-Means k=4: ", klaster_info, "\n\n",
      "### KATALOG DESTINASI (Nama|Kab|Prov|Kat|Klaster|LabelRF|Rating|Review|Harga|ALAMAT|DESC) ###\n",
      if (!is.null(entity$kab_filter) && nzchar(entity$kab_filter))
        paste0("DATA AKTIF: Katalog berikut berisi destinasi di ", entity$kab_filter,
               " sesuai konteks percakapan. WAJIB gunakan data ini untuk menjawab.\n",
               if (!is.null(entity$context_note) && nzchar(entity$context_note))
                 paste0("Konteks: ", entity$context_note, "\n") else "")
      else
        "Gunakan katalog di bawah untuk menjawab. Angka jumlah dari STATISTIK RESMI.\n",
      catalog, "\n\n",
      "### CARA KERJA SYLVA ###\n",
      "1. Pertanyaan JUMLAH wisata → pakai angka STATISTIK RESMI / FAKTA WAJIB.\n",
      "2. Pertanyaan DAFTAR wisata di kabupaten X → HANYA sebut destinasi dari kolom Kab = X.\n",
      "3. Pertanyaan ALAMAT / LOKASI / CARA KE SANA → gunakan kolom ALAMAT dari katalog.\n",
      "4. Pertanyaan DESKRIPSI / INFO DETAIL → gunakan kolom DESC dari katalog.\n",
      "5. Destinasi tidak ada di katalog → akui tidak ada, jangan mengarang.\n",
      "6. Bahasa santai + emoji secukupnya. Bullet list jika banyak item.\n",
      "7. Akhiri dengan pertanyaan balik agar obrolan mengalir.\n",
      "8. Di luar pariwisata Sulawesi: tolak sopan + sedikit humor.\n"
    )
    message("[SYLVA][data] prompt: ", nchar(sys), " chars (~", round(nchar(sys) / 4), " tokens)")
    sys
  }

  # ---------------------------------------------------------------
  # PROMPT BUILDER: Mode FOLLOWUP (ringan, hemat token)
  # ---------------------------------------------------------------
  build_followup_prompt <- function(mem_destinasi, mem_kabupaten, user_name = "") {
    # Sapa user dengan namanya jika diketahui
    sapa <- if (nzchar(user_name)) paste0("Nama user: ", user_name, ". ") else ""

    ctx_note <- if (nzchar(mem_destinasi)) {
      paste0(sapa, "Konteks aktif: destinasi '", mem_destinasi, "'",
             if (nzchar(mem_kabupaten)) paste0(" di '", mem_kabupaten, "'") else "",
             ".\n")
    } else if (nzchar(mem_kabupaten)) {
      paste0(sapa, "Konteks aktif: Kabupaten/Kota '", mem_kabupaten, "'.\n")
    } else {
      paste0(sapa, "Tidak ada konteks spesifik.\n")
    }

    sys <- paste0(
      "Kamu adalah SYLVA - asisten analitik pariwisata Sulawesi yang cerdas, hangat, dan asyik!\n",
      "Riwayat percakapan sudah mencakup semua data yang dibutuhkan.\n",
      ctx_note,
      "Aturan:\n",
      "- Jika user menyebutkan namanya di riwayat, ingat dan gunakan namanya.\n",
      "- Jawab secara natural dan ringkas berdasarkan riwayat chat.\n",
      "- Jangan ulangi data yang sudah disebutkan kecuali diminta.\n",
      "- JANGAN mengarang nama orang, nama tempat, atau fakta yang tidak ada di riwayat.\n",
      "- Bahasa santai + emoji secukupnya.\n",
      "- Akhiri dengan pertanyaan balik jika relevan.\n",
      "- Di luar pariwisata Sulawesi: tolak sopan + sedikit humor.\n"
    )
    message("[SYLVA][followup] Lightweight prompt ~", round(nchar(sys) / 4), " tokens")
    sys
  }

  # ---------------------------------------------------------------
  # RENDER BUBBLE CHAT
  # ---------------------------------------------------------------
  output$sylva_messages_ui <- renderUI({
    hist    <- chat_state$history
    loading <- chat_state$loading

    bubbles <- lapply(hist, function(msg) {
      is_user    <- msg$role == "user"
      row_cls    <- if (is_user) "sylva-msg-row user-row" else "sylva-msg-row bot-row"
      bubble_cls <- if (is_user) "sylva-bubble user" else "sylva-bubble bot"
      tags$div(class = row_cls, tags$div(class = bubble_cls, msg$content))
    })

    if (loading) {
      bubbles <- c(bubbles, list(
        tags$div(
          class = "sylva-msg-row bot-row",
          tags$div(class = "sylva-bubble typing",
                   tags$span(class = "sylva-dots",
                             tags$span(), tags$span(), tags$span()))
        )
      ))
    }
    tagList(bubbles)
  })

  # ---------------------------------------------------------------
  # OBSERVEEVENT: Tangkap pesan user & panggil OpenAI
  # ---------------------------------------------------------------
  observeEvent(input$sylva_user_msg, {
    req(nzchar(trimws(input$sylva_user_msg)))
    user_text <- trimws(input$sylva_user_msg)

    # 1. Tampilkan bubble user & loading SEGERA via JS
    session$sendCustomMessage("sylvaAddUserBubble", list(text = user_text))
    session$sendCustomMessage("sylvaShowLoading",   list())
    session$sendCustomMessage("sylvaDisableBtn",    list(disabled = TRUE))

    # 2. Simpan ke history
    chat_state$history <- c(chat_state$history, list(list(role = "user", content = user_text)))
    chat_state$loading <- TRUE

    # 3. Deteksi entitas (3-tier)
    entity <- detect_entity(user_text, df_final)
    message("[SYLVA] Entity detected: ", entity$type)

    # 4. Update memori & pilih mode prompt
    if (entity$type == "destinasi") {
      # TAHAP 1: Destinasi spesifik ditemukan → update KEDUA memori
      chat_state$mem_destinasi <- tolower(entity$dest_row$nama_wisata)
      chat_state$mem_kabupaten <- tolower(entity$dest_row$kabupaten)
      sys_prompt <- build_data_prompt(chat_state$history, df_final, entity)

    } else if (entity$type %in% c("kabupaten", "provinsi")) {
      # TAHAP 2: Wilayah baru → update kabupaten, HAPUS memori destinasi
      chat_state$mem_kabupaten <- if (!is.null(entity$kab_filter)) tolower(entity$kab_filter) else ""
      chat_state$mem_destinasi <- ""
      sys_prompt <- build_data_prompt(chat_state$history, df_final, entity)

    } else if (entity$type == "list_followup") {
      # TAHAP 3a: List request — bangun ulang entity dari memori
      # User minta daftar, kita harus kirim data nyata bukan prompt ringan
      if (nzchar(chat_state$mem_destinasi)) {
        # Sedang bahas destinasi spesifik → cari row-nya & kirim detail + serupa
        dest_row_mem <- df_final[tolower(df_final$nama_wisata) == chat_state$mem_destinasi, ]
        if (nrow(dest_row_mem) > 0) {
          entity_mem <- list(type = "destinasi", dest_row = dest_row_mem[1, ],
                             kab_filter = dest_row_mem[1, "kabupaten"])
        } else {
          entity_mem <- list(type = "kabupaten", dest_row = NULL,
                             kab_filter = chat_state$mem_kabupaten)
        }
      } else if (nzchar(chat_state$mem_kabupaten)) {
        # Sedang bahas kabupaten → kirim daftar HANYA kabupaten tersebut
        # Sertakan context_note agar LLM tahu mengapa ia menerima data kabupaten ini
        ctx_note_mem <- paste0("User meminta informasi/rekomendasi wisata di ",
                               chat_state$mem_kabupaten, ".")
        entity_mem <- list(type = "kabupaten", dest_row = NULL,
                           kab_filter = chat_state$mem_kabupaten,
                           context_note = ctx_note_mem)
      } else {
        entity_mem <- list(type = "followup", dest_row = NULL, kab_filter = NULL,
                           context_note = NULL)
      }
      sys_prompt <- build_data_prompt(chat_state$history, df_final, entity_mem)

    } else {
      # TAHAP 3b: Chitchat murni → prompt ringan jika ada memori, data umum jika tidak
      has_memory <- nzchar(chat_state$mem_destinasi) || nzchar(chat_state$mem_kabupaten)
      if (has_memory && length(chat_state$history) >= 4) {
        sys_prompt <- build_followup_prompt(chat_state$mem_destinasi,
                                            chat_state$mem_kabupaten,
                                            chat_state$user_name)
      } else {
        entity_general <- list(type = "followup", dest_row = NULL, kab_filter = NULL,
                               context_note = NULL)
        sys_prompt <- build_data_prompt(chat_state$history, df_final, entity_general)
      }
    }

    # 4b. Ekstrak nama user dari pesan awal jika belum tersimpan
    if (!nzchar(chat_state$user_name) && length(chat_state$history) <= 6) {
      early_text <- tolower(paste(sapply(head(chat_state$history, 6), function(x) x$content), collapse = " "))
      # Pola: "nama saya X", "saya X", "panggil saya X", "halo saya X", "halo, saya X"
      name_match <- regmatches(early_text,
        regexpr("(?:nama saya|panggil saya|halo[,!]? saya|saya) ([a-z]{3,12})",
                early_text, perl = TRUE))
      if (length(name_match) > 0) {
        extracted <- trimws(sub(".*(?:nama saya|panggil saya|halo[,!]? saya|saya) ", "", name_match[1], perl = TRUE))
        # Jangan simpan kata umum sebagai nama
        not_names <- c("orang", "dari", "butuh", "mau", "ingin", "perlu", "akan", "bisa")
        if (nchar(extracted) >= 3 && !(extracted %in% not_names)) {
          chat_state$user_name <- extracted
          message("[SYLVA] Nama user terdeteksi: ", extracted)
        }
      }
    }

    # 5. Susun payload — preservasi 2 pesan pertama + 8 pesan terbaru
    # Ini mencegah SYLVA lupa nama/konteks yang diperkenalkan di awal percakapan.
    hist <- chat_state$history
    n_h  <- length(hist)
    if (n_h > 10) {
      first_msgs  <- hist[1:min(2, n_h)]              # selalu simpan 2 pesan pembuka
      recent_msgs <- hist[max(3, n_h - 7):n_h]        # 8 pesan terbaru
      # Gabungkan, hilangkan duplikat jika overlap
      hist <- unique(c(first_msgs, recent_msgs))
    }

    messages_payload <- c(list(list(role = "system", content = sys_prompt)), hist)

    req_body <- list(
      model       = "gpt-4o-mini",
      messages    = messages_payload,
      max_tokens  = 800L,
      temperature = 0.7
    )

    # 6. Panggil OpenAI API
    resp <- tryCatch(
      httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(Authorization = paste("Bearer", OPENAI_KEY)),
        httr::content_type_json(),
        body   = jsonlite::toJSON(req_body, auto_unbox = TRUE),
        encode = "raw"
      ),
      error = function(e) { message("[SYLVA] API error: ", e$message); NULL }
    )

    # 7. Proses respons
    session$sendCustomMessage("sylvaHideLoading", list())
    chat_state$loading <- FALSE
    session$sendCustomMessage("sylvaDisableBtn", list(disabled = FALSE))

    if (is.null(resp) || httr::http_error(resp)) {
      if (!is.null(resp)) {
        ed <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "")
        message("[SYLVA] HTTP error ", httr::status_code(resp), ": ", ed)
      }
      bot_reply <- "\u26a0\ufe0f Maaf, terjadi gangguan koneksi. Silakan coba lagi!"
    } else {
      resp_text <- httr::content(resp, "text", encoding = "UTF-8")
      parsed    <- jsonlite::fromJSON(resp_text, flatten = TRUE)
      bot_reply <- tryCatch(
        parsed$choices$message.content[[1]],
        error = function(e) {
          message("[SYLVA] Parse error: ", e$message)
          "\u26a0\ufe0f Maaf, respons tidak dapat diproses. Coba lagi."
        }
      )
    }

    chat_state$history <- c(chat_state$history, list(list(role = "assistant", content = bot_reply)))
    session$sendCustomMessage("sylvaRenderMd", list())
    session$sendCustomMessage("sylvaScroll",   list())
  })
}


  # ---------------------------------------------------------------
  # INTENT DETECTOR
