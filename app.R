# ============================================================
# app.R — Entry Point Utama
# Semua logika telah dipindahkan ke file terpisah:
#   - global.R       : library, data, K-Means, Random Forest
#   - ui.R           : definisi UI dashboardPage(...)
#   - server.R       : logika server + shinyApp(ui, server)
#   - chatbot.R      : modul SYLVA chatbot
#   - www/custom.css : semua CSS kustom & animasi mobile
#   - www/custom.js  : semua JS kustom (handler updateKabupaten)
# ============================================================

# Shiny secara otomatis memuat global.R, ui.R, dan server.R
# ketika aplikasi dijalankan dari direktori ini.
# Jalankan baris di bawah untuk memulai aplikasi dari file ini.

shiny::runApp()
