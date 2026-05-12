# ============================================================
# SYLVA CHAT CACHE — Persistent Conversation Memory
# ============================================================
# Menyimpan riwayat percakapan per-session ke file .rds lokal.
# Setiap session mendapat file cache unik berdasarkan session_id.
# Cache otomatis kedaluwarsa setelah CACHE_TTL_DAYS hari.
# ============================================================

# --- Konfigurasi Cache ---
CACHE_DIR      <- "chat_cache"          # folder penyimpanan (dibuat otomatis)
CACHE_TTL_DAYS <- 7                     # cache kedaluwarsa setelah 7 hari
CACHE_MAX_MSGS <- 200                   # maksimum pesan yang disimpan per session

# Pastikan folder cache ada
ensure_cache_dir <- function() {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
}

# ---------------------------------------------------------------
# Buat path file cache untuk session tertentu
# ---------------------------------------------------------------
cache_path <- function(session_id) {
  ensure_cache_dir()
  file.path(CACHE_DIR, paste0("session_", session_id, ".rds"))
}

# ---------------------------------------------------------------
# LOAD: Memuat riwayat percakapan dari cache
# Mengembalikan list berisi: history, mem_kabupaten, mem_destinasi,
# user_name, saved_at
# Mengembalikan NULL jika cache tidak ada atau sudah kedaluwarsa
# ---------------------------------------------------------------
cache_load <- function(session_id) {
  path <- cache_path(session_id)

  if (!file.exists(path)) {
    message("[CACHE] No cache found for session: ", session_id)
    return(NULL)
  }

  cache <- tryCatch(readRDS(path), error = function(e) {
    message("[CACHE] Failed to read cache: ", e$message)
    NULL
  })

  if (is.null(cache)) return(NULL)

  # Cek apakah cache sudah kedaluwarsa
  if (!is.null(cache$saved_at)) {
    age_days <- as.numeric(difftime(Sys.time(), cache$saved_at, units = "days"))
    if (age_days > CACHE_TTL_DAYS) {
      message("[CACHE] Cache expired (", round(age_days, 1), " days old). Deleting.")
      file.remove(path)
      return(NULL)
    }
  }

  message("[CACHE] Loaded ", length(cache$history), " messages for session: ", session_id,
          " (saved ", round(as.numeric(difftime(Sys.time(), cache$saved_at, units = "hours")), 1),
          " hours ago)")
  cache
}

# ---------------------------------------------------------------
# SAVE: Menyimpan state percakapan ke cache
# ---------------------------------------------------------------
cache_save <- function(session_id, history, mem_kabupaten = "", mem_destinasi = "", user_name = "") {
  path <- cache_path(session_id)

  # Batasi jumlah pesan yang disimpan (hindari file terlalu besar)
  n <- length(history)
  if (n > CACHE_MAX_MSGS) {
    # Simpan 2 pesan pertama (perkenalan) + pesan terbaru
    first_msgs  <- history[1:min(2, n)]
    recent_msgs <- history[max(3, n - CACHE_MAX_MSGS + 3):n]
    history     <- unique(c(first_msgs, recent_msgs))
    message("[CACHE] Trimmed history to ", length(history), " messages (was ", n, ")")
  }

  cache <- list(
    history       = history,
    mem_kabupaten = mem_kabupaten,
    mem_destinasi = mem_destinasi,
    user_name     = user_name,
    saved_at      = Sys.time(),
    session_id    = session_id
  )

  tryCatch({
    saveRDS(cache, path)
    message("[CACHE] Saved ", length(history), " messages for session: ", session_id)
  }, error = function(e) {
    message("[CACHE] Failed to save cache: ", e$message)
  })
}

# ---------------------------------------------------------------
# CLEAR: Hapus cache untuk session tertentu
# ---------------------------------------------------------------
cache_clear <- function(session_id) {
  path <- cache_path(session_id)
  if (file.exists(path)) {
    file.remove(path)
    message("[CACHE] Cleared cache for session: ", session_id)
    return(TRUE)
  }
  message("[CACHE] No cache to clear for session: ", session_id)
  FALSE
}

# ---------------------------------------------------------------
# PURGE: Hapus semua cache yang sudah kedaluwarsa (maintenance)
# Dipanggil otomatis saat aplikasi dimulai
# ---------------------------------------------------------------
cache_purge_expired <- function() {
  ensure_cache_dir()
  files <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(invisible(0))

  n_deleted <- 0
  for (f in files) {
    cache <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(cache) || is.null(cache$saved_at)) {
      file.remove(f)
      n_deleted <- n_deleted + 1
      next
    }
    age_days <- as.numeric(difftime(Sys.time(), cache$saved_at, units = "days"))
    if (age_days > CACHE_TTL_DAYS) {
      file.remove(f)
      n_deleted <- n_deleted + 1
    }
  }

  if (n_deleted > 0) message("[CACHE] Purged ", n_deleted, " expired cache file(s)")
  invisible(n_deleted)
}

# ---------------------------------------------------------------
# INFO: Daftar semua session cache yang aktif (untuk debugging)
# ---------------------------------------------------------------
cache_list_sessions <- function() {
  ensure_cache_dir()
  files <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("[CACHE] No active sessions found.")
    return(invisible(NULL))
  }

  info <- lapply(files, function(f) {
    cache <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(cache)) return(NULL)
    data.frame(
      session_id    = cache$session_id %||% basename(f),
      n_messages    = length(cache$history),
      user_name     = if (nzchar(cache$user_name %||% "")) cache$user_name else "(unknown)",
      last_topic    = if (nzchar(cache$mem_destinasi %||% "")) cache$mem_destinasi
                      else if (nzchar(cache$mem_kabupaten %||% "")) cache$mem_kabupaten
                      else "(general)",
      saved_at      = as.character(cache$saved_at),
      age_hours     = round(as.numeric(difftime(Sys.time(), cache$saved_at, units = "hours")), 1),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), info))
  message("[CACHE] Active sessions: ", nrow(result))
  result
}

# Jalankan purge saat file ini di-source
cache_purge_expired()
