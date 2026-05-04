// ============================================================
// PCA 2D / 3D Toggle — Client-side panel switching
// ============================================================
function setPcaDim(dim) {
  var btn2d = document.getElementById('btn_pca_2d');
  var btn3d = document.getElementById('btn_pca_3d');
  var panel2d = document.getElementById('pca_2d_panel');
  var panel3d = document.getElementById('pca_3d_panel');

  if (!btn2d || !btn3d || !panel2d || !panel3d) return;

  if (dim === '3D') {
    btn2d.classList.remove('active');
    btn3d.classList.add('active');
    // Sembunyikan 2D dulu, lalu tampilkan 3D
    panel2d.classList.remove('active');
    panel3d.classList.add('active');

    // Paksa Plotly resize agar canvas WebGL 3D merender dengan benar
    // Perlu delay cukup agar display:block sudah di-apply oleh browser
    setTimeout(function () {
      var el3d = document.querySelector('#pca_3d_panel .js-plotly-plot');
      if (el3d) {
        try { Plotly.Plots.resize(el3d); } catch (e) { }
        // Double resize setelah animasi CSS selesai (~400ms)
        setTimeout(function () {
          try { Plotly.Plots.resize(el3d); } catch (e) { }
        }, 420);
      }
    }, 50);

  } else {
    btn3d.classList.remove('active');
    btn2d.classList.add('active');
    panel3d.classList.remove('active');
    panel2d.classList.add('active');

    // Resize 2D plot setelah tampil
    setTimeout(function () {
      var el2d = document.querySelector('#pca_2d_panel .js-plotly-plot');
      if (el2d) {
        try { Plotly.Plots.resize(el2d); } catch (e) { }
      }
    }, 80);
  }

  // Opsional: kirim state ke Shiny jika dibutuhkan reaktivitas server
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('pca_dim_active', dim, { priority: 'event' });
  }
}

// ============================================================
// Download Modal — buka / tutup overlay
// ============================================================
Shiny.addCustomMessageHandler('toggleDlModal', function (data) {
  var overlay = document.getElementById('dl_modal_overlay');
  if (!overlay) return;
  if (data.show) {
    overlay.classList.add('active');
    document.body.style.overflow = 'hidden';
  } else {
    overlay.classList.remove('active');
    document.body.style.overflow = '';
  }
});

// Tutup modal jika klik di luar box
document.addEventListener('click', function (e) {
  var overlay = document.getElementById('dl_modal_overlay');
  if (overlay && overlay.classList.contains('active') && e.target === overlay) {
    overlay.classList.remove('active');
    document.body.style.overflow = '';
    // Beritahu Shiny agar state sinkron (opsional)
    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue('btn_dl_close', Math.random(), { priority: 'event' });
    }
  }
});

// Indeterminate state untuk checkbox "Pilih Semua Kolom"
document.addEventListener('change', function (e) {
  if (e.target && e.target.id && e.target.id.startsWith('dl_col_')) {
    var allCheckboxes = document.querySelectorAll('.dl-checkbox-grid input[type="checkbox"]:not(#dl_check_all)');
    var checkAllBox = document.getElementById('dl_check_all');
    if (!checkAllBox) return;

    var checkedCount = 0;
    allCheckboxes.forEach(function(cb) {
      if (cb.checked) checkedCount++;
    });

    if (checkedCount === 0) {
      checkAllBox.checked = false;
      checkAllBox.indeterminate = false;
    } else if (checkedCount === allCheckboxes.length) {
      checkAllBox.checked = true;
      checkAllBox.indeterminate = false;
    } else {
      checkAllBox.checked = false;
      checkAllBox.indeterminate = true;
    }
  }
});

// ============================================================
// Custom Handler untuk mengupdate dropdown Kabupaten via JS
// Menggunakan JS murni untuk memanipulasi Selectize API secara langsung
// agar bug cache/re-render Shiny tidak mengunci pilihan dropdown.
// ============================================================
Shiny.addCustomMessageHandler('updateKabupatenJS', function (data) {
  var $el = $('#ov_kabupaten');
  if ($el.length === 0) return;

  // Periksa apakah selectize sudah diinisialisasi oleh Shiny
  if ($el[0].selectize) {
    var control = $el[0].selectize;
    control.clearOptions(); // Hapus pilihan lama
    control.addOption({ value: 'Semua', label: 'Semua' });
    data.choices.forEach(function (c) {
      control.addOption({ value: c, label: c });
    });
    // Reset nilai ke 'Semua'. Ini otomatis memicu event 'change' untuk dikirim ke R.
    control.setValue('Semua', false); // false = don't silence event
  } else {
    // Fallback jika form masih berupa tag <select> HTML native
    $el.empty();
    $el.append($('<option>', { value: 'Semua', text: 'Semua' }));
    data.choices.forEach(function (c) {
      $el.append($('<option>', { value: c, text: c }));
    });
    $el.val('Semua').trigger('change');
  }
});

// ============================================================
// Handler: reset mobile search query dari server
// (dipanggil saat btn_reset_search diklik)
// ============================================================
Shiny.addCustomMessageHandler('resetMobileSearch', function (data) {
  var inp = document.getElementById('mobile_search_native');
  if (inp) { inp.value = ''; }
  toggleClearBtn(false);
  hideSuggestions();
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('mobile_search_query', '', { priority: 'event' });
  }
});

// ============================================================
// PORTAL PATTERN: Pindahkan elemen mobile search ke document.body
//
// ALASAN: #mobile-search-panel ditempatkan di dalam dashboardBody
// (content-wrapper). Jika content-wrapper punya transform/filter
// (misalnya efek blur saat sidebar terbuka), maka position:fixed
// anak-anaknya menjadi relatif ke content-wrapper, bukan viewport.
// Ini menyebabkan keyboard mobile dismiss saat mengetik.
// Solusi: pindahkan ke document.body langsung setelah Shiny siap.
// ============================================================
$(document).on('shiny:sessioninitialized', function () {
  var panel = document.getElementById('mobile-search-panel');
  var overlay = document.getElementById('mobile-search-overlay');
  if (panel) document.body.appendChild(panel);
  if (overlay) document.body.appendChild(overlay);
});

// ============================================================
// Tutup sidebar mobile saat area kosong (konten/peta) diklik
// ============================================================
document.addEventListener('click', function (e) {
  // Hanya jalankan logika ini di layar mobile dan saat sidebar terbuka
  if (window.innerWidth <= 767 && document.body.classList.contains('sidebar-open')) {
    var clickedInsideSidebar = $(e.target).closest('.main-sidebar').length > 0;
    var clickedToggleBtn = $(e.target).closest('.sidebar-toggle').length > 0;

    // Jika yang diklik BUKAN area sidebar dan BUKAN tombol hamburger/X
    if (!clickedInsideSidebar && !clickedToggleBtn) {
      // Menggunakan click() pada toggle menjamin state AdminLTE sinkron
      var toggleBtn = document.querySelector('.sidebar-toggle');
      if (toggleBtn) toggleBtn.click();
    }
  }
}, true); // useCapture = true krusial agar klik tidak dihentikan (stopPropagation) oleh Peta Leaflet!

// ============================================================
// Tutup sidebar mobile secara otomatis saat tab menu dipilih
// ============================================================
$(document).on('click', '.sidebar-menu a', function (e) {
  if (window.innerWidth <= 767 && document.body.classList.contains('sidebar-open')) {
    // Pastikan menu yang diklik adalah link tab yang valid (menghindari penutupan prematur)
    if ($(this).attr('data-toggle') === 'tab' || $(this).attr('data-value')) {
      setTimeout(function () {
        $('.sidebar-toggle').click();
      }, 150);
    }
  }
});

// ============================================================
// Mobile Full-Width Search Panel
// ============================================================
var mobileSearchTimer = null;
var uiDebounceTimer = null;
var MAX_SUGGESTIONS = 8;

// ---- Toggle search panel ----
function toggleMobileSearch() {
  if (document.body.classList.contains('mobile-search-active')) {
    closeMobileSearch();
  } else {
    openMobileSearch();
  }
}

// ---- Buka search panel ----
function openMobileSearch() {
  document.body.classList.add('mobile-search-active');
  var icon = document.querySelector('#mobile-search-toggle i');
  if (icon) { icon.classList.remove('fa-search'); icon.classList.add('fa-times'); }

  // Fokus input setelah animasi top selesai (~320ms)
  setTimeout(function () {
    var inp = document.getElementById('mobile_search_native');
    if (inp) {
      inp.focus();
      if (inp.value.trim().length > 0) buildSuggestions(inp.value.trim());
    }
  }, 340);
}

// ---- Tutup search panel DENGAN reset query (cancel / ESC / overlay) ----
function closeMobileSearch() {
  document.body.classList.remove('mobile-search-active');
  var icon = document.querySelector('#mobile-search-toggle i');
  if (icon) { icon.classList.remove('fa-times'); icon.classList.add('fa-search'); }

  var inp = document.getElementById('mobile_search_native');
  if (inp) { inp.value = ''; inp.blur(); }

  toggleClearBtn(false);
  hideSuggestions();

  // Reset Shiny → konten kembali ke overview
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('mobile_search_query', '', { priority: 'event' });
  }
}

// ---- Tutup panel TANPA reset query (setelah user pilih destinasi) ----
// Query tetap ada di Shiny sehingga detail panel tetap tampil
function closeMobileSearchAfterSelect() {
  document.body.classList.remove('mobile-search-active');
  var icon = document.querySelector('#mobile-search-toggle i');
  if (icon) { icon.classList.remove('fa-times'); icon.classList.add('fa-search'); }

  hideSuggestions();
  var inp = document.getElementById('mobile_search_native');
  if (inp) inp.blur();
  // TIDAK reset inp.value dan TIDAK reset Shiny query
}

// ---- Clear isi input saja (tombol X, tanpa menutup panel) ----
function clearMobileSearch() {
  var inp = document.getElementById('mobile_search_native');
  if (inp) { inp.value = ''; inp.focus(); }
  toggleClearBtn(false);
  hideSuggestions();
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('mobile_search_query', '', { priority: 'event' });
  }
}

// ---- Toggle tombol clear (X) ----
function toggleClearBtn(show) {
  var btn = document.getElementById('mobile-search-clear');
  if (!btn) return;
  if (show) btn.classList.add('visible');
  else btn.classList.remove('visible');
}

// ---- Sembunyikan suggestion list ----
function hideSuggestions() {
  var ul = document.getElementById('mobile-search-suggestions');
  if (!ul) return;
  ul.classList.remove('visible');
  setTimeout(function () {
    if (!ul.classList.contains('visible')) ul.innerHTML = '';
  }, 200);
}

// ---- Highlight keyword dalam teks ----
function highlightMatch(text, keyword) {
  if (!keyword) return escapeHtml(text);
  var escaped = escapeHtml(text);
  var re = new RegExp('(' + escapeRegex(keyword) + ')', 'gi');
  return escaped.replace(re, '<mark>$1</mark>');
}

function escapeHtml(str) {
  return str
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

function escapeRegex(str) {
  return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// ---- Bangun suggestion list dari wisataNames ----
function buildSuggestions(query) {
  var ul = document.getElementById('mobile-search-suggestions');
  if (!ul) return;

  var names = (typeof wisataNames !== 'undefined') ? wisataNames : [];
  if (!query || query.length === 0) { hideSuggestions(); return; }

  var lower = query.toLowerCase();
  var matched = names.filter(function (n) {
    return n.toLowerCase().indexOf(lower) !== -1;
  });

  // Bangun HTML sekaligus (satu kali innerHTML set) untuk minimal reflow
  if (matched.length === 0) {
    ul.innerHTML = '<li class="no-result">Destinasi tidak ditemukan</li>';
  } else {
    var html = '';
    matched.slice(0, MAX_SUGGESTIONS).forEach(function (name) {
      html +=
        '<li data-name="' + escapeHtml(name) + '">' +
        '<span class="sugg-icon"><i class="fa fa-map-marker-alt"></i></span>' +
        '<span class="sugg-text">' + highlightMatch(name, query) + '</span>' +
        '</li>';
    });
    ul.innerHTML = html;
  }

  // Pasang event listeners
  var items = ul.querySelectorAll('li:not(.no-result)');
  items.forEach(function (li) {
    var name = li.getAttribute('data-name');

    // touchstart preventDefault: cegah blur pada input saat user tap suggestion
    li.addEventListener('touchstart', function (e) {
      e.preventDefault();
    }, { passive: false });

    // touchend: aksi pilih
    li.addEventListener('touchend', function (e) {
      e.preventDefault();
      selectSuggestion(name);
    });

    // click: mouse fallback
    li.addEventListener('click', function (e) {
      e.preventDefault();
      selectSuggestion(name);
    });
  });

  ul.classList.add('visible');
}

// ---- Pilih suggestion → kirim ke Shiny, tutup panel (simpan query) ----
function selectSuggestion(name) {
  var inp = document.getElementById('mobile_search_native');
  if (inp) inp.value = name;
  toggleClearBtn(true);

  // Kirim ke Shiny SEBELUM menutup panel
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('mobile_search_query', name, { priority: 'event' });
  }

  // Tutup panel TANPA reset query → detail wisata tetap tampil
  closeMobileSearchAfterSelect();
}

// ---- Event: input pengguna (real-time) ----
$(document).on('input', '#mobile_search_native', function (e) {
  e.stopPropagation(); // Cegah Shiny menangkap event ketikan real-time
  var val = this.value;

  toggleClearBtn(val.length > 0); // Manipulasi CSS ringan, bisa dieksekusi langsung

  // Debounce render UI (daftar saran) selama 200ms untuk menghindari nge-freeze
  clearTimeout(uiDebounceTimer);
  uiDebounceTimer = setTimeout(function () {
    // requestAnimationFrame memastikan render HTML berat tidak memblokir antarmuka
    window.requestAnimationFrame(function () {
      buildSuggestions(val.trim());
    });
  }, 200);

  // Gunakan logika THROTTLING (maksimal 1x pengiriman per 500ms)
  // Ini memastikan Shiny.setInputValue tidak di-bombardir, seberapa cepat pun user mengetik
  if (!mobileSearchTimer) {
    mobileSearchTimer = setTimeout(function () {
      mobileSearchTimer = null; // Lepaskan kunci throttle
      var latestVal = document.getElementById('mobile_search_native').value.trim();

      if (typeof Shiny !== 'undefined') {
        if (latestVal.length >= 3 || latestVal.length === 0) {
          Shiny.setInputValue('mobile_search_query', latestVal, { priority: 'event' });
        }
      }
    }, 500);
  }
});

// ---- Tekan Escape untuk menutup, atau Enter untuk search ----
$(document).on('keydown', '#mobile_search_native', function (e) {
  if (e.key === 'Enter') {
    e.preventDefault();
    var val = this.value.trim();
    if (val.length > 0) {
      if (typeof Shiny !== 'undefined') {
        Shiny.setInputValue('mobile_search_query', val, { priority: 'event' });
      }
      closeMobileSearchAfterSelect();
    } else {
      closeMobileSearch();
    }
  }
});

$(document).on('keydown', function (e) {
  if (e.key === 'Escape' && document.body.classList.contains('mobile-search-active')) {
    closeMobileSearch();
  }
});

// ============================================================
// Tutup sidebar mobile saat area kosong (konten/peta) diklik
// ============================================================
// PERBAIKAN: Menggunakan overlay transparan khusus sebagai penangkap klik
// di luar sidebar, BUKAN useCapture=true yang mengganggu selectize dropdown.
// useCapture=true menyebabkan handler ini berjalan SEBELUM selectize sempat
// membuka dropdown-nya, sehingga dropdown langsung tertutup.
// ============================================================
// ============================================================
// Tutup sidebar mobile saat area kosong (konten/peta) diklik/ditap
// ============================================================
// PENDEKATAN: Overlay transparan (#sidebar-close-overlay) di belakang sidebar.
// KRUSIAL: overlay harus dinonaktifkan saat selectize dropdown sedang terbuka,
// karena dropdown ter-render sebagai fixed element di LUAR .main-sidebar,
// sehingga tap pada dropdown akan jatuh ke overlay dan menutup sidebar.
// ============================================================
$(document).on('shiny:sessioninitialized', function () {
  var sidebarOverlay = document.createElement('div');
  sidebarOverlay.id = 'sidebar-close-overlay';
  sidebarOverlay.style.cssText = [
    'display:none',
    'position:fixed',
    'top:0', 'left:0', 'right:0', 'bottom:0',
    'z-index:1023',
    'background:transparent',
    'cursor:pointer',
    '-webkit-tap-highlight-color:transparent'
  ].join(';');
  document.body.appendChild(sidebarOverlay);

  sidebarOverlay.addEventListener('click', function (e) {
    // Jangan tutup jika selectize dropdown sedang terbuka
    if (document.querySelector('.selectize-dropdown:visible') ||
      document.querySelector('.selectize-dropdown[style*="display: block"]') ||
      document.querySelector('.selectize-dropdown').offsetParent !== null) {
      return;
    }
    closeSidebarMobile();
  });

  sidebarOverlay.addEventListener('touchend', function (e) {
    // Cek apakah touch target adalah bagian dari selectize dropdown
    var target = e.target || e.changedTouches[0].target;
    if ($(target).closest('.selectize-dropdown, .selectize-input').length > 0) {
      return; // Biarkan selectize menangani sendiri
    }
    e.preventDefault();
    closeSidebarMobile();
  }, { passive: false });
});

// Nonaktifkan overlay saat selectize membuka dropdown-nya
$(document).on('focus', '.selectize-input input', function () {
  var overlay = document.getElementById('sidebar-close-overlay');
  if (overlay) overlay.style.pointerEvents = 'none';
});
// Aktifkan kembali overlay saat selectize dropdown ditutup
$(document).on('blur', '.selectize-input input', function () {
  // Tunda sedikit agar selectize sempat memproses pilihan user
  setTimeout(function () {
    var overlay = document.getElementById('sidebar-close-overlay');
    if (overlay && document.body.classList.contains('sidebar-open')) {
      overlay.style.pointerEvents = 'auto';
    }
  }, 300);
});

function closeSidebarMobile() {
  document.body.classList.remove('sidebar-open');
  document.body.classList.remove('sidebar-collapse');
  var overlay = document.getElementById('sidebar-close-overlay');
  if (overlay) overlay.style.display = 'none';
}

// Pantau perubahan class sidebar-open untuk show/hide overlay
var _sidebarObserver = new MutationObserver(function () {
  if (window.innerWidth > 767) return;
  var overlay = document.getElementById('sidebar-close-overlay');
  if (!overlay) return;
  if (document.body.classList.contains('sidebar-open')) {
    overlay.style.display = 'block';
  } else {
    overlay.style.display = 'none';
  }
});
_sidebarObserver.observe(document.body, { attributes: true, attributeFilter: ['class'] });

// ============================================================
// Tutup sidebar mobile secara otomatis saat tab menu dipilih
// ============================================================
$(document).on('click', '.sidebar-menu a', function (e) {
  if (window.innerWidth <= 767 && document.body.classList.contains('sidebar-open')) {
    if ($(this).attr('data-value')) {
      setTimeout(function () {
        closeSidebarMobile();
      }, 150);
    }
  }
});

// ============================================================
// Isolasi Event Click pada Filter (Selectize) — versi aman
// ============================================================
// Catatan: stopPropagation DIHAPUS karena handler penutup sidebar kini
// menggunakan overlay terpisah (#sidebar-close-overlay), BUKAN event
// propagation. stopPropagation justru dapat mengganggu Shiny/selectize
// dalam beberapa skenario edge case.
//
// Satu-satunya perlindungan yang masih dibutuhkan: pastikan klik pada
// tombol hamburger (sidebar-toggle) tidak memicu overlay close.
$(document).on('click', '.sidebar-toggle', function (e) {
  // Toggle manual: jika sidebar sedang terbuka, tutup; jika tidak, buka.
  if (window.innerWidth <= 767) {
    // Biarkan AdminLTE menangani toggle, kita hanya sync overlay
    setTimeout(function () {
      var overlay = document.getElementById('sidebar-close-overlay');
      if (!overlay) return;
      overlay.style.display = document.body.classList.contains('sidebar-open') ? 'block' : 'none';
    }, 50);
  }
});