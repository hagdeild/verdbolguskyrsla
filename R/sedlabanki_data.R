# ==============================================================================
# SEÐLABANKAGÖGN - SÓTT SJÁLFVIRKT (kemur í stað handvirkra blaða í
# data/verdbolguskyrsla_data.xlsx)
# ==============================================================================
#
# Sourced af R/data_preparation.R. Hvert fall skilar tibble á NÁKVÆMLEGA sama
# formi og gamla Excel-lesturinn skilaði, svo kóðinn neðar í data_preparation.R
# er óbreyttur.
#
#   Blað í xlsx         Fall                    Heimild
#   ------------------  ----------------------  -----------------------------------
#   meginvextir         si_meginvextir()        XML tímaröð 17923
#   vextir              si_almennir_vextir()    mánaðarlegar vaxtatilkynningar SÍ
#   inngrip_si          si_inngrip()            XML tímaraðir 284 / 287 / 285
#   bank_stabbi         si_utlan_bankar()       Excel á sedlabanki.is/library
#   bank_ny_utlan       si_ny_utlan_bankar()    Excel á sedlabanki.is/library
#   lif_stabbi          si_utlan_lif()          gagnabanki.is (headless Chrome)
#   lif_ny_utlan        si_ny_utlan_lif()       gagnabanki.is (headless Chrome)
#   lanasjodir_stabbi   si_utlan_lanasjodir()   gagnabanki.is (headless Chrome)
#
# Blaðið "spar" (spár bankanna) er ENN handvirkt - spár bankanna eru aðeins
# birtar í fréttagreinum/PDF og ekki í neinu véllesanlegu formi.
#
# Ólíkt gömlu lestrunum eru raðir hér fundnar eftir HEITI (ekki raðnúmeri) og
# dagsetningar lesnar úr hausröð skjalsins (ekki búnar til með seq.Date), svo
# að ný röð eða nýr mánuður í skjölum Seðlabankans færir ekkert til. Ef heiti
# finnst ekki stöðvast keyrslan með villu frekar en að skila röngum tölum.
#
# SKYNDIMINNI: hvert fall er keyrt í gegnum si_cached(). Takist niðurhal er
# niðurstaðan vistuð í data/sedlabanki_cache/<nafn>.rds; mistakist það er
# síðasta vistaða útgáfa notuð með viðvörun, svo að ein bilun hjá Seðlabankanum
# stöðvi ekki daglega uppfærslu. Fylgstu með viðvörunum í keyrslunni.
# ==============================================================================

si_cache_dir <- "data/sedlabanki_cache"

si_cached <- function(name, fetch) {
  path <- file.path(si_cache_dir, paste0(name, ".rds"))
  out <- tryCatch(fetch(), error = function(e) e)
  if (!inherits(out, "error")) {
    dir.create(si_cache_dir, showWarnings = FALSE, recursive = TRUE)
    write_rds(out, path)
    return(out)
  }
  if (!file.exists(path)) stop("Seðlabankagögn '", name, "': ", conditionMessage(out))
  warning("Seðlabankagögn '", name, "' - niðurhal mistókst, nota vistað afrit (",
          format(file.mtime(path), "%Y-%m-%d"), "): ", conditionMessage(out), call. = FALSE)
  read_rds(path)
}


# 1.0.0 HJÁLPARFÖLL ----

# XML tímaraðaþjónusta SÍ (sama og gengi í kafla 9.0.0 í data_preparation.R).
# Skilar date | value. Hópa og TimeSeriesID má fletta upp með
#   https://www.sedlabanki.is/xmltimeseries/Default.aspx?GroupID=<n>&DagsFra=...&DagsTil=...&Type=xml
si_xml_series <- function(id, from = "2007-01-01") {
  x <- xml2::read_xml(paste0(
    "https://www.sedlabanki.is/xmltimeseries/Default.aspx?TimeSeriesID=", id,
    "&DagsFra=", from, "&DagsTil=", today(), "&Type=xml"
  ))
  entries <- xml2::xml_find_all(x, ".//Entry")
  tibble(
    date = as.Date(mdy_hms(xml2::xml_text(xml2::xml_find_first(entries, ".//Date")))),
    value = as.numeric(xml2::xml_text(xml2::xml_find_first(entries, ".//Value")))
  ) |>
    filter(!is.na(value))
}

# gagnabanki.is geymir slóðir á Excel-skrár sínar í þýðingarskránni (lyklar sem
# enda á ".EXCEL"). Fyrir innlánsstofnanir eru það fastar skrár á
# sedlabanki.is/library?itemid=... - slóðin er lesin hér í hvert sinn frekar en
# harðkóðuð, ef itemid skyldi breytast.
si_library_xlsx <- function(key) {
  tr <- request("https://gagnabanki.is/api/translation/is") |>
    req_perform() |>
    resp_body_json()
  url <- tr[[key]]
  if (is.null(url)) stop("Lykill '", key, "' finnst ekki í þýðingarskrá gagnabanki.is")
  tmp <- tempfile(fileext = ".xlsx")
  request(url) |> req_perform(path = tmp)
  tmp
}

# Skýrslur á gagnabanki.is sem hafa ekki fasta skrá (lífeyrissjóðir,
# lánasjóðir). Excel-hnappurinn þar býr skrána til í vafranum sem Blob, svo
# síðan er opnuð í headless Chrome (chromote, eins og í althjodleg_data.R),
# URL.createObjectURL er "krækt" til að grípa Blob-ið og ýtt á "Excel".
# Tekið úr Vidaringa/macro_iceland (R/ingest/sedlabanki.R), þar sem þetta
# keyrir mánaðarlega.
si_gagnabanki_xlsx <- function(slug, report = NULL) {
  url <- paste0(
    "https://gagnabanki.is/report/", slug, "?from=1990-01-01&to=", today(),
    if (!is.null(report)) paste0("&page=", report) else ""
  )

  b <- chromote::ChromoteSession$new(wait_ = TRUE)
  on.exit(b$close(), add = TRUE)
  b$default_timeout <- 60
  # Smellur á "Excel" vistar líka afrit í niðurhalsmöppu stýrikerfisins - því
  # er hafnað hér, Blob-ið er gripið beint.
  try(b$Browser$setDownloadBehavior(behavior = "deny"), silent = TRUE)
  b$Page$navigate(url)
  b$Page$loadEventFired(wait_ = TRUE)

  b$Runtime$evaluate(paste0(
    "window.__capturedBlob=null;(function(){var o=URL.createObjectURL;",
    "URL.createObjectURL=function(b){try{if(b instanceof Blob)",
    "window.__capturedBlob=b;}catch(e){}return o.apply(this,arguments);};})();'ok'"
  ))

  # Hnappurinn er virkur áður en gögnin eru komin og smellur þá gerir ekkert,
  # svo það er smellt aftur þar til Blob birtist.
  click_excel <- paste0(
    "(function(){var s=Array.from(document.querySelectorAll",
    "('span.mdc-button__label')).find(s=>s.textContent.trim()==='Excel');",
    "if(!s)return'no-btn';(s.closest('button')||s).click();return'ok';})()"
  )
  has_blob <- function() isTRUE(b$Runtime$evaluate("!!window.__capturedBlob")$result$value)
  for (attempt in seq_len(12)) {
    b$Runtime$evaluate(click_excel)
    for (tick in seq_len(5)) {
      Sys.sleep(1)
      if (has_blob()) break
    }
    if (has_blob()) break
  }

  b64 <- b$Runtime$evaluate(paste0(
    "new Promise(function(res){var bl=window.__capturedBlob;",
    "if(!bl){res('NO_BLOB');return;}var fr=new FileReader();",
    "fr.onload=function(){res(fr.result.split(',')[1]);};fr.readAsDataURL(bl);})"
  ), awaitPromise = TRUE)$result$value
  if (is.null(b64) || identical(b64, "NO_BLOB")) {
    stop("gagnabanki.is skilaði engri Excel-skrá fyrir '", slug, "/", report, "'")
  }

  tmp <- tempfile(fileext = ".xlsx")
  writeBin(jsonlite::base64_dec(b64), tmp)
  tmp
}

# Les BREIÐ Seðlabanka-skjöl (mánuðir sem dálkar, liðir sem raðir).
#
# `rows` er nafngreindur listi: nafn = heiti stuðuls í úttaki, gildi = vektor
# af reglulegum segðum sem eru eltar í röð niður skjalið. Hver segð er leituð
# NEÐAN við línuna sem fyrri segð fann, t.d.
#   c("Verðtryggð útlán / Indexed", "veði í íbúð")
# = fyrsta "veði í íbúð" röðin fyrir neðan "Verðtryggð útlán"-fyrirsögnina.
# Röð með fleiri en einni lokasegð er lögð saman (ekki notað núna).
#
# Hausröðin er fundin sjálfkrafa: fyrsta röðin með a.m.k. 12 dagsetningum,
# annaðhvort "YYYY-MM" textar (gagnabanki.is) eða Excel-raðtölur (library-
# skrárnar, sem eru mánaðarlok - færðar á 1. dag mánaðar).
si_read_wide <- function(xlsx, sheet = 1, rows) {
  raw <- suppressMessages(
    read_excel(xlsx, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  )
  cells <- as.matrix(raw)

  as_month <- function(x) {
    out <- rep(as.Date(NA), length(x))
    ym <- !is.na(x) & str_detect(x, "^[0-9]{4}-[0-9]{2}$")
    out[ym] <- ym(x[ym])
    num <- suppressWarnings(as.numeric(x))
    serial <- !ym & !is.na(num) & num > 30000 & num < 70000
    out[serial] <- floor_date(as.Date(num[serial], origin = "1899-12-30"), "month")
    out
  }

  header_row <- detect_index(seq_len(nrow(cells)), \(r) sum(!is.na(as_month(cells[r, ]))) >= 12)
  if (header_row == 0) stop("Fann enga hausröð með dagsetningum í ", basename(xlsx))
  dates <- as_month(cells[header_row, ])
  date_cols <- which(!is.na(dates))

  # Heiti raðar = textinn í fyrstu tveimur dálkunum (heitið er í A eða B eftir skjali).
  labels <- apply(cells[, 1:2, drop = FALSE], 1, \(x) paste(na.omit(x), collapse = " "))

  find_row <- function(patterns, name) {
    at <- header_row
    for (p in patterns) {
      # stringr:: skýrt, því annar pakki í data_preparation.R skyggir á fixed().
      hit <- which(seq_along(labels) > at & str_detect(labels, stringr::fixed(p)))
      if (length(hit) == 0) {
        stop("Röð fannst ekki í ", basename(xlsx), " (", name, "): '", p, "'")
      }
      at <- hit[1]
    }
    at
  }

  values <- imap(rows, \(patterns, name) {
    as.numeric(cells[find_row(patterns, name), date_cols])
  })

  tibble(date = dates[date_cols], !!!values) |>
    filter(if_all(-date, \(x) !is.na(x))) |>
    arrange(date)
}


# 2.0.0 VEXTIR ----

# Meginvextir SÍ, daglegt gildi -> mánaðarmeðaltal sem brot (0,0825).
# Sama form og gamla blaðið "meginvextir": date | Meginvextir.
si_meginvextir <- function() {
  si_cached("meginvextir", \() {
    si_xml_series(17923, from = "2007-01-01") |>
      group_by(date = floor_date(date, "month")) |>
      summarise(Meginvextir = mean(value) / 100)
  })
}

# "Vextir bankanna" = almennir vextir skv. 10. gr. laga nr. 38/2001: SÍ birtir
# mánaðarlega vexti óverðtryggðra og verðtryggðra útlána (lægstu vextir á nýjum
# almennum útlánum lánastofnana) í "Tilkynningu um dráttarvexti og vexti af
# peningakröfum nr. MM/ÁÁÁÁ", sjá https://sedlabanki.is/gagnatorg/vextir/.
#
# Tilkynning nr. MM/ÁÁÁÁ er birt um 20. hvers mánaðar og gildir fyrir NÆSTA
# mánuð, en í gamla blaðinu var hún skráð á útgáfumánuðinn (t.d. nr. 07/2025 ->
# 2025-07-01). Sama venja er notuð hér.
#
# Eldri tilkynningar eru aðeins til sem PDF með óreglulegum slóðum, svo
# SAGAN er áfram lesin úr blaðinu "vextir" (frosið - ekki þarf lengur að bæta
# við það) og aðeins mánuðir á eftir síðustu útfylltu línu eru sóttir hér.
# Skilar date | Óverðtryggðir | Verðtryggðir í PRÓSENTUM (eins og blaðið).
si_almennir_vextir <- function(history_path) {
  saga <- read_excel(history_path, sheet = "vextir") |>
    mutate(date = as.Date(date)) |>
    filter(!is.na(Óverðtryggðir), !is.na(Verðtryggðir))

  get_notice <- function(m) {
    # Slóðin er stundum með núlli fremst (nr-07-2026) og stundum ekki (nr-7-2025).
    slugs <- unique(c(
      sprintf("nr-%02d-%d", month(m), year(m)),
      sprintf("nr-%d-%d", month(m), year(m))
    ))
    for (slug in slugs) {
      resp <- request(paste0(
        "https://sedlabanki.is/frettir-og-utgefid-efni/grein/",
        "tilkynning-um-drattarvexti-og-vexti-af-peningakrofum-", slug
      )) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform()
      if (resp_status(resp) != 200) next

      txt <- resp |>
        resp_body_string(encoding = "UTF-8") |>
        read_html() |>
        html_text2()
      rate <- function(label) {
        str_match(txt, paste0(label, "[^0-9]*([0-9]+,[0-9]+)"))[, 2] |>
          str_replace(",", ".") |>
          as.numeric()
      }
      out <- tibble(
        date = m,
        Óverðtryggðir = rate("[Vv]extir óverðtryggðra útlána"),
        Verðtryggðir = rate("[Vv]extir verðtryggðra útlána")
      )
      if (anyNA(out)) stop("Gat ekki lesið vexti úr tilkynningu ", slug)
      return(out)
    }
    # Tilkynning mánaðarins kemur ekki fyrr en um 20., en eldri mánuður á að vera til.
    if (m < floor_date(today(), "month")) {
      warning("Fann ekki vaxtatilkynningu SÍ fyrir ", format(m, "%m/%Y"), call. = FALSE)
    }
    NULL
  }

  first_new <- max(saga$date) %m+% months(1)
  this_month <- floor_date(today(), "month")
  new_months <- if (first_new <= this_month) {
    seq.Date(first_new, this_month, by = "month")
  } else {
    as.Date(character())
  }

  nyjar <- si_cached("almennir_vextir", \() map(new_months, get_notice) |> list_rbind())

  bind_rows(saga |> select(date, Óverðtryggðir, Verðtryggðir), nyjar) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)
}


# 3.0.0 INNGRIP SÍ Á GJALDEYRISMARKAÐ ----

# Dagleg velta á millibankamarkaði með gjaldeyri og viðskipti SÍ, m.kr.
# (XML hópur 8 "Velta á gjaldeyrismarkaði"):
#   284 = Heildarvelta á innlendum gjaldeyrismarkaði (ISK)  -> velta
#   287 = Gjaldeyriskaup SÍ í ISK                           -> kaup
#   285 = Gjaldeyrissala SÍ í ISK                           -> sala
# Skilar DAGLEGUM gildum: date | velta | kaup | sala (samantekt í mánuði er
# gerð í data_preparation.R eins og áður).
si_inngrip <- function() {
  si_cached("inngrip", \() {
    list(velta = 284, kaup = 287, sala = 285) |>
      imap(\(id, name) si_xml_series(id, from = "2009-01-01") |> rename(!!name := value)) |>
      reduce(full_join, by = "date") |>
      mutate(across(-date, \(x) replace_na(x, 0))) |>
      arrange(date)
  })
}


# 4.0.0 ÚTLÁN ----

# Útlán innlánsstofnana til heimila, staða (blað IV í "Útlán" skjali SÍ).
# M.kr., mánaðarlega frá desember 1997.
si_utlan_bankar <- function() {
  si_cached("utlan_bankar", \() {
    si_library_xlsx("FINSTATS.MONETARY.LOANS.EXCEL") |>
      si_read_wide(sheet = "IV", rows = list(
        banki_vtr = c("Verðtryggð útlán / Indexed loans", "Með veð í íbúð"),
        banki_ovtr = c("Önnur útlán og kröfur í íslenskum krónum", "Með veð í íbúð")
      ))
  })
}

# Ný útlán innlánsstofnana að frádregnum upp- og umframgreiðslum, íbúðalán
# heimila, skipt í breytilega / fasta vexti. M.kr., mánaðarlega frá janúar 2013.
si_ny_utlan_bankar <- function() {
  si_cached("ny_utlan_bankar", \() {
    si_library_xlsx("FINSTATS.MONETARY.NEWCREDIT.EXCEL") |>
      si_read_wide(sheet = "I", rows = list(
        br_ovt = c("Ný óverðtryggð útlán að frádregnum", "veði í íbúð - breytilegir"),
        fst_ovt = c("Ný óverðtryggð útlán að frádregnum", "veði í íbúð - fastir"),
        br_vt = c("Ný verðtryggð útlán að frádregnum", "veði í íbúð - breytilegir"),
        fst_vt = c("Ný verðtryggð útlán að frádregnum", "veði í íbúð - fastir")
      ))
  })
}

# Íbúðalán heimila, staða - sama uppsetning hjá lífeyrissjóðum og lánasjóðum
# ríkisins ("Útlán eftir geirum" á gagnabanki.is).
si_heimili_rows <- function(prefix) {
  set_names(
    list(
      c("Heimili / Households", "Verðtryggð útlán / Indexed", "veði í íbúð"),
      c("Heimili / Households", "Óverðtryggð útlán / Unindexed", "veði í íbúð")
    ),
    paste0(prefix, c("_vtr", "_ovtr"))
  )
}

# Lífeyrissjóðir, M.kr., mánaðarlega frá janúar 1997.
si_utlan_lif <- function() {
  si_cached("utlan_lif", \() {
    si_gagnabanki_xlsx("pension", report = "MARKETS.PENSIONFUNDS.LOANS.SECTOR.TABLE") |>
      si_read_wide(rows = si_heimili_rows("lif"))
  })
}

# Lánasjóðir ríkisins (m.a. ÍL-sjóður, HMS), M.kr., mánaðarlega frá mars 1992.
si_utlan_lanasjodir <- function() {
  si_cached("utlan_lanasjodir", \() {
    si_gagnabanki_xlsx("funds", report = "MARKETS.FUNDS.LOANS.SECTOR.TABLE") |>
      si_read_wide(rows = si_heimili_rows("lanasjodir"))
  })
}

# Ný útlán lífeyrissjóða til heimila að frádregnum upp- og umframgreiðslum,
# M.kr., mánaðarlega frá janúar 2009. Fyrsta "Verðtryggð/Óverðtryggð" röðin
# er fjárhæð; seinni (fjöldi lána) er ekki notuð.
si_ny_utlan_lif <- function() {
  si_cached("ny_utlan_lif", \() {
    si_gagnabanki_xlsx("pensionloans") |>
      si_read_wide(rows = list(
        vtr = "Verðtryggð útlán",
        ovtr = "Óverðtryggð útlán"
      ))
  })
}
