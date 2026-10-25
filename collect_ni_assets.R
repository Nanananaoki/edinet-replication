# EDINET API v2 から有報の「主要な経営指標等の推移」を取って
# 当期純利益と総資産を最大5年分まとめて集めるスクリプト
#
# ポイント
#   有報の最初のほうにある「主要な経営指標等の推移」には
#   純利益も総資産も5年分並んでいる。
#   なので1社につき有報を1本取れば、EPが4年分、ΔEPが3年分作れる。
#
# 必要パッケージ: install.packages(c("httr2", "xml2"))
# 使い方:
#   1. TEST_MODE <- TRUE のまま実行 → トヨタ・ソニー・ホンダだけで動作確認
#   2. うまくいったら TEST_MODE <- FALSE にして全社分を集める

library(httr2)
library(xml2)

# ---------------------------------------------------------------------------
# 設定
# ---------------------------------------------------------------------------
EDINET_API_KEY <- Sys.getenv("EDINET_API_KEY")
BASE_URL       <- "https://api.edinet-fsa.go.jp/api/v2"

TEST_MODE  <- TRUE
TEST_CODES <- c("E02144", "E01777", "E02166")   # トヨタ, ソニーG, ホンダ

# どの年の6月に出た有報を取るか。2017, 2021, 2024 の3回回せば
# 2012〜2023年度がすき間なくそろう
FILE_YEAR <- 2017

# 有報を探す提出日の範囲。3月決算の会社は6月下旬にまとめて出す
DATE_FROM <- as.Date(sprintf("%d-06-15", FILE_YEAR))
DATE_TO   <- as.Date(sprintf("%d-06-30", FILE_YEAR))

OUT_CSV <- if (TEST_MODE) sprintf("ni_assets_test_%d.csv", FILE_YEAR) else
                          sprintf("ni_assets_%d.csv", FILE_YEAR)

# ---------------------------------------------------------------------------
# API まわり
# ---------------------------------------------------------------------------
if (nchar(EDINET_API_KEY) == 0) {
  stop("EDINET_API_KEY が未設定です。.Renviron に EDINET_API_KEY=キー を書いてください。")
}

edinet_get <- function(path, ...) {
  resp <- request(paste0(BASE_URL, path)) |>
    req_url_query(`Subscription-Key` = EDINET_API_KEY, ...) |>
    req_timeout(120) |>
    req_retry(max_tries = 3, backoff = \(i) 5 * i) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  if (resp_status(resp) != 200) stop(sprintf("HTTP %d: %s", resp_status(resp), path))
  resp
}

# ---------------------------------------------------------------------------
# Step 1: 期間内の有報の一覧を作る
# ---------------------------------------------------------------------------
list_yuho <- function(from, to) {
  out <- list()
  for (d in as.character(seq(from, to, by = "day"))) {
    res <- tryCatch(resp_body_json(edinet_get("/documents.json", date = d, type = 2))$results,
                    error = function(e) { cat("  失敗:", d, conditionMessage(e), "\n"); NULL })
    for (doc in res) {
      if (identical(doc$ordinanceCode, "010") && identical(doc$formCode, "030000") &&
          !is.null(doc$edinetCode) && identical(doc$xbrlFlag, "1")) {
        out[[length(out) + 1]] <- data.frame(
          doc_id = doc$docID, edinet_code = doc$edinetCode,
          sec_code = if (is.null(doc$secCode)) NA else doc$secCode,
          filer = doc$filerName, submit_date = d,
          period_end = if (is.null(doc$periodEnd)) NA else doc$periodEnd)
      }
    }
    cat(sprintf("  %s: 累計 %d 件\n", d, length(out)))
    Sys.sleep(1)
  }
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Step 2: ZIP を取ってきて有報本体の XBRL を読み込む
# ---------------------------------------------------------------------------
read_main_xbrl <- function(doc_id) {
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile()
  on.exit(unlink(c(tmp_zip, tmp_dir), recursive = TRUE))

  writeBin(resp_body_raw(edinet_get(sprintf("/documents/%s", doc_id), type = 1)), tmp_zip)
  files  <- unzip(tmp_zip, list = TRUE)$Name
  target <- files[grepl("PublicDoc/.*jpcrp030000-asr.*\\.xbrl$", files)]
  if (length(target) == 0) stop("有報本体の XBRL が見つからない")
  unzip(tmp_zip, files = target[1], exdir = tmp_dir)
  read_xml(file.path(tmp_dir, target[1]))
}

# ---------------------------------------------------------------------------
# Step 3: 純利益と総資産を5年分取り出す
# ---------------------------------------------------------------------------
# タグ名を正規表現で、contextRef を完全一致で探して数値を返す
get_val <- function(doc, tag_regex, context) {
  nodes <- xml_find_all(doc, sprintf("//*[@contextRef='%s']", context))
  hit   <- nodes[grepl(tag_regex, xml_name(nodes))]
  if (length(hit) == 0) return(NA_real_)
  txt <- trimws(xml_text(hit[1]))
  if (xml_attr(hit[1], "nil", default = "false") == "true" || txt == "") return(NA_real_)
  as.numeric(txt)
}

get_dei <- function(doc, local) {
  hit <- xml_find_first(doc, sprintf("//*[local-name()='%s']", local))
  if (inherits(hit, "xml_missing")) NA else trimws(xml_text(hit))
}

# 日本基準・IFRS・米国基準のどれでも拾えるようにしたタグ名のパターン
NI_REGEX <- "(^|:)(ProfitLossAttributableToOwnersOfParent|NetIncomeLossAttributableToOwnersOfParent)(IFRS|USGAAP)?SummaryOfBusinessResults$"
NI_SOLO  <- "(^|:)NetIncomeLossSummaryOfBusinessResults$"   # 連結がない会社用
TA_REGEX <- "(^|:)TotalAssets(IFRS|USGAAP)?SummaryOfBusinessResults$"

extract_5y <- function(doc) {
  periods <- c("CurrentYear", paste0("Prior", 1:4, "Year"))
  ni <- sapply(periods, \(p) get_val(doc, NI_REGEX, paste0(p, "Duration")))

  # 2015年度より前の日本基準では、連結でも「当期純利益」という名前だった。
  # 古い有報で「親会社株主に帰属する〜」が空いてる年はこっちで埋める
  old <- is.na(ni)
  if (any(old) && !all(old)) {
    ni[old] <- sapply(periods[old], \(p) get_val(doc, NI_SOLO, paste0(p, "Duration")))
  } else if (all(old)) {
    ni <- sapply(periods, \(p) get_val(doc, NI_SOLO, paste0(p, "Duration")))
  }
  ta <- sapply(periods, \(p) get_val(doc, TA_REGEX, paste0(p, "Instant")))
  basis <- "連結"

  # 連結の数字がない会社は単体の数字を使う
  if (all(is.na(ni))) {
    ni <- sapply(periods, \(p) get_val(doc, NI_SOLO, paste0(p, "Duration_NonConsolidatedMember")))
    ta <- sapply(periods, \(p) get_val(doc, "(^|:)TotalAssetsSummaryOfBusinessResults$",
                                       paste0(p, "Instant_NonConsolidatedMember")))
    basis <- "単体"
  }

  row <- data.frame(
    accounting_std = get_dei(doc, "AccountingStandardsDEI"),
    fy_end         = get_dei(doc, "CurrentFiscalYearEndDateDEI"),
    basis          = basis)
  for (k in 0:4) {
    row[[paste0("NI_t", k)]] <- ni[k + 1]
    row[[paste0("TA_t", k)]] <- ta[k + 1]
  }
  row
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
cat("=== Step 1: 有報の一覧を作成 ===\n")
docs <- list_yuho(DATE_FROM, DATE_TO)
if (TEST_MODE) docs <- docs[docs$edinet_code %in% TEST_CODES, ]
cat(sprintf("対象: %d 件\n", nrow(docs)))

# 途中で止まっても続きからやれるように、取得済みの docID は飛ばす
done <- if (file.exists(OUT_CSV)) read.csv(OUT_CSV)$doc_id else character(0)
docs <- docs[!docs$doc_id %in% done, ]

cat("=== Step 2-3: XBRL から数字を取り出す ===\n")
for (i in seq_len(nrow(docs))) {
  d <- docs[i, ]
  cat(sprintf("[%d/%d] %s %s ... ", i, nrow(docs), d$edinet_code, d$filer))
  row <- tryCatch(cbind(d, extract_5y(read_main_xbrl(d$doc_id))),
                  error = function(e) { cat("失敗:", conditionMessage(e), "\n"); NULL })
  if (!is.null(row)) {
    write.table(row, OUT_CSV, sep = ",", row.names = FALSE,
                col.names = !file.exists(OUT_CSV), append = file.exists(OUT_CSV),
                fileEncoding = "UTF-8")
    cat(sprintf("OK 純利益=%s 総資産=%s\n",
                format(row$NI_t0, big.mark = ","), format(row$TA_t0, big.mark = ",")))
  }
  Sys.sleep(2)
}
cat(sprintf("\n完了 → %s\n", OUT_CSV))
