# EDINET API v2 - トヨタ自動車の有価証券報告書から当期純利益・発行済株式数を取得
# 必要パッケージ: httr2, xml2
# install.packages(c("httr2", "xml2"))

library(httr2)
library(xml2)

# ---------------------------------------------------------------------------
# 設定
# ---------------------------------------------------------------------------
EDINET_API_KEY  <- Sys.getenv("EDINET_API_KEY")
BASE_URL        <- "https://api.edinet-fsa.go.jp/api/v2"
TOYOTA_EDINET   <- "E02144"

# 3月期決算 → 有報提出は6月頃。複数日付を候補にする
CANDIDATE_DATES <- c(
  "2024-06-25", "2024-06-24", "2024-06-26", "2024-06-27", "2024-06-28",
  "2023-06-26", "2023-06-27", "2023-06-28"
)

# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------
check_api_key <- function() {
  if (nchar(EDINET_API_KEY) == 0) {
    stop(paste(
      "環境変数 EDINET_API_KEY が設定されていません。\n",
      "  Sys.setenv(EDINET_API_KEY = 'your_api_key')  または\n",
      "  .Renviron に EDINET_API_KEY=your_key を追記してください。"
    ))
  }
}

edinet_get <- function(path, ...) {
  req <- request(paste0(BASE_URL, path)) |>
    req_url_query(`Subscription-Key` = EDINET_API_KEY, ...) |>
    req_timeout(120) |>
    req_error(is_error = \(resp) FALSE)   # エラーも手動でハンドル

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    cat(sprintf("[ERROR] %s → HTTP %d\n", path, resp_status(resp)))
    cat(substr(resp_body_string(resp), 1, 300), "\n")
    stop("API request failed")
  }
  resp
}

# ---------------------------------------------------------------------------
# Step 1: docID を特定
# ---------------------------------------------------------------------------
find_toyota_doc_id <- function() {
  cat("=== トヨタ自動車の有報 docID を検索中 ===\n")

  for (d in CANDIDATE_DATES) {
    cat(sprintf("\n[検索] 提出日: %s\n", d))

    resp <- tryCatch(
      edinet_get("/documents.json", date = d, type = 2),
      error = function(e) { cat("  リクエスト失敗:", conditionMessage(e), "\n"); NULL }
    )
    if (is.null(resp)) { Sys.sleep(3); next }

    body    <- resp_body_json(resp)
    results <- body$results

    if (length(results) == 0) {
      cat(sprintf("  該当なし（0件）\n"))
      Sys.sleep(3)
      next
    }

    for (doc in results) {
      if (
        identical(doc$edinetCode,    TOYOTA_EDINET) &&
        identical(doc$ordinanceCode, "010")         &&
        identical(doc$formCode,      "030000")
      ) {
        cat(sprintf("  [発見] filerName=%s  docID=%s\n", doc$filerName, doc$docID))
        return(list(doc_id = doc$docID, submit_date = d))
      }
    }

    cat(sprintf("  該当なし（%d件中）\n", length(results)))
    Sys.sleep(3)
  }

  stop("対象期間内にトヨタ自動車の有報が見つかりませんでした。CANDIDATE_DATES を調整してください。")
}

# ---------------------------------------------------------------------------
# Step 2: XBRL ZIP をダウンロード
# ---------------------------------------------------------------------------
download_xbrl_zip <- function(doc_id) {
  cat(sprintf("\n=== XBRL ダウンロード (docID=%s) ===\n", doc_id))

  resp <- edinet_get(sprintf("/documents/%s", doc_id), type = 1)

  raw_bytes <- resp_body_raw(resp)
  cat(sprintf("  ダウンロード完了: %.1f KB\n", length(raw_bytes) / 1024))
  raw_bytes
}

# ---------------------------------------------------------------------------
# Step 3: ZIP を解凍して XBRL バイト列を返す
# ---------------------------------------------------------------------------
extract_xbrl_from_zip <- function(raw_bytes) {
  cat("\n=== ZIP を解凍して XBRL ファイルを探す ===\n")

  tmp_zip  <- tempfile(fileext = ".zip")
  tmp_dir  <- tempdir()
  on.exit(unlink(tmp_zip))

  writeBin(raw_bytes, tmp_zip)
  all_files <- unzip(tmp_zip, list = TRUE)$Name
  cat(sprintf("  ZIP 内ファイル数: %d\n", length(all_files)))

  xbrl_files <- all_files[grepl("\\.xbrl$", all_files, ignore.case = TRUE)]
  cat(sprintf("  XBRL ファイル一覧:\n"))
  cat(paste("   ", xbrl_files, collapse = "\n"), "\n")

  if (length(xbrl_files) == 0) {
    cat("ZIP 内容:\n"); print(head(all_files, 20))
    stop("XBRL ファイルが見つかりませんでした。")
  }

  # PublicDoc 配下の有価証券報告書本体を優先。AuditDoc（監査報告書）は除外する。
  public_xbrl <- xbrl_files[
    grepl("PublicDoc", xbrl_files) &
    !grepl("AuditDoc", xbrl_files) &
    grepl("jpcrp030000-asr-001", basename(xbrl_files))
  ]

  if (length(public_xbrl) == 0) {
    # jpcrp030000-asr-001 に限定せず PublicDoc 配下全体にフォールバック
    public_xbrl <- xbrl_files[
      grepl("PublicDoc", xbrl_files) & !grepl("AuditDoc", xbrl_files)
    ]
  }

  target <- if (length(public_xbrl) > 0) public_xbrl[1] else xbrl_files[1]
  cat(sprintf("  使用する XBRL: %s\n", target))

  unzip(tmp_zip, files = target, exdir = tmp_dir, overwrite = TRUE)
  readBin(file.path(tmp_dir, target), "raw", n = 100e6)
}

# ---------------------------------------------------------------------------
# Step 4: XBRL をパースして値を抽出
# ---------------------------------------------------------------------------
parse_xbrl <- function(xbrl_bytes) {
  cat("\n=== XBRL をパース ===\n")

  doc  <- read_xml(xbrl_bytes)
  ns   <- xml_ns(doc)                      # 名前空間一覧
  all_nodes <- xml_find_all(doc, "//*")    # 全要素

  # ローカル名（名前空間プレフィックスなし）を取得
  local_names <- xml_name(all_nodes, ns)   # "prefix:localname" 形式
  # ":" 以降だけ取り出す
  local_only  <- sub("^[^:]+:", "", local_names)

  # --- タグ候補を表示 -------------------------------------------------------
  profit_kw <- c("NetIncomeLoss", "ProfitLoss", "NetIncome", "profit", "loss", "Income")
  shares_kw <- c("NumberOfIssuedShares", "IssuedShares", "TotalNumberOfIssued", "Shares", "shares")

  profit_tags <- unique(local_only[Reduce(`|`, lapply(profit_kw, grepl, x = local_only, ignore.case = TRUE))])
  shares_tags <- unique(local_only[Reduce(`|`, lapply(shares_kw, grepl, x = local_only, ignore.case = TRUE))])

  cat("\n--- 純利益関連タグ候補 ---\n")
  cat(paste(" ", sort(profit_tags), collapse = "\n"), "\n")

  cat("\n--- 発行済株式数関連タグ候補 ---\n")
  cat(paste(" ", sort(shares_tags), collapse = "\n"), "\n")

  # --- 値抽出ヘルパー -------------------------------------------------------
  # context_prefer: contextRef に含まれるべき文字列（優先順）
  # context_exclude: contextRef に含まれていたら除外する文字列
  find_value <- function(keywords, context_prefer, context_exclude = character(0)) {

    node_to_record <- function(node) {
      txt <- trimws(xml_text(node))
      if (nchar(txt) == 0 || !grepl("^-?[0-9]+$", txt)) return(NULL)
      list(
        tag      = xml_name(node),
        value    = as.numeric(txt),
        context  = xml_attr(node, "contextRef"),
        decimals = xml_attr(node, "decimals"),
        unit_ref = xml_attr(node, "unitRef")
      )
    }

    for (kw in keywords) {
      xpath   <- sprintf("//*[contains(local-name(), '%s')]", kw)
      matches <- xml_find_all(doc, xpath)

      # 数値を持つ候補を全部収集
      candidates <- Filter(Negate(is.null), lapply(matches, node_to_record))
      if (length(candidates) == 0) next

      # 候補を全表示
      cat(sprintf("\n  [候補一覧: %s]\n", kw))
      for (r in candidates) {
        cat(sprintf("    contextRef=%-55s value=%s\n",
                    r$context, format(r$value, big.mark = ",")))
      }

      # 除外フィルタ
      if (length(context_exclude) > 0) {
        candidates <- Filter(
          function(r) !any(sapply(context_exclude, grepl, x = r$context)),
          candidates
        )
      }

      # 優先 contextRef を順番に試す
      for (pref in context_prefer) {
        hit <- Filter(function(r) grepl(pref, r$context), candidates)
        if (length(hit) > 0) return(hit[[1]])
      }

      # 優先パターンに合うものがなければ残った候補の先頭を返す
      if (length(candidates) > 0) return(candidates[[1]])
    }
    NULL
  }

  # --- 当期純利益 -----------------------------------------------------------
  cat("\n=== 抽出結果 ===\n")

  profit_priority <- c(
    "ProfitLossAttributableToOwnersOfParent",
    "NetIncomeLoss",
    "ProfitLoss",
    "NetIncome"
  )
  # CurrentYearDuration（連結・当期）を最優先。Prior系は除外しない（フォールバック用に残す）
  profit_context_prefer  <- c("CurrentYearDuration")
  profit_context_exclude <- character(0)
  res_profit <- find_value(profit_priority, profit_context_prefer, profit_context_exclude)

  if (!is.null(res_profit)) {
    cat(sprintf("\n当期純利益:\n"))
    cat(sprintf("  タグ名     : %s\n",   res_profit$tag))
    cat(sprintf("  contextRef : %s\n",   res_profit$context))
    cat(sprintf("  値（原単位）: %s\n",  format(res_profit$value, big.mark = ",")))
    cat(sprintf("  decimals   : %s\n",   res_profit$decimals))
    cat(sprintf("  unitRef    : %s\n",   res_profit$unit_ref))

    dec <- res_profit$decimals
    val <- res_profit$value
    if (!is.na(dec) && dec == "-6") {
      cat(sprintf("\n当期純利益：%s 百万円\n", format(val, big.mark = ",")))
    } else if (!is.na(dec) && dec == "0") {
      cat(sprintf("\n当期純利益：%s 百万円（%s 円換算）\n",
                  format(val / 1e6, big.mark = ","), format(val, big.mark = ",")))
    } else if (!is.na(dec) && dec == "-3") {
      cat(sprintf("\n当期純利益：%s 百万円（%s 千円換算）\n",
                  format(val / 1e3, big.mark = ","), format(val, big.mark = ",")))
    } else {
      cat(sprintf("\n当期純利益：%s（単位は decimals=%s を確認）\n",
                  format(val, big.mark = ","), dec))
    }
  } else {
    cat("\n当期純利益: タグが見つかりませんでした。\n")
    cat("上記「純利益関連タグ候補」からタグ名を確認してください。\n")
  }

  # --- 発行済株式数 ---------------------------------------------------------
  shares_priority <- c(
    "NumberOfIssuedSharesSummaryOfBusinessResults",
    "TotalNumberOfIssuedShares",
    "NumberOfIssuedShares",
    "IssuedShares"
  )
  # CurrentYearInstant を優先。"_NonConsolidatedMember" サフィックス付きは後回し
  shares_context_prefer  <- c("^CurrentYearInstant$", "CurrentYearInstant")
  shares_context_exclude <- character(0)
  res_shares <- find_value(shares_priority, shares_context_prefer, shares_context_exclude)

  if (!is.null(res_shares)) {
    cat(sprintf("\n発行済株式数:\n"))
    cat(sprintf("  タグ名     : %s\n",  res_shares$tag))
    cat(sprintf("  contextRef : %s\n",  res_shares$context))
    cat(sprintf("  値         : %s 株\n", format(res_shares$value, big.mark = ",")))
    cat(sprintf("\n発行済株式数：%s 株\n", format(res_shares$value, big.mark = ",")))
  } else {
    cat("\n発行済株式数: タグが見つかりませんでした。\n")
    cat("上記「発行済株式数関連タグ候補」からタグ名を確認してください。\n")
  }
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
check_api_key()

result      <- find_toyota_doc_id()
Sys.sleep(4)

zip_bytes   <- download_xbrl_zip(result$doc_id)
Sys.sleep(3)

xbrl_bytes  <- extract_xbrl_from_zip(zip_bytes)

parse_xbrl(xbrl_bytes)
