# 中間発表のあとに言われた3つをまとめて作るスクリプト
#
#   1. 元論文 BD1997 のグラフと比べられる形の図と比較表
#   2. コード導入前後の比較。前後で凹み具合に差があるかの検定つき
#   3. サンプルをどう絞ったかの表と、データの信頼性チェック用の資料
#
# 必要なもの  compare_pre_post.R と同じ
#   ni_assets_2017.csv, ni_assets_2021.csv, ni_assets_2024.csv, EdinetcodeDlInfo.csv
#
# 出てくるもの  out_midterm フォルダにまとめて保存
#   表1_サンプル選択.csv        どの段階で何件減ったか
#   表2_記述統計.csv            EP と ΔEP の平均や中央値など
#   表3_BD1997との比較.csv      BD1997 の欄は論文を見て自分で埋める
#   表4_導入前後の検定.csv      前後で凹み具合が変わったかの検定
#   表5_抜き取り確認リスト.csv  有報と手で照らし合わせる用
#   図1_利益水準_全期間.png     BD1997 の図と同じビン幅・範囲・縦軸が件数
#   図2_利益変化_全期間.png
#   CSV は Excel でそのまま開いても文字化けしない形で保存してる

library(ggplot2)

IN_FILES  <- c("ni_assets_2017.csv", "ni_assets_2021.csv", "ni_assets_2024.csv")
CODE_LIST <- "EdinetcodeDlInfo.csv"
REFORM_FY <- 2015
OUT_DIR   <- "out_midterm"
DROP_INDUSTRY <- c("銀行業", "保険業", "証券、商品先物取引業", "その他金融業", "電気・ガス業")

EP_WIDTH  <- 0.005;  EP_RANGE  <- c(-0.25, 0.35)
DEP_WIDTH <- 0.0025; DEP_RANGE <- c(-0.15, 0.15)

FONT <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else
        if (.Platform$OS.type == "windows") "Yu Gothic" else ""

dir.create(OUT_DIR, showWarnings = FALSE)

# Excel で開いても文字化けしないように、先頭に目印をつけて保存する
save_csv <- function(df, name) {
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  writeBin(c(bom, readBin(tmp, "raw", file.size(tmp))), file.path(OUT_DIR, name))
  unlink(tmp)
}

# ===========================================================================
# 3. サンプルの絞り込み。段階ごとに件数を記録しながら進める
# ===========================================================================
steps <- list()
add_step <- function(label, n_rows, n_firms, unit) {
  steps[[length(steps) + 1]] <<- data.frame(
    段階 = label, 単位 = unit, 件数 = n_rows, 企業数 = n_firms)
}

files <- IN_FILES[file.exists(IN_FILES)]
if (length(files) < length(IN_FILES)) warning("見つからないファイルがあります")
raw <- do.call(rbind, lapply(files, function(f) {
  d <- read.csv(f, fileEncoding = "UTF-8")
  d$file_year <- as.integer(gsub("\\D", "", f))
  d[, c("file_year", "doc_id", "edinet_code", "sec_code", "filer",
        "accounting_std", "basis", "fy_end", paste0(rep(c("NI_t", "TA_t"), 5), rep(0:4, each = 2)))]
}))

for (y in sort(unique(raw$file_year))) {
  add_step(sprintf("%d年6月提出の有報で数字が取れたもの", y),
           sum(raw$file_year == y), length(unique(raw$edinet_code[raw$file_year == y])), "有報")
}
add_step("3年分の合計", nrow(raw), length(unique(raw$edinet_code)), "有報")

raw <- raw[!is.na(raw$sec_code), ]
add_step("提出時点で証券コードがある、つまり上場している", nrow(raw), length(unique(raw$edinet_code)), "有報")

fy_end <- as.Date(raw$fy_end)
raw <- raw[!is.na(fy_end) & format(fy_end, "%m") == "03", ]
add_step("3月決算", nrow(raw), length(unique(raw$edinet_code)), "有報")

if (file.exists(CODE_LIST)) {
  cl <- read.csv(CODE_LIST, skip = 1, fileEncoding = "CP932")
  industry <- setNames(cl[[11]], cl[[1]])
  raw$industry <- industry[raw$edinet_code]
  n_unknown <- length(unique(raw$edinet_code[is.na(raw$industry)]))
  raw <- raw[!raw$industry %in% DROP_INDUSTRY, ]
  add_step("金融と電力・ガスを除く", nrow(raw), length(unique(raw$edinet_code)), "有報")
} else {
  raw$industry <- NA
  n_unknown <- NA
  warning("EdinetcodeDlInfo.csv がないので業種で外していません")
}

# 有報1本 → 5年分の「企業・年度」に広げる
base_fy <- as.integer(format(as.Date(raw$fy_end), "%Y")) - 1
panel <- do.call(rbind, lapply(0:4, function(k) data.frame(
  edinet_code = raw$edinet_code, filer = raw$filer, industry = raw$industry,
  accounting_std = raw$accounting_std, basis = raw$basis, file_year = raw$file_year,
  fy = base_fy - k, NI = raw[[paste0("NI_t", k)]], TA = raw[[paste0("TA_t", k)]], k = k)))
add_step("5年分に広げた企業・年度", nrow(panel), length(unique(panel$edinet_code)), "企業・年度")

panel <- panel[order(panel$edinet_code, panel$fy, panel$k), ]
panel <- panel[!duplicated(panel[, c("edinet_code", "fy")]), ]
add_step("同じ年度が2本の有報に載ってる分を除く", nrow(panel), length(unique(panel$edinet_code)), "企業・年度")

# EP と ΔEP を計算
key   <- paste(panel$edinet_code, panel$fy)
prev1 <- match(paste(panel$edinet_code, panel$fy - 1), key)
prev2 <- match(paste(panel$edinet_code, panel$fy - 2), key)
panel$TA_prev1 <- panel$TA[prev1]
panel$TA_prev2 <- panel$TA[prev2]
panel$NI_prev1 <- panel$NI[prev1]
panel$EP  <- panel$NI / panel$TA_prev1
panel$DEP <- (panel$NI - panel$NI_prev1) / panel$TA_prev2
panel$period <- factor(ifelse(panel$fy < REFORM_FY, "導入前", "導入後"), levels = c("導入前", "導入後"))

ep_ok  <- is.finite(panel$EP)  & panel$TA_prev1 > 0
dep_ok <- is.finite(panel$DEP) & panel$TA_prev2 > 0
add_step("EP が計算できる。純利益と前期末総資産がそろってる", sum(ep_ok),
         length(unique(panel$edinet_code[ep_ok])), "企業・年度")
ep_in <- ep_ok & panel$EP >= EP_RANGE[1] & panel$EP < EP_RANGE[2]
add_step(sprintf("EP が %s 以上 %s 未満。図と検定に使う最終サンプル", EP_RANGE[1], EP_RANGE[2]),
         sum(ep_in), length(unique(panel$edinet_code[ep_in])), "企業・年度")
add_step("ΔEP が計算できる。前期純利益と前々期末総資産もそろってる", sum(dep_ok),
         length(unique(panel$edinet_code[dep_ok])), "企業・年度")
dep_in <- dep_ok & panel$DEP >= DEP_RANGE[1] & panel$DEP < DEP_RANGE[2]
add_step(sprintf("ΔEP が %s 以上 %s 未満。図と検定に使う最終サンプル", DEP_RANGE[1], DEP_RANGE[2]),
         sum(dep_in), length(unique(panel$edinet_code[dep_in])), "企業・年度")

tab1 <- do.call(rbind, steps)
cat("\n===== 表1 サンプル選択 =====\n"); print(tab1, row.names = FALSE)
cat(sprintf("\n業種がコードリストに見つからず、残している会社: %s 社\n", n_unknown))
cat(sprintf("EP の年度: %s〜%s  ΔEP の年度: %s〜%s\n",
            min(panel$fy[ep_ok]), max(panel$fy[ep_ok]), min(panel$fy[dep_ok]), max(panel$fy[dep_ok])))
save_csv(tab1, "表1_サンプル選択.csv")

ep  <- data.frame(panel[ep_in,  c("edinet_code", "fy", "period")], value = panel$EP[ep_in])
dep <- data.frame(panel[dep_in, c("edinet_code", "fy", "period")], value = panel$DEP[dep_in])

# ===========================================================================
# 記述統計
# ===========================================================================
describe <- function(x, name, period) {
  q <- quantile(x, c(0.01, 0.25, 0.5, 0.75, 0.99))
  data.frame(変数 = name, 期間 = period, 件数 = length(x),
             平均 = mean(x), 標準偏差 = sd(x), 下位1パーセント = q[1], 第1四分位 = q[2],
             中央値 = q[3], 第3四分位 = q[4], 上位1パーセント = q[5],
             マイナスの割合 = mean(x < 0), row.names = NULL)
}
tab2 <- do.call(rbind, c(
  lapply(c("全期間", "導入前", "導入後"), function(p) {
    x <- if (p == "全期間") panel$EP[ep_ok] else panel$EP[ep_ok & panel$period == p]
    describe(x, "EP", p) }),
  lapply(c("全期間", "導入前", "導入後"), function(p) {
    x <- if (p == "全期間") panel$DEP[dep_ok] else panel$DEP[dep_ok & panel$period == p]
    describe(x, "ΔEP", p) })))
tab2[, 4:11] <- round(tab2[, 4:11], 4)
cat("\n===== 表2 記述統計  範囲で切る前の全データ =====\n"); print(tab2, row.names = FALSE)
save_csv(tab2, "表2_記述統計.csv")

# ===========================================================================
# BD1997 の標準化差
# ===========================================================================
bin_counts <- function(values, width, range) {
  breaks <- seq(range[1], range[2], by = width)
  data.frame(bin_from = head(breaks, -1),
             n = as.vector(table(cut(values, breaks, right = FALSE))))
}
sd_at <- function(values, width, range, which = c("below", "above")) {
  which <- match.arg(which)
  b <- bin_counts(values, width, range); N <- sum(b$n); p <- b$n / N
  i <- if (which == "below") which(abs(b$bin_from + width) < width / 2) else
                             which(abs(b$bin_from) < width / 2)
  expected <- (b$n[i - 1] + b$n[i + 1]) / 2
  variance <- N * p[i] * (1 - p[i]) + N * (p[i - 1] + p[i + 1]) * (1 - p[i - 1] - p[i + 1]) / 4
  list(actual = b$n[i], expected = expected, left = b$n[i - 1], right = b$n[i + 1],
       sd = (b$n[i] - expected) / sqrt(variance))
}

# ===========================================================================
# 1. BD1997 との比較表。BD1997 の数字は論文を見て埋める
# ===========================================================================
summ <- function(df, width, range) {
  lo <- sd_at(df$value, width, range, "below"); hi <- sd_at(df$value, width, range, "above")
  c(件数 = nrow(df), すぐ下_実際 = lo$actual, すぐ下_予想 = lo$expected,
    すぐ下_実際割る予想 = round(lo$actual / lo$expected, 2),
    すぐ下_標準化差 = round(lo$sd, 2), すぐ上_標準化差 = round(hi$sd, 2))
}
s_ep  <- summ(ep,  EP_WIDTH,  EP_RANGE)
s_dep <- summ(dep, DEP_WIDTH, DEP_RANGE)
tab3 <- data.frame(
  項目 = c("対象", "期間", "利益の数字", "分母", "利益水準 ビン幅", "利益変化 ビン幅",
           "利益水準 件数", "利益水準 ゼロのすぐ下の標準化差", "利益水準 ゼロのすぐ上の標準化差",
           "利益変化 件数", "利益変化 ゼロのすぐ下の標準化差", "利益変化 ゼロのすぐ上の標準化差"),
  BD1997 = c("論文から記入", "論文から記入", "論文から記入", "期首の時価総額",
             EP_WIDTH, DEP_WIDTH, "", "", "", "", "", ""),
  本研究 = c("国内上場の3月決算企業 金融・電力ガス除く",
            sprintf("%d〜%d年度", min(ep$fy), max(ep$fy)),
            "親会社株主に帰属する当期純利益 連結がなければ単体",
            "期首の総資産 ΔEP は前々期末の総資産",
            EP_WIDTH, DEP_WIDTH,
            s_ep["件数"], s_ep["すぐ下_標準化差"], s_ep["すぐ上_標準化差"],
            s_dep["件数"], s_dep["すぐ下_標準化差"], s_dep["すぐ上_標準化差"]))
cat("\n===== 表3 BD1997 との比較  BD1997 の空欄は論文から埋める =====\n"); print(tab3, row.names = FALSE)
save_csv(tab3, "表3_BD1997との比較.csv")

# 元論文と同じ見た目の図。ビン幅と表示範囲をそろえて、縦軸は件数のまま
bd_style <- function(df, width, range, xlab, title) {
  b <- bin_counts(df$value, width, range)
  b$just_below_zero <- abs(b$bin_from + width) < width / 2
  ggplot(b, aes(bin_from + width / 2, n, fill = just_below_zero)) +
    geom_col(width = width * 0.9) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey30") +
    scale_fill_manual(values = c(`FALSE` = "grey65", `TRUE` = "grey15"), guide = "none") +
    scale_x_continuous(limits = range, breaks = seq(range[1], range[2], by = 0.05),
                       labels = function(x) sprintf("%.2f", x)) +
    labs(x = xlab, y = "件数", title = title,
         subtitle = sprintf("%d〜%d年度  n = %s  ビン幅 %s  黒い棒がゼロのすぐ下",
                            min(df$fy), max(df$fy), format(nrow(df), big.mark = ","), width)) +
    theme_classic(base_size = 11, base_family = FONT) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
}
ggsave(file.path(OUT_DIR, "図1_利益水準_全期間.png"),
       bd_style(ep, EP_WIDTH, EP_RANGE, "当期純利益 ÷ 期首総資産", "利益水準の分布"),
       width = 8, height = 4.5, dpi = 200)
ggsave(file.path(OUT_DIR, "図2_利益変化_全期間.png"),
       bd_style(dep, DEP_WIDTH, DEP_RANGE, "純利益の変化 ÷ 前々期末総資産", "利益変化の分布"),
       width = 8, height = 4.5, dpi = 200)

# ===========================================================================
# 2. 導入前後の検定
#
#   ゼロのすぐ下の棒と、その両隣の棒、合わせて3本だけを見る。
#   分布がなめらかなら、3本の中でまん中の棒が占める割合はだいたい3分の1。
#   赤字回避があるとまん中が少なくなる。
#   この「まん中の割合」が導入前と導入後で違うかを、カイ二乗検定で確かめる。
#   件数の多さに左右されないので、標準化差をそのまま比べるより公平。
# ===========================================================================
pre_post_test <- function(df, width, range, name) {
  rows <- lapply(levels(df$period), function(p) {
    s <- sd_at(df$value[df$period == p], width, range, "below")
    c(below = s$actual, neighbors = s$left + s$right)
  })
  m <- do.call(rbind, rows); rownames(m) <- levels(df$period)
  test <- suppressWarnings(chisq.test(m, correct = FALSE))
  data.frame(変数 = name, 期間 = rownames(m),
             すぐ下の件数 = m[, "below"], 両隣の件数 = m[, "neighbors"],
             まん中の割合 = round(m[, "below"] / rowSums(m), 3),
             なめらかなら = round(1 / 3, 3),
             カイ二乗値 = round(test$statistic, 2), p値 = round(test$p.value, 3),
             row.names = NULL)
}
tab4 <- rbind(pre_post_test(ep,  EP_WIDTH,  EP_RANGE,  "EP"),
              pre_post_test(dep, DEP_WIDTH, DEP_RANGE, "ΔEP"))
cat("\n===== 表4 導入前後の検定 =====\n"); print(tab4, row.names = FALSE)
cat("p値が0.05より小さければ、前後で凹み具合が変わったと言える\n")
save_csv(tab4, "表4_導入前後の検定.csv")

# ===========================================================================
# 3. 抜き取り確認リスト
#   ランダムに選んだ企業・年度を、実際の有報と手で照らし合わせる用。
#   赤字のケースも必ず入るように、プラスとマイナスから半分ずつ選ぶ
# ===========================================================================
set.seed(2015)
cand <- panel[ep_in, ]
pick <- c(sample(which(cand$NI >= 0), 7), sample(which(cand$NI < 0), 3))
tab5 <- cand[pick, c("edinet_code", "filer", "fy", "file_year", "accounting_std", "basis",
                     "NI", "TA_prev1", "EP")]
tab5$NI_百万円       <- round(tab5$NI / 1e6)
tab5$前期末総資産_百万円 <- round(tab5$TA_prev1 / 1e6)
tab5$EP <- round(tab5$EP, 4)
tab5$確認結果 <- ""
tab5 <- tab5[order(tab5$fy), c("edinet_code", "filer", "fy", "file_year", "accounting_std",
                              "basis", "NI_百万円", "前期末総資産_百万円", "EP", "確認結果")]
names(tab5)[1:6] <- c("EDINETコード", "会社名", "年度", "数字を取った有報", "会計基準", "連結か単体か")
cat("\n===== 表5 抜き取り確認リスト =====\n"); print(tab5, row.names = FALSE)
save_csv(tab5, "表5_抜き取り確認リスト.csv")

cat(sprintf("\n全部 %s フォルダに保存しました\n", OUT_DIR))
