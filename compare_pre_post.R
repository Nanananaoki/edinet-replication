# 2017・2021・2024年の有報データをまとめて、
# コーポレートガバナンス・コード導入の前後で利益分布を比べるスクリプト
#
#   導入前 = 2014年度まで   導入後 = 2015年度から
#   EP  = 当期純利益 ÷ 前期末総資産
#   ΔEP = (当期純利益 − 前期純利益) ÷ 前々期末総資産
#
# 必要なもの
#   ni_assets_2017.csv, ni_assets_2021.csv, ni_assets_2024.csv
#   EdinetcodeDlInfo.csv   業種で金融と電力・ガスを外すのに使う
#
# 必要パッケージ: install.packages("ggplot2")

library(ggplot2)

IN_FILES   <- c("ni_assets_2017.csv", "ni_assets_2021.csv", "ni_assets_2024.csv")
CODE_LIST  <- "EdinetcodeDlInfo.csv"
REFORM_FY  <- 2015   # この年度から「導入後」

FONT <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else
        if (.Platform$OS.type == "windows") "Yu Gothic" else ""

EP_WIDTH  <- 0.005;  EP_RANGE  <- c(-0.25, 0.35)
DEP_WIDTH <- 0.0025; DEP_RANGE <- c(-0.15, 0.15)

# ---------------------------------------------------------------------------
# Step 1: 3つのファイルを「会社 × 年度」の表にまとめる
#
#   有報1本に5年分載ってるので、同じ会社・同じ年度が2つのファイルに
#   出てくることがある。そのときは、その年度に一番近い有報、
#   つまりその年度のことを最初に報告した有報の数字を使う。
# ---------------------------------------------------------------------------
files <- IN_FILES[file.exists(IN_FILES)]
if (length(files) == 0) stop("ni_assets_XXXX.csv が見つかりません")
cat("読み込むファイル:", paste(files, collapse = ", "), "\n")

panel <- do.call(rbind, lapply(files, function(f) {
  d <- read.csv(f, fileEncoding = "UTF-8")
  d <- d[!is.na(d$sec_code), ]                      # 上場してない会社は外す

  # 決算期末が3月以外の会社はほとんどいないけど、念のため外す
  fy_end <- as.Date(d$fy_end)
  d <- d[!is.na(fy_end) & format(fy_end, "%m") == "03", ]
  base_fy <- as.integer(format(as.Date(d$fy_end), "%Y")) - 1   # 2024年3月期 → 2023年度

  do.call(rbind, lapply(0:4, function(k) data.frame(
    edinet_code = d$edinet_code,
    fy          = base_fy - k,
    NI          = d[[paste0("NI_t", k)]],
    TA          = d[[paste0("TA_t", k)]],    # その年度の期末の総資産
    k           = k)))
}))

# 同じ会社・同じ年度がダブってたら k が小さい方を残す
panel <- panel[order(panel$edinet_code, panel$fy, panel$k), ]
panel <- panel[!duplicated(panel[, c("edinet_code", "fy")]), ]

# ---------------------------------------------------------------------------
# Step 2: 金融と電力・ガスを外す
# ---------------------------------------------------------------------------
if (file.exists(CODE_LIST)) {
  cl <- read.csv(CODE_LIST, skip = 1, fileEncoding = "CP932")
  industry <- setNames(cl[[11]], cl[[1]])
  panel$industry <- industry[panel$edinet_code]
  DROP <- c("銀行業", "保険業", "証券、商品先物取引業", "その他金融業", "電気・ガス業")
  cat("業種で外す会社:", length(unique(panel$edinet_code[panel$industry %in% DROP])), "社\n")
  cat("業種がわからない会社:", length(unique(panel$edinet_code[is.na(panel$industry)])), "社\n")
  panel <- panel[!panel$industry %in% DROP, ]
} else {
  warning("EdinetcodeDlInfo.csv がないので業種で外していません")
}

# ---------------------------------------------------------------------------
# Step 3: 前の年の数字を横に並べて EP と ΔEP を計算する
# ---------------------------------------------------------------------------
key <- function(code, fy) paste(code, fy)
idx <- setNames(seq_len(nrow(panel)), key(panel$edinet_code, panel$fy))
prev1 <- idx[key(panel$edinet_code, panel$fy - 1)]
prev2 <- idx[key(panel$edinet_code, panel$fy - 2)]

panel$EP  <- panel$NI / panel$TA[prev1]
panel$DEP <- (panel$NI - panel$NI[prev1]) / panel$TA[prev2]
panel$period <- ifelse(panel$fy < REFORM_FY, "導入前", "導入後")
panel$period <- factor(panel$period, levels = c("導入前", "導入後"))

ep  <- panel[is.finite(panel$EP),  c("edinet_code", "fy", "period", "EP")]
dep <- panel[is.finite(panel$DEP), c("edinet_code", "fy", "period", "DEP")]
names(ep)[4] <- names(dep)[4] <- "value"

cat("\n年度ごとの件数\n")
print(rbind(EP = table(ep$fy), "ΔEP" = table(factor(dep$fy, levels = sort(unique(ep$fy))))))

write.csv(panel, "panel_all_years.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------------
# Step 4: BD1997 の標準化差。bd_test.R と同じ計算
# ---------------------------------------------------------------------------
bd_sd <- function(values, width, range) {
  v <- values[values >= range[1] & values < range[2]]
  N <- length(v)
  breaks <- seq(range[1], range[2], by = width)
  n <- as.vector(table(cut(v, breaks, right = FALSE)))
  p <- n / N
  out <- data.frame(bin_from = head(breaks, -1), n = n, expected = NA, sd_stat = NA)
  for (i in 2:(length(n) - 1)) {
    out$expected[i] <- (n[i - 1] + n[i + 1]) / 2
    var_i <- N * p[i] * (1 - p[i]) +
             N * (p[i - 1] + p[i + 1]) * (1 - p[i - 1] - p[i + 1]) / 4
    out$sd_stat[i] <- (n[i] - out$expected[i]) / sqrt(var_i)
  }
  out
}

around_zero <- function(values, width, range) {
  r <- bd_sd(values, width, range)
  below <- r[abs(r$bin_from + width) < width / 2, ]
  above <- r[abs(r$bin_from) < width / 2, ]
  data.frame(件数 = sum(values >= range[1] & values < range[2]),
             すぐ下_実際 = below$n, すぐ下_予想 = below$expected,
             すぐ下_実際割る予想 = round(below$n / below$expected, 2),
             すぐ下_標準化差 = round(below$sd_stat, 2),
             すぐ上_標準化差 = round(above$sd_stat, 2))
}

# 注意: 標準化差は件数が多いほど大きく出やすい。導入後のほうが年数が多いので、
# 前後を比べるときは「実際 ÷ 予想」や年度ごとの標準化差のほうを見る
cat("\n===== 導入前と導入後の比較 =====\n")
result <- rbind(
  cbind(変数 = "EP",  期間 = "導入前", around_zero(ep$value[ep$period == "導入前"],   EP_WIDTH,  EP_RANGE)),
  cbind(変数 = "EP",  期間 = "導入後", around_zero(ep$value[ep$period == "導入後"],   EP_WIDTH,  EP_RANGE)),
  cbind(変数 = "ΔEP", 期間 = "導入前", around_zero(dep$value[dep$period == "導入前"], DEP_WIDTH, DEP_RANGE)),
  cbind(変数 = "ΔEP", 期間 = "導入後", around_zero(dep$value[dep$period == "導入後"], DEP_WIDTH, DEP_RANGE)))
print(result, row.names = FALSE)
write.csv(result, "bd_pre_post.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 年度ごとにも見ておく
cat("\n===== 年度ごとの「ゼロのすぐ下」の標準化差 =====\n")
by_year <- do.call(rbind, lapply(sort(unique(ep$fy)), function(y) {
  e <- around_zero(ep$value[ep$fy == y], EP_WIDTH, EP_RANGE)
  d <- if (any(dep$fy == y)) around_zero(dep$value[dep$fy == y], DEP_WIDTH, DEP_RANGE) else NULL
  data.frame(年度 = y,
             EP_件数 = e$件数, EP_標準化差 = e$すぐ下_標準化差,
             ΔEP_件数 = if (is.null(d)) NA else d$件数,
             ΔEP_標準化差 = if (is.null(d)) NA else d$すぐ下_標準化差)
}))
print(by_year, row.names = FALSE)
write.csv(by_year, "bd_by_year.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------------
# Step 5: グラフ
# ---------------------------------------------------------------------------
base_theme <- theme_minimal(base_size = 11, base_family = FONT) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 11))

# 導入前と導入後のヒストグラムを左右に並べる。
# 件数がちがうので、縦軸は件数じゃなくて全体に占める割合にしてる
draw_pre_post <- function(df, width, range, xlab, title) {
  df <- df[df$value >= range[1] & df$value < range[2], ]
  df$bin <- floor(round(df$value / width, 8)) * width
  cnt <- aggregate(list(n = df$value), list(period = df$period, bin = df$bin), length)
  cnt$share <- cnt$n / ave(cnt$n, cnt$period, FUN = sum)
  cnt$just_below_zero <- abs(cnt$bin + width) < width / 2
  n_lab <- tapply(df$value, df$period, length)
  levels(cnt$period) <- sprintf("%s  n = %s", levels(cnt$period),
                                format(n_lab[levels(cnt$period)], big.mark = ","))

  ggplot(cnt, aes(x = bin + width / 2, y = share, fill = just_below_zero)) +
    geom_col(width = width * 0.85) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey35") +
    facet_wrap(~ period, ncol = 2) +
    scale_fill_manual(values = c(`FALSE` = "#8BA6C4", `TRUE` = "#1F4E78"), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    labs(x = xlab, y = "全体に占める割合", title = title,
         subtitle = sprintf("ビン幅 %s  濃い棒がゼロのすぐ下", width)) +
    base_theme
}

ggsave("fig_ep_pre_post.png",
       draw_pre_post(ep, EP_WIDTH, EP_RANGE, "EP 当期純利益 ÷ 前期末総資産",
                     "利益水準の分布 コード導入前後"),
       width = 10, height = 4.2, dpi = 200)
ggsave("fig_dep_pre_post.png",
       draw_pre_post(dep, DEP_WIDTH, DEP_RANGE, "ΔEP 純利益の変化 ÷ 前々期末総資産",
                     "利益変化の分布 コード導入前後"),
       width = 10, height = 4.2, dpi = 200)

# 年度ごとの標準化差の推移
long <- rbind(data.frame(fy = by_year$年度, 変数 = "EP",  sd = by_year$EP_標準化差),
              data.frame(fy = by_year$年度, 変数 = "ΔEP", sd = by_year$ΔEP_標準化差))
long <- long[!is.na(long$sd), ]
p3 <- ggplot(long, aes(fy, sd, colour = 変数)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = -2, colour = "grey45", linewidth = 0.4, linetype = "dotted") +
  geom_vline(xintercept = REFORM_FY - 0.5, colour = "grey35", linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = REFORM_FY - 0.4, y = max(long$sd, 0) + 0.3, label = "コード導入",
           hjust = 0, size = 3.3, family = FONT, colour = "grey30") +
  annotate("text", x = max(long$fy), y = -2.25, label = "-2 より下なら偶然とは言いにくい凹み",
           hjust = 1, size = 3, family = FONT, colour = "grey40") +
  geom_line(linewidth = 0.7) + geom_point(size = 2.4) +
  scale_colour_manual(values = c(EP = "#1F4E78", ΔEP = "#D97A2B"), name = NULL) +
  scale_x_continuous(breaks = sort(unique(long$fy))) +
  labs(x = "年度", y = "ゼロのすぐ下の標準化差",
       title = "ゼロのすぐ下の凹み具合の推移",
       subtitle = "マイナスが大きいほど、ちょっとだけ赤字や減益の会社が少ない") +
  base_theme + theme(legend.position = "top", legend.justification = "left")
ggsave("fig_sd_by_year.png", p3, width = 8, height = 4.5, dpi = 200)

cat("\n保存しました: fig_ep_pre_post.png, fig_dep_pre_post.png, fig_sd_by_year.png\n")
cat("表: bd_pre_post.csv, bd_by_year.csv, panel_all_years.csv\n")
