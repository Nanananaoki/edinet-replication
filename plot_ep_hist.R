# BD1997 の Figure 風に、EP と ΔEP のヒストグラムを描くスクリプト
# collect_ni_assets.R が作った CSV を読み込む
#
#   EP  = 当期純利益 ÷ 前期末総資産
#   ΔEP = (当期純利益 − 前期純利益) ÷ 前々期末総資産
#
# 有報1本に5年分の数字があるので、1社から EP は4年分、ΔEP は3年分作れる
#
# 必要パッケージ: install.packages("ggplot2")

library(ggplot2)

IN_CSV <- "ni_assets_2024.csv"   # テストなら "ni_assets_test.csv"

# 日本語が文字化けしないようにフォントを指定。Macならヒラギノ
FONT <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else if (.Platform$OS.type == "windows") "Yu Gothic" else ""

# BD1997 に合わせたビンの幅と表示範囲
EP_WIDTH  <- 0.005;  EP_RANGE  <- c(-0.25, 0.35)
DEP_WIDTH <- 0.0025; DEP_RANGE <- c(-0.15, 0.15)

# ---------------------------------------------------------------------------
# 1社1行の横長データ → 1社1年1行の縦長データにする
# ---------------------------------------------------------------------------
raw <- read.csv(IN_CSV, fileEncoding = "UTF-8")

# 証券コードがない会社は上場してないので外す
raw <- raw[!is.na(raw$sec_code), ]

# 金融と電力・ガスを外す。EDINETコードリストの11列目が業種
code_list <- read.csv("EdinetcodeDlInfo.csv", skip = 1, fileEncoding = "CP932")
industry  <- setNames(code_list[[11]], code_list[[1]])
raw$industry <- industry[raw$edinet_code]
DROP_INDUSTRY <- c("銀行業", "保険業", "証券、商品先物取引業", "その他金融業", "電気・ガス業")
cat("外す件数:", sum(raw$industry %in% DROP_INDUSTRY), "\n")
raw <- raw[!raw$industry %in% DROP_INDUSTRY, ]

ep <- do.call(rbind, lapply(0:3, function(k) data.frame(
  edinet_code = raw$edinet_code, lag = k,
  value = raw[[paste0("NI_t", k)]] / raw[[paste0("TA_t", k + 1)]])))

dep <- do.call(rbind, lapply(0:2, function(k) data.frame(
  edinet_code = raw$edinet_code, lag = k,
  value = (raw[[paste0("NI_t", k)]] - raw[[paste0("NI_t", k + 1)]]) /
          raw[[paste0("TA_t", k + 2)]])))

ep  <- ep[is.finite(ep$value), ]
dep <- dep[is.finite(dep$value), ]

# ---------------------------------------------------------------------------
# ヒストグラムを描く関数
#   ゼロがちょうどビンの境目になるようにしていて、
#   ゼロのすぐ左のビンだけ色を濃くしている
# ---------------------------------------------------------------------------
draw_hist <- function(df, width, range, xlab, title) {
  df <- df[df$value >= range[1] & df$value < range[2], ]
  df$bin <- floor(df$value / width) * width
  counts <- aggregate(list(n = df$value), list(bin = df$bin), length)
  counts$just_below_zero <- abs(counts$bin + width) < width / 2

  ggplot(counts, aes(x = bin + width / 2, y = n, fill = just_below_zero)) +
    geom_col(width = width * 0.85) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey35") +
    scale_fill_manual(values = c(`FALSE` = "#8BA6C4", `TRUE` = "#1F4E78"), guide = "none") +
    labs(x = xlab, y = "企業・年の数", title = title,
         subtitle = sprintf("n = %s  ビン幅 %s  濃い棒がゼロのすぐ下",
                            format(nrow(df), big.mark = ","), width)) +
    theme_minimal(base_size = 11, base_family = FONT) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"))
}

p1 <- draw_hist(ep,  EP_WIDTH,  EP_RANGE,  "EP 当期純利益 ÷ 前期末総資産",
                "利益水準の分布")
p2 <- draw_hist(dep, DEP_WIDTH, DEP_RANGE, "ΔEP 純利益の変化 ÷ 前々期末総資産",
                "利益変化の分布")

ggsave("fig_ep_hist.png",  p1, width = 7, height = 4.2, dpi = 200)
ggsave("fig_dep_hist.png", p2, width = 7, height = 4.2, dpi = 200)
cat("保存しました: fig_ep_hist.png, fig_dep_hist.png\n")
