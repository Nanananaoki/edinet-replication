# BD1997 の「標準化差」の検定
#
# 考え方
#   分布がなめらかなら、ある棒の高さは両隣の棒の平均くらいになるはず。
#   実際の高さが、その予想からどれだけずれてるかを「標準化差」として数字にする。
#   だいたい -2 より小さければ、偶然とは言いにくいくらい凹んでる。
#   +2 より大きければ、偶然とは言いにくいくらい盛り上がってる。
#
# 使い方
#   plot_ep_hist.R を先に回して ep と dep を作ってから source("bd_test.R")

if (!exists("ep") || !exists("dep")) source("plot_ep_hist.R")

# 全部の棒について標準化差を計算する
bd_sd <- function(values, width, range) {
  v <- values[values >= range[1] & values < range[2]]
  N <- length(v)

  # ビンの区切りを作る。ゼロがちょうど境目になる
  breaks <- seq(range[1], range[2], by = width)
  bins   <- head(breaks, -1)
  n      <- as.vector(table(cut(v, breaks, right = FALSE)))
  p      <- n / N

  out <- data.frame(bin_from = bins, bin_to = bins + width, n = n,
                    expected = NA_real_, sd_stat = NA_real_)

  # 両端の棒は隣が片方しかないので計算しない
  for (i in 2:(length(n) - 1)) {
    expected <- (n[i - 1] + n[i + 1]) / 2
    variance <- N * p[i] * (1 - p[i]) +
                N * (p[i - 1] + p[i + 1]) * (1 - p[i - 1] - p[i + 1]) / 4
    out$expected[i] <- expected
    out$sd_stat[i]  <- (n[i] - expected) / sqrt(variance)
  }
  out
}

# 結果を見やすく表示する
show_result <- function(res, width, name) {
  below <- res[abs(res$bin_to) < width / 2, ]     # ゼロのすぐ下の棒
  above <- res[abs(res$bin_from) < width / 2, ]   # ゼロのすぐ上の棒

  cat(sprintf("\n===== %s =====\n", name))
  cat(sprintf("ゼロのすぐ下: 実際 %d件  予想 %.1f件  標準化差 %.2f\n",
              below$n, below$expected, below$sd_stat))
  cat(sprintf("ゼロのすぐ上: 実際 %d件  予想 %.1f件  標準化差 %.2f\n",
              above$n, above$expected, above$sd_stat))

  # 他の棒と比べてどれくらい目立つか
  others <- res$sd_stat[!is.na(res$sd_stat) &
                        !(res$bin_from %in% c(below$bin_from, above$bin_from))]
  cat(sprintf("ほかの棒の標準化差: 平均 %.2f  一番小さい %.2f  一番大きい %.2f\n",
              mean(others), min(others), max(others)))
}

res_ep  <- bd_sd(ep$value,  EP_WIDTH,  EP_RANGE)
res_dep <- bd_sd(dep$value, DEP_WIDTH, DEP_RANGE)

show_result(res_ep,  EP_WIDTH,  "EP 利益水準")
show_result(res_dep, DEP_WIDTH, "ΔEP 利益変化")

write.csv(res_ep,  "bd_test_ep.csv",  row.names = FALSE)
write.csv(res_dep, "bd_test_dep.csv", row.names = FALSE)
cat("\n全部の棒の結果は bd_test_ep.csv と bd_test_dep.csv に保存しました\n")
