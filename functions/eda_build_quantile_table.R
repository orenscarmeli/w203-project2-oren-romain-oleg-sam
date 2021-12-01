eda_build_quantile_table <- function(column) {
  probs <- sort(c(seq(0, 1, 0.25), 0.05, 0.95))
  result <-
    as.data.table(t(data.table(quantile(d[, column], probs = probs))))
  setnames(result, paste0(probs * 100, '%'))
  result[, variable := column]
  result[, diff_min_vs_max := `100%` - `0%`]
  return(result)
  
}