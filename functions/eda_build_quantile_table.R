eda_build_quantile_table <- function(column, dt) {
  probs <- sort(c(seq(0, 1, 0.25), 0.025, 0.975))
  result <-
    as.data.table(t(data.table(quantile(dt[, column], probs = probs))))
  setnames(result, paste0(probs * 100, '%'))
  result[, variable := column]
  return(result)
  
}