eda_calculate_stats_by_group <-
  function(column, dt, quantile_table = FALSE) {
    # calc summary statistics per unique element in categorical column
    result <- dt[, .(
      `Count Apps` = .N,
      `Log of Install Count (Avg)` = mean(log_installs, na.rm = TRUE)
    ),
    by = column][`Count Apps` > 100][order(-`Log of Install Count (Avg)`)]
    setnames(result, column, 'Label')
    result[, `Variable` := column]
    
    # create a table that shows the distribution across group averages
    if (quantile_table) {
      result <- as.data.table(t(data.table(
        quantile(result$`Log of Install Count (Avg)`)
      )))
      setnames(result, c('0%', '25%', '50%', '75%', '100%'))
      result[, `Variable` := column]
      result[, `Diff (Max - Min)` := `100%` - `0%`]
      result[, `Diff (Max - Min) / Min` := `Diff (Max - Min)` / `0%`]
      result <- subset(result, select = c('Variable','0%', '25%', '50%', '75%', '100%','Diff (Max - Min)','Diff (Max - Min) / Min'))
      setorder(result,`Diff (Max - Min)`)
    }
    return(result)
  }