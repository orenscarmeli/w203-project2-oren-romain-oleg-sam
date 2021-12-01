eda_calculate_stats_by_group <-
  function(column, quantile_table = FALSE) {
    
    # calc summary statistics per unique element in categorical column
    result <- d_dt[, .(count_apps = .N,
                       install_group_avg = mean(install_group, na.rm = TRUE),
                       install_count_med = median(installs, na.rm = TRUE)), 
                   by = column][count_apps > 100][order(-install_group_avg)]
    setnames(result, column, 'group_by_column')
    result[, variable := column]
    
    # create a table that shows the distribution across group averages
    if (quantile_table) {
      result <- as.data.table(t(data.table(quantile(
        result$install_group_avg
      ))))
      setnames(result, c('0%', '25%', '50%', '75%', '100%'))
      result[, variable := column]
      result[, diff_min_vs_max := `100%` - `0%`]
    }
    return(result)
  }