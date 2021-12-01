get_clean_dataset <- function(minimum_review_count = 100) {
  
  # ##############################################
  # load raw csv
  
  data <- read.csv('data/googleplaystore.csv')
  
  # ##############################################
  # data cleaning
  
  clean_size <- function(s) {
    n <- nchar(s)
    last <- substr(s, n, n)
    if (last == "B") {
      k <- 10.0
    } else if (last == "M") {
      k <- 1.0
    } else if (last == "k") {
      k <- 0.1
    } else {
      return(NA)
    }
    return(as.numeric(substr(s, 1, n - 1)) * k)
  }
  
  clean_reviews <- function(s) {
    n = nchar(s)
    last <- substr(s, n, n + 1)
    if (last == "M") {
      return(as.numeric(substr(s, 1, n - 1)) * 1.0e6)
    } else {
      return(as.numeric(s))
    }
  }
  
  MAX_DATE = suppressWarnings(max(mdy(data$Last.Updated), na.rm = TRUE))
  clean_date <- function(d) {
    return(interval(d, MAX_DATE) / years(1))
  }
  
  data_clean <- data %>%
    distinct() %>%
    mutate(
      installs            = suppressWarnings(as.numeric(gsub(
        "[\\+,]", "", Installs
      ))),
      size                = sapply(Size, clean_size),
      reviews             = sapply(Reviews, clean_reviews),
      rating              = Rating,
      price               = suppressWarnings(as.numeric(gsub(
        "\\$", "", as.character(Price)
      ))),
      is_free             = price == 0,
      last_updated        = suppressWarnings(mdy(Last.Updated)),
      last_updated        = interval(last_updated, MAX_DATE) / years(1),
      android_version         = suppressWarnings(as.numeric(substr(
        gsub("Varies with device", NA, Android.Ver),
        start = 1,
        stop = 3
      ))),
      current_version     = suppressWarnings(as.numeric(substr(
        gsub("Varies with device", NA, Current.Ver),
        start = 1,
        stop = 3
      ))),
      category            = Category,
      is_family_category  = category == "FAMILY",
      is_game_category    = category == "GAME",
      is_tools_category   = category == "TOOLS",
      genre               = Genres,
      content_rating      = Content.Rating,
      is_content_everyone = content_rating == "Everyone",
      type = Type
    ) %>%
    select(
      .,
      installs,
      size,
      reviews,
      rating,
      price,
      is_free,
      last_updated,
      android_version,
      current_version,
      category,
      is_family_category,
      is_game_category,
      is_tools_category,
      genre,
      content_rating,
      is_content_everyone,
      type
    ) %>%
    drop_na() %>%
    filter(., reviews >= minimum_review_count, rating <= 5.0)
  
  # bins for install count: install groups-- increments by 1, there are 19 of them.
  install_groups <- data.table(table(data_clean$installs))
  setnames(install_groups, c('V1', 'N'), c('installs', 'count'))
  install_groups[, install_group := 1:.N]
  install_groups <- install_groups[, c('installs', 'install_group')]
  data_clean <- merge(data_clean, install_groups, by = 'installs')
  
  # ##############################################
  # transformations
  
  data_clean$log_installs        <- log10(data_clean$installs)
  data_clean$log_size            <- log10(data_clean$size)
  data_clean$log_current_version <- log10(data_clean$current_ver + 1)
  data_clean$log_last_updated    <- log10(data_clean$last_updated + 1)
  data_clean$log_reviews         <- log10(data_clean$reviews)
  
  return(data_clean)
}