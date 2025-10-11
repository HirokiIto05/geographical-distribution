main <- function() {

  source(here::here("02_clean", "population", "funs_population.r"))
  
  # 日本語の列名を英語に変換
  list_cols <- generate_list_cols()

  list_year <- seq(2013, 2022)

  non_numeric_variables <- c("prefecture_name", "city_name")

  df_new_old_id_mapping <- generate_df_merge_muni()

  # clean data ---------------------------------------------------------------------
  # read raw data
  # Japanese
  df_jp_raw <- clean_population_data("japanese")
  # Foreigners
  df_for_raw <- clean_population_data("overseas")
  # both
  df_both_raw <- clean_population_data("both")

  # adjust data
  df_jp_adjusted <- adjust_muni_population(df_jp_raw, df_new_old_id_mapping)
  df_for_adjusted <- adjust_muni_population(df_for_raw, df_new_old_id_mapping)
  df_both_adjusted <- adjust_muni_population(df_both_raw, df_new_old_id_mapping)

  # add lag variables
  df_jp_master <- add_lag_variables(df_jp_adjusted)
  df_for_master <- add_lag_variables(df_for_adjusted)
  df_both_master <- add_lag_variables(df_both_adjusted)

  # save -----------------------------------------------------------------
  write.csv(df_jp_master, here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_for_master, here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_both_master, here::here("01_data", "intermediate", "population", "both_master.csv"), fileEncoding = "cp932", row.names = FALSE)

}

main()
