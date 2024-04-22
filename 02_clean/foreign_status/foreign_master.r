main <- function() {
  
  # 都道府県コードとidを読み込む
  df_code <- read_code()
  
  df_status <- read.csv(here::here("01_data", "intermediate", "foreign_status", "foreign_classified.csv"), fileEncoding = "cp932") |> 
    dplyr::ungroup() |> 
    dplyr::filter(year >= 2010)

  df_pop <- read_population_data()

  # df_status |> View()
  
  df_joint <- df_code |> 
    dplyr::left_join(df_status) 
    # dplyr::left_join(df_pop, by = c("year", 
    #                                 "prefecture_name"))
  
  df_master <- add_log_var(df_joint) |> 
    dplyr::ungroup()
    
  write.csv(df_master, here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932", row.names = FALSE)
  
}


read_code <- function() {
  
  df_code <- readxl::read_xls(here::here("01_data", "raw", "municipality_code", "municipality_code.xls"), col_names = FALSE) |> 
    dplyr::select(1, 2, 3)
  
  colnames(df_code) <- c("id", "prefecture_name", "city_name") 
  
  df_output <- df_code |> 
    dplyr::filter(is.na(city_name)) |> 
    dplyr::mutate(id = str_sub(id, start = 1, end = -2),
                  id = as.numeric(id)) |> 
    dplyr::select(-city_name) |> 
    dplyr::mutate(prefecture_name = str_replace_all(prefecture_name, "県", "")) |> 
    dplyr::mutate(prefecture_name = str_replace_all(prefecture_name, "府", "")) |> 
    dplyr::mutate(prefecture_name = str_replace_all(prefecture_name, "東京都", "東京"))
  
  return(df_output)
  
  
}


read_population_data <- function() {
  
  df_pop <- read.csv(here::here("01_data", "intermediate", "foreign_status", "both_for_foreign_status.csv"), fileEncoding = "cp932") |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "県", "")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "府", "")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京")) |> 
    dplyr::select(
      prefecture_name,
      year,
      total) |> 
    dplyr::mutate(
      ln_total = log(total)) 
  
  return(df_pop)
  
}

add_log_var <- function(df_input) {
  
  df_based <- df_input |> 
    dplyr::group_by(prefecture_name) |> 
    dplyr::mutate(
      ln_foreign        = log(total_foreign),
      lag_ln_foreign    = dplyr::lag(ln_foreign),
      lag_ln_foreign_2  = dplyr::lag(ln_foreign, n = 2),
      lag_ln_foreign_3  = dplyr::lag(ln_foreign, n = 3),
      lag_ln_foreign_5  = dplyr::lag(ln_foreign, n = 5),
      lag_ln_foreign_10 = dplyr::lag(ln_foreign, n = 10),
      ln_change_rate_foreign    = ln_foreign - lag_ln_foreign,
      ln_change_rate_foreign_2  = ln_foreign - lag_ln_foreign_2,
      ln_change_rate_foreign_3  = ln_foreign - lag_ln_foreign_3,
      ln_change_rate_foreign_5  = ln_foreign - lag_ln_foreign_5,
      ln_change_rate_foreign_10 = ln_foreign - lag_ln_foreign_10,
      .after = total_foreign
    ) |>
    dplyr::mutate(
      ln_high = log(high_skill),
      lag_ln_high = dplyr::lag(ln_high),
      lag_ln_high_2 = dplyr::lag(ln_high, n = 2),
      lag_ln_high_3 = dplyr::lag(ln_high, n = 3),
      lag_ln_high_5 = dplyr::lag(ln_high, n = 5),
      ln_change_rate_high = ln_high - lag_ln_high,
      ln_change_rate_high_2 = ln_high - lag_ln_high_2,
      ln_change_rate_high_3 = ln_high - lag_ln_high_3,
      ln_change_rate_high_5 = ln_high - lag_ln_high_5,
      .after = high_skill
    ) |>
    dplyr::mutate(
      ln_low = log(low_skill),
      lag_ln_low = dplyr::lag(ln_low),
      lag_ln_low_2 = dplyr::lag(ln_low, n = 2),
      lag_ln_low_3 = dplyr::lag(ln_low, n = 3),
      lag_ln_low_5 = dplyr::lag(ln_low, n = 5),
      ln_change_rate_low = ln_low - lag_ln_low,
      ln_change_rate_low_2 = ln_low - lag_ln_low_2,
      ln_change_rate_low_3 = ln_low - lag_ln_low_3,
      ln_change_rate_low_5 = ln_low - lag_ln_low_5,
      .after = low_skill
    ) |>
    dplyr::mutate(
      ln_status = log(status),
      lag_ln_status = dplyr::lag(ln_status),
      lag_ln_status_2 = dplyr::lag(ln_status, n = 2),
      lag_ln_status_3 = dplyr::lag(ln_status, n = 3),
      lag_ln_status_5 = dplyr::lag(ln_status, n = 5),
      ln_change_rate_status = ln_status - lag_ln_status,
      ln_change_rate_status_2 = ln_status - lag_ln_status_2,
      ln_change_rate_status_3 = ln_status - lag_ln_status_3,
      ln_change_rate_status_5 = ln_status - lag_ln_status_5,
      .after = status
    ) |>
    dplyr::mutate(
      ln_sp_residents = log(specific_permanent_resident),
      lag_ln_sp_residents = dplyr::lag(ln_sp_residents),
      lag_ln_sp_residents_2 = dplyr::lag(ln_sp_residents, n = 2),
      lag_ln_sp_residents_3 = dplyr::lag(ln_sp_residents, n = 3),
      lag_ln_sp_residents_5 = dplyr::lag(ln_sp_residents, n = 5),
      ln_change_rate_sp_residents = ln_sp_residents - lag_ln_sp_residents,
      ln_change_rate_sp_residents_2 = ln_sp_residents - lag_ln_sp_residents_2,
      ln_change_rate_sp_residents_3 = ln_sp_residents - lag_ln_sp_residents_3,
      ln_change_rate_sp_residents_5 = ln_sp_residents - lag_ln_sp_residents_5,
      .after = specific_permanent_resident
    ) |>
    dplyr::mutate(
      ln_training = log(training),
      lag_ln_training = dplyr::lag(ln_training),
      lag_ln_training_2 = dplyr::lag(ln_training, n = 2),
      lag_ln_training_3 = dplyr::lag(ln_training, n = 3),
      lag_ln_training_5 = dplyr::lag(ln_training, n = 5),
      ln_change_rate_training = ln_training - lag_ln_training,
      ln_change_rate_training_2 = ln_training - lag_ln_training_2,
      ln_change_rate_training_3 = ln_training - lag_ln_training_3,
      ln_change_rate_training_5 = ln_training - lag_ln_training_5,
      .after = training
    ) |>
    dplyr::mutate(
      status_total = status + specific_permanent_resident,
      # 身分系に特別永住者を含めた変数
      ln_status_total = log(status),
      lag_ln_status_total = dplyr::lag(ln_status_total),
      lag_ln_status_total_2 = dplyr::lag(ln_status_total, n = 2),
      lag_ln_status_total_3 = dplyr::lag(ln_status_total, n = 3),
      lag_ln_status_total_5 = dplyr::lag(ln_status_total, n = 5),
      ln_change_rate_status_total = ln_status_total - lag_ln_status_total,
      ln_change_rate_status_total_2 = ln_status_total - lag_ln_status_total_2,
      ln_change_rate_status_total_3 = ln_status_total - lag_ln_status_total_3,
      ln_change_rate_status_total_5 = ln_status_total - lag_ln_status_total_5,
      .after = ln_change_rate_sp_residents_5
    ) |>
    dplyr::mutate(
      ln_except_specific = log(except_specific),
      lag_ln_except_specific = dplyr::lag(ln_except_specific),
      lag_ln_except_specific_2 = dplyr::lag(ln_except_specific, n = 2),
      lag_ln_except_specific_3 = dplyr::lag(ln_except_specific, n = 3),
      lag_ln_except_specific_5 = dplyr::lag(ln_except_specific, n = 5),
      lag_ln_except_specific_10 = dplyr::lag(ln_except_specific, n = 10),
      ln_change_rate_except_specific = ln_except_specific - lag_ln_except_specific,
      ln_change_rate_except_specific_2 = ln_except_specific - lag_ln_except_specific_2,
      ln_change_rate_except_specific_3 = ln_except_specific - lag_ln_except_specific_3,
      ln_change_rate_except_specific_5 = ln_except_specific - lag_ln_except_specific_5,
      ln_change_rate_except_specific_10 = ln_except_specific - lag_ln_except_specific_10,
      .after = ln_change_rate_except_specific_5
    ) # |>
    # dplyr::mutate(
    #   lag_ln_total = dplyr::lag(ln_total),
    #   lag_ln_total_2 = dplyr::lag(ln_total, n = 2),
    #   lag_ln_total_3 = dplyr::lag(ln_total, n = 3),
    #   lag_ln_total_5 = dplyr::lag(ln_total, n = 5),
    #   lag_ln_total_10 = dplyr::lag(ln_total, n = 10),
    #   .after = ln_total
    # )
  
    
  
  return(df_based)
  
}

main()
