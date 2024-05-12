main <- function() {
  # データ読み込み
  df_jap <- read.csv(here::here("01_data", "intermediate", "population", "japanese_adjusted.csv"), fileEncoding = "cp932")
  df_for <- read.csv(here::here("01_data", "intermediate", "population", "overseas_adjusted.csv"), fileEncoding = "cp932")
  
  # 対数変化率の計算。1, 5年のラグを伴った変数の作成
  df_add_jap <- add_lag_variables(df_jap)
  df_add_for <- add_lag_variables(df_for)
  
  # データ保存
  write.csv(df_add_jap, here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_add_for, here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  
}


add_lag_variables <- function(df_input) {
  
  df_based <- df_input |>
    dplyr::group_by(city_id) |>
    dplyr::mutate(
      lag_total = dplyr::lag(total),
      change_rate = change/dplyr::lag(total)*100,
      natural_rate = natural/dplyr::lag(total)*100,
      social_rate = (social/dplyr::lag(total))*100,
      ln_total = log(total),
      ln10_total = log10(total),
      lag_ln_total = dplyr::lag(ln_total),
      lag_ln_total_5 = dplyr::lag(ln_total, n = 5),
      ln_change_rate_total = ln_total - lag_ln_total,
      ln_change_rate_total_5 = ln_total - lag_ln_total_5
    ) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year,
      male,
      female,
      total,
      household,
      moving_in_dom,
      moving_in_int,
      moving_in_total,
      birth,
      increase_total,
      moving_out_dom,
      moving_out_int,
      moving_out_total,
      mortality,
      decrease_total,
      change,
      change_rate,
      natural,
      natural_rate,
      social,
      social_rate,
      lag_total,
      ln_total,
      ln10_total,
      lag_ln_total,
      lag_ln_total_5,
      ln_change_rate_total,
      ln_change_rate_total_5,
    )
  
  return(df_based)
  
}


main()