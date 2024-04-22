main <- function(){

# load data ---------------------------------------------------------------
  
  df_jap <- read.csv(here::here("01_data", "intermediate", "population", "japanese.csv"), fileEncoding = "cp932")
  df_for <- read.csv(here::here("01_data", "intermediate", "population", "overseas.csv"), fileEncoding = "cp932")
  df_both <- read.csv(here::here("01_data", "intermediate", "population", "both.csv"), fileEncoding = "cp932")
  

# 市町村合併データ ----------------------------------------------------------------
  adjust_df <- readxl::read_xlsx("01_data/raw/municipality_converter/municipality_converter_jp.xlsx")
  
  df_city_id_2020 <- adjust_df |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::rename(
      city_id = id_muni2020
    )
  
  list_2020_id <- df_city_id_2020 |> 
    dplyr::pull(city_id)
  

# 市町村名とidのデータフレーム ---------------------------------------------------------
  df_name_id <- df_jap |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name
    ) |> 
    dplyr::distinct() |> 
    dplyr::right_join(
      df_city_id_2020
    ) |>
    # 重複する市区町村名を削除
    dplyr::filter(
      !city_name %in% c("丹波篠山市", "八丈島　八丈町", "三宅島　三宅村")
    )

  # 市町村合併の調整  
  df_adjusted_jap <- purrr::map(list_2020_id, adjust_city_id, df_jap, adjust_df) |> 
    dplyr::bind_rows()
  
  df_adjusted_for <- purrr::map(list_2020_id, adjust_city_id, df_for, adjust_df) |> 
    dplyr::bind_rows()
  
  df_adjusted_both <- purrr::map(list_2020_id, adjust_city_id, df_both, adjust_df) |> 
    dplyr::bind_rows()
  
  # time_test(df_both, current_cityid_list, adjust_df)  
  
  # 対数と変化率の追加
  df_jap_output <- add_variabels(df_adjusted_jap, df_name_id)
  df_for_output <- add_variabels(df_adjusted_for, df_name_id)
  df_both_output <- add_variabels(df_adjusted_both, df_name_id)

  # データの保存
  write.csv(df_jap_output, here::here("01_data", "intermediate", "population", "japanese_adjusted.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_for_output, here::here("01_data", "intermediate", "population", "overseas_adjusted.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_both_output, here::here("01_data", "intermediate", "population", "both_adjusted.csv"), fileEncoding = "cp932", row.names = FALSE)
  
  

}


adjust_city_id <- function(id_n, df_pop, adjust_df){
  print(id_n)
  
  list_id <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n) |> 
    dplyr::select(seq(2,10)) |> 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "year",
      values_to = "id"
    ) |> 
    dplyr::distinct(id) |> 
    dplyr::pull()
  
  df_id_n <- df_pop |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
      )
  
  city_data <- df_pop |>
    dplyr::filter(year == 2020,
                  city_id == id_n) |> 
    dplyr::select(city_id, city_name, prefecture_name)
  
  city_id_n <- city_data |> 
    dplyr::pull(city_id)
  city_name_n <- city_data |> 
    dplyr::pull(city_name)
  prefecture_name_n <- city_data |> 
    dplyr::pull(prefecture_name)
  
  list_vars <- c(
    "male",
    "female",
    "total",
    "household",
    "moving_in_dom",
    "moving_in_int",
    "moving_in_total",
    "birth",
    "increase_total",
    "moving_out_dom",
    "moving_out_int",
    "moving_out_total",
    "mortality",
    "decrease_total",
    "change",
    "natural",
    "social"
  )


  output_data <- df_id_n |>
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = year
    ) |>
    dplyr::mutate(
      city_id = city_id_n
      )
  
  return(output_data)
  
}


add_variabels <- function(df_input, df_name_id) {

  df_output <- df_input |> 
    dplyr::left_join(
      df_name_id
    ) |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(lag_total = dplyr::lag(total), .after = total) |> 
    dplyr::mutate(change_rate = change/dplyr::lag(total)*100, .after = change) |> 
    dplyr::mutate(natural_rate = natural/dplyr::lag(total)*100, .after = natural) |> 
    dplyr::mutate(social_rate = (social/dplyr::lag(total))*100, .after = social) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year,
      dplyr::everything()
    )
  
  return(df_output)
}


main()

