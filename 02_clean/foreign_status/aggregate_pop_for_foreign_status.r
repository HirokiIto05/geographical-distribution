main <- function() {
  
  # 日本語の列名を英語に変換
  list_colname <- create_list_colname()
  
  # 在留資格の分析に用いるため, 2010 - 2022年でも一度コードを実行して保存する。
  year_list <- seq(2010, 2022)
  
  non_numeric_variables <- c("prefecture_name", "city_name")

  # clean data ---------------------------------------------------------------------
  
  # 市区町村のパネルデータの作成
  df_both <- purrr::map(year_list,
                        aggregate_pop,
                        list_colname = list_colname,
                        nationality = "both") |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "both",
                  .after = city_name) |>
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate))

  # 市町村合併データ 
  adjust_df <- readxl::read_xlsx("01_data/raw/municipality_converter/municipality_converter_jp.xlsx")
  
  df_city_id_2020 <- adjust_df |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::rename(
      city_id = id_muni2020
    )
  
  list_2020_id <- df_city_id_2020 |> 
    dplyr::pull(city_id)
  

  # 市町村名とidのデータフレーム
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
    dplyr::mutate(
      city_id = as.character(city_id)
    )

  # 市町村合併の調整  
  
  df_adjusted_both <- purrr::map(list_2020_id, adjust_city_id, df_both, adjust_df) |> 
    dplyr::bind_rows()
  
  # 対数と変化率の追加
  df_both_output <- df_adjusted_both |>
    # dplyr::mutate(city_id = as.character(city_id)) |>
    add_variabels(df_name_id)
  
  # save -----------------------------------------------------------------
  write.csv(df_both_output, here::here("01_data", "intermediate", "foreign_status", "both_for_foreign_status.csv"), fileEncoding = "cp932", row.names = FALSE)
  
}


# Functions -----------------------------------------------------------------

# generate a colname's list 
create_list_colname <- function() {
  list_colname <- c(
    "city_id",
    "prefecture_name",
    "city_name",
    "male",
    "female",
    "total",
    "household",
    "moving_in_dom", # 国内からの転入者
    "moving_in_int", # 国外からの転入者
    "moving_in_total", # 転入者合計
    "birth",
    "increase_total",
    "moving_out_dom", # 国内への転出者
    "moving_out_int", # 国外への転出者
    "moving_out_total", # 転出者合計
    "mortality",
    "decrease_total",
    "change", # 増減数
    "change_rate",
    "natural", # 自然増減数
    "natural_rate",
    "social", # 社会増減数
    "social_rate"
  )
  
  return(list_colname)
  
}

# aggregate for changing raw data to panel data
aggregate_pop <- function(year_n, list_colname, nationality) {
  print(year_n)
  if (year_n <= 2020) {
    file_name <- here::here(
      "01_data",
      'raw', 
      'jumin_kihon_daicho', 
      nationality, 
      paste0(year_n,'.xls')
      )
    
    new_df <-  readxl::read_xls(file_name) |>
      dplyr::select(-c(12, 18))
  } else if (year_n >= 2021) {
    file_name <- here::here(
      "01_data",
      'raw', 
      'jumin_kihon_daicho', 
      nationality, 
      paste0(year_n,'.xlsx')
      )
    
    new_df <-  readxl::read_xlsx(file_name) |>
      dplyr::select(-c(12, 18))
    
  }
  colnames(new_df) = list_colname
  
  output_df <- new_df |>
    dplyr::mutate(year = year_n, .after = city_name) |> 
    dplyr::slice(c(-1:-4)) |> 
    dplyr::ungroup()
  
  return(output_df)
  
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
