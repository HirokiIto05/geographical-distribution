main <- function() {
  
  # 日本語の列名を英語に変換
  list_colname <- create_list_colname()
  
  year_list <- seq(2013, 2022)
  
  non_numeric_variables <- c("prefecture_name", "city_name")

  # clean data ---------------------------------------------------------------------
  # Japanese
  df_jap <- purrr::map(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = "japanese") |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "japanese",
                  .after = city_name) |>
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 
  
  # Foreigners
  df_for <- purrr::map(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = "overseas") |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "overseas",
                  .after = city_name) |>
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 
  
  # save -----------------------------------------------------------------
  write.csv(df_jap, here::here("01_data", "intermediate", "population", "japanese.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_for, here::here("01_data", "intermediate", "population", "overseas.csv"), fileEncoding = "cp932", row.names = FALSE)
  
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
  
  if (nationality == "japanese") {
    if (year_n <= 2020) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(year_n, '.xls'))
      
      new_df <-
        readxl::read_xls(file_name, col_names = FALSE) |> 
        dplyr::select(-c(7, 8, 14, 15,
                         16, 22, 23, 24))
    } else if (year_n >= 2021) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(year_n, '.xlsx'))
      
      new_df <-  readxl::read_xlsx(file_name) |>
        dplyr::select(-c(7, 8, 14, 15,
                         16, 22, 23, 24))
      
    }
    colnames(new_df) = list_colname
    
  } else if (nationality == "overseas") {
    if (year_n <= 2013) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(year_n, '.xls'))
      
      new_df <-  readxl::read_xls(file_name, col_names = FALSE) |>
        dplyr::select(-c(12, 13, 14, 15,
                         20, 21, 22))
    } else if (year_n <= 2020) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(year_n, '.xls'))
      
      new_df <-  readxl::read_xls(file_name, col_names = FALSE) |>
        dplyr::select(-c(12, 13, 14,
                         20, 21, 22))
    } else if (year_n >= 2021) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(year_n, '.xlsx'))
      
      new_df <-
        readxl::read_xlsx(file_name, col_names = FALSE) |>
        dplyr::select(-c(12, 13, 14,
                         20, 21, 22))
      
    }
    colnames(new_df) = list_colname
    
  } else if (nationality == "both") {
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
    
  }
  
  output_df <- new_df |>
    dplyr::mutate(year = year_n, .after = city_name) |> 
    dplyr::slice(c(-1:-4)) |> 
    dplyr::ungroup()
  
  return(output_df)
  
}


main()