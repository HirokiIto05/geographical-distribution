generate_list_cols <- function() {
  list_cols <- c(
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
    "moving_in_others", # その他合計
    "increase_total",
    "moving_out_dom", # 国内への転出者
    "moving_out_int", # 国外への転出者
    "moving_out_total", # 転出者合計
    "mortality",
    "moving_out_others", # その他合計
    "decrease_total",
    "change", # 増減数
    "natural", # 自然増減数
    "social" # 社会増減数
  )

  return(list_cols)

}

aggregate_population <- function(nationality_i, list_year, list_cols) {

  purrr::map_dfr(
    list_year,
    read_population_year,
    list_cols,
    nationality = nationality_i
    ) |>  
  mutate(nationality = nationality_i, .after = city_name) 
}


change_cols_class <- function(df) {

  non_numeric_variables <- c("prefecture_name", "city_name", "nationality")

  df |>
    mutate(across(-any_of(non_numeric_variables), as.numeric)) |>
    mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) 
}


# aggregate for changing raw data to panel data
read_population_year <- function(year_n, list_colname, nationality) {
  print(year_n)

  if (nationality == "japanese") {
    raw_name <- paste0((year_n -2000), "07nsjin")

    if (year_n <= 2020) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, ".xls"))

      new_df <- readxl::read_xls(file_name, col_names = FALSE) |>
        select(-c(7, 8, 14, 15,22, 23, 27, 29, 31)) 
    } else if (year_n <= 2023) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, ".xlsx"))

      new_df <- readxl::read_xlsx(file_name) |>  
        select(-c(7, 8, 14, 15, 22, 23, 27, 29, 31))  
      
    } else if(year_n >= 2024) {
      raw_name <- paste0((year_n -2000), "nsjin")
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))

      new_df <- readxl::read_xlsx(file_name) |>  
        select(-c(7, 8, 14, 15, 22, 23)) 
    }
    
    colnames(new_df) = list_colname
  } else if (nationality == "overseas") {
    raw_name <- paste0((year_n -2000), "11gsjin")

    if (year_n <= 2013) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xls'))

      new_df <-  readxl::read_xls(file_name, col_names = FALSE) |> 
        select(-c(12, 13, 14, 21, 22, 26, 28, 30))  

    } else if (year_n <= 2020) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xls'))

      new_df <- readxl::read_xls(file_name, col_names = FALSE) |> 
        select(-c(12, 13, 20, 21, 25, 27, 29))

    } else if (year_n <= 2023) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))

      new_df <- readxl::read_xlsx(file_name, col_names = FALSE) |>  
        select(-c(12, 13, 20, 21,  25, 27, 29))
      
    } else if(year_n >= 2024) {
      raw_name <- paste0((year_n -2000), "gsjin")
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))

      new_df <- readxl::read_xlsx(file_name, col_names = FALSE) |> 
        select(-c(12, 13, 20, 21))
    }
    colnames(new_df) = list_colname
  } else if(nationality == "both") {

    raw_name <- paste0((year_n -2000), "03ssjin")

    if (year_n <= 2020) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xls'))
      
      new_df <- readxl::read_xls(file_name, col_names = FALSE) |> 
        select(-c(21, 23, 25))
    } else if(year_n <= 2023) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))
      
      new_df <- readxl::read_xlsx(file_name, col_names = FALSE) |> 
        select(-c(21, 23, 25))
    } else if(year_n >= 2024) {

      raw_name <- paste0((year_n -2000), "ssjin")
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))

      new_df <- readxl::read_xlsx(file_name, col_names = FALSE) |>
      select(-c(21, 23, 25))
    }
  colnames(new_df) <- list_colname
  }

  output_df <- new_df |>
    mutate(year = year_n, .after = city_name) |> 
    slice(c(-1:-4)) |> 
    ungroup()

  return(output_df)
  
}

clean_population_data <- function(nationality_i) {

  # 日本語の列名を英語に変換
  list_colname <- generate_list_cols()
  list_year <- seq(2013, 2022)

  # aggregate_population(nationality_i, list_year, list_cols) |>
  aggregate_population("japanese", list_year, list_cols) |>
    change_cols_class()
  
}


change_year <- function(df) {

  df_output <- df |>
    mutate(
      year = as.numeric(year),
      year = year - 1
    )
  
  return(df_output)
}





# merge municipalities ----------------------------------------------------------------

read_correspondent_table <- function() {

    readxl::read_xlsx(
      here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))

}


generate_list_id_2020 <- function(df) {

  list_id <- read_correspondent_table() |> 
    distinct(id_muni2020) |> 
    pull()
}


extract_new_old_id <- function(id_i, df_merge_municipalities){

  list_munis <- df_merge_municipalities |> 
    dplyr::filter(id_muni2020 == id_i) |>
    select(seq(2,10)) |> 
    unlist() |>
    unique() |>
    na.omit()

  tibble(
    "id_2020" = id_i,
    "id_all" = list_munis
  )
}


#' Generate Municipality Merge Data Frame
#'
#' Creates a data frame mapping old and new municipality IDs based on 2020 base identifiers.
#'
#' @return A data frame with municipality ID correspondences.
#'
generate_df_merge_muni <- function(){

  # read merge data frame
  df_merge_municipalities <- read_correspondent_table()
  # extract base id at 2020
  list_id <- generate_list_id_2020()
  # generate correspondent table
  df_merge_id <- purrr::map_dfr(list_id, extract_new_old_id, df_merge_municipalities)

  return(df_merge_id)
}


# Merge dataframes

merge_dfs <- function(df_population, df_new_old_id) {

  df_new_old_id |> 
    left_join(df_population, join_by(id_all == city_id))  |>
    rename(city_id = id_2020)

}


summarise_vars <- function(df_merged) {

  list_vars <- setdiff(generate_list_cols(), c("prefecture_name", "city_name"))

  df_merged |>
    reframe(
      across(any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = c(year, city_id)
    ) 
}

  

generate_muni_pref_name <- function(df_merged, df_population) {

  list_muni_id <- unique(df_merged$city_id)

  df_population |>
    dplyr::filter(
      year == 2020, 
      city_id %in% list_muni_id
      ) |>
      distinct(
        city_id, 
        city_name,
        prefecture_name
      )
}


add_muni_pref_name <- function(df_merged, df_population) {

  df_muni_pref_name <- generate_muni_pref_name(df_merged, df_population)

  df_merged |>
    left_join(df_muni_pref_name, by = "city_id") |> 
    relocate(c(city_name, prefecture_name), .after = city_id) |>
    relocate(year, .after = prefecture_name)

}


#' Adjust Municipal Population Data
#'
#' Adjusts municipal population data by merging with ID mapping, summarizing variables,
#' and adding municipal/prefectural names.
#'
#' @param df Population data frame
#' @param df_new_old_id_mapping Municipal ID mapping data frame
#' @return Adjusted population data frame
adjust_muni_population <- function(df, df_new_old_id_mapping) {

  df |>
    merge_dfs(df_new_old_id_mapping) |> 
    summarise_vars() |>
    add_muni_pref_name(df) |>
    dplyr::filter(!is.na(year)) 

}

# Add lag variables ----------------------------------------------------------------
add_lag_variables <- function(df_input) {
  
  df_based <- df_input |>
    mutate(
      lag_total = dplyr::lag(total),
      change_rate = change/dplyr::lag(total)*100,
      natural_rate = natural/dplyr::lag(total)*100,
      social_rate = (social/dplyr::lag(total))*100,
      ln_total = log(total),
      ln10_total = log10(total),
      lag_ln_total = dplyr::lag(ln_total),
      lag_ln_total_5 = dplyr::lag(ln_total, n = 5),
      ln_change_rate_total = ln_total - lag_ln_total,
      ln_change_rate_total_5 = ln_total - lag_ln_total_5,
      .by = city_id
    ) |> 
    select(
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
      moving_in_others,
      increase_total,
      moving_out_dom,
      moving_out_int,
      moving_out_total,
      mortality,
      moving_out_others,
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
      ln_change_rate_total_5
    )

  return(df_based)
  
}