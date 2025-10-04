main <- function() {

  source(here::here("02_clean", "population", "funs_population.r"))
  
  # 日本語の列名を英語に変換
  list_colname <- create_list_colname()

  year_list <- seq(2013, 2022)

  non_numeric_variables <- c("prefecture_name", "city_name")

  df_new_old_id_mapping <- generate_df_merge_muni()

  # 3) 対応表に基づいて人口を再集計
  df_population_adjusted_refactored <- adjust_muni_population(df_population, df_new_old_id_mapping)

  # clean data ---------------------------------------------------------------------
  # Japanese
  df_jap_raw <- purrr::map(year_list,
                       read_population_year,
                       list_colname = list_colname,
                       nationality = "japanese") |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "japanese",
                  .after = city_name) |>
    dplyr::mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) 
  
  # Foreigners
  df_for_raw <- purrr::map(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = "overseas") |>
    dplyr::bind_rows() |> 
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "overseas",
                  .after = city_name) |>
    dplyr::mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 

  # both
  df_both_raw <- purrr::map(
    year_list,
    aggregate_pop,
    list_colname = list_colname,
    nationality = "both"
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "both",
                  .after = city_name) |>
    dplyr::mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 

  df_jp_adjusted <- df_jap_raw |>
    adjust_munis() |>
    sumamrise_new_munis() |>
    extract_muni_2020()

  df_for_adjusted <- df_for_raw |>
    adjust_munis() |>
    sumamrise_new_munis() |>
    extract_muni_2020()

  df_both_adjusted <- df_both_raw |>
    adjust_munis() |>
    sumamrise_new_munis() |>
    extract_muni_2020()

  # add lag variables
  df_jp_master <- add_lag_variables(df_jp_adjusted) |>
    add_city_pref_name()
  df_for_master <- add_lag_variables(df_for_adjusted) |>
    add_city_pref_name()
  df_both_master <- add_lag_variables(df_both_adjusted) |>
    add_city_pref_name()

  # save -----------------------------------------------------------------
  write.csv(df_jp_master, here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_for_master, here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_both_master, here::here("01_data", "intermediate", "population", "both_master.csv"), fileEncoding = "cp932", row.names = FALSE)

}

read_population_data <- function(nationality_i) {

    purrr::map_dfr(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = nationality_i)
}


# main()

