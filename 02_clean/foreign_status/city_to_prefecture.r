main <- function(){
  
  # 市区町村人口を都道府県人口に変更
  df_pop <- summarise_prefecture()
  
  df_id <- read_id_df()

  df_add_id <- df_pop |> 
    dplyr::left_join(df_id) |> 
    dplyr::relocate(id, .after = prefecture_name) |> 
    dplyr::mutate(
      prefecture_name = stringr::str_replace_all(prefecture_name, "県", ""),
      prefecture_name = stringr::str_replace_all(prefecture_name, "府", ""),
      prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京")
      ) 
  
  write.csv(df_add_id, here::here("01_data", "intermediate", "foreign_status", "prefecture_pop.csv"), fileEncoding = "cp932", row.names = FALSE)
  
  
}


summarise_prefecture <- function() {

  list_vars <- c(
    "male",
    "female",
    "total",
    "household",
    "moving_in",
    "birth",
    "moving_out",
    "mortality",
    "change",
    "natural",
    "social"
  )

  df_pop <- read.csv(here::here("01_data", "intermediate", "foreign_status", "both_for_foreign_status.csv"), fileEncoding = "cp932") |> 
    dplyr::filter(year %in% seq(2010, 2022)) |> 
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = c("prefecture_name", "year")
    )

  return(df_pop)


}

read_id_df <- function() {
  
    df_id <- readxl::read_xls(here::here("01_data", "raw", "prefecture_code_name", "prefecture_data.xls"), col_names = FALSE) |> 
      dplyr::select(1,2,3)
    
    colnames(df_id) <- c("id", "prefecture_name", "city_name")
    
    df_output <- df_id |>
      dplyr::mutate(dplyr::across(.cols = id, .fns = as.numeric, .names = "id")) |>
      dplyr::filter(is.na(city_name)) |> 
      dplyr::mutate(id = stringr::str_sub(id, start = 1, end = -2)) |> 
      dplyr::select(-city_name)
    
    return(df_output)
  
}
  
