main <- function() {
  
  df_jap <- read_df_csv("add_log_variable", "japanese")
  df_for <- read_df_csv("add_log_variable", "overseas")
  
  list_year <- rev(seq(2006, 2022))
  
  list_category <- purrr::map(list_year, create_list_status)
  
  df_status_year <- plyr::join_all(list_category,  by = "key",
                                   type = "left") |> 
    dplyr::relocate(key, .before = y_2022) |>  
    dplyr::mutate(y_2019 = dplyr::lag(y_2019),
                  y_2020 = dplyr::lag(y_2020)) |> 
    rev() |> 
    dplyr::select(-key) |> 
    dplyr::slice_tail(n = 37)
  
  save_df_xlsx(df_status_year, "foreign_status", "status_list")
  
  # status_kbl <-  df_status_year |> 
  #   kbl(align = "c") |> 
  #   kable_styling(font_size = 20)
  # 
  # save_kable(status_kbl, file = "03_build/foreign_status/data/status.pdf")
}

create_list_status <- function(year_n) {

  if (year_n < 2013) {
    file_name <- paste0(year_n, ".xls")
    df_based <- readxl::read_xls(here::here('02_raw', 'foreign_residents', file_name),
                                 col_names = FALSE) |> 
      dplyr::slice(3) 
  } else if(year_n <= 2020) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(here::here('02_raw', 'foreign_residents', file_name),
                                  col_names = FALSE) |> 
      dplyr::slice(3)
  } else if(year_n >= 2021) {
    
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(here::here('02_raw', 'foreign_residents', file_name),
                                  col_names = FALSE) |> 
      dplyr::slice(2) 
  } 
  
  df_int <- df_based  |> 
    pivot_longer(cols = -c(...1)) |> 
    dplyr::distinct(value)
    
  df_output <- df_int |> 
    dplyr::mutate(key = seq(nrow(df_int))) |> 
    tidyr::drop_na()
  
  colnames(df_output) <- c(paste0("y_", year_n), "key")
  
  return(df_output)
  
}