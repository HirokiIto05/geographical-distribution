main <- function() {
  
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
  
  openxlsx::write.xlsx(df_status_year, here::here("04_output", "tables", "appendix_table_2.xlsx"))
  
}

create_list_status <- function(year_n) {

  if (year_n < 2013) {
    file_name <- paste0(year_n, ".xls")
    df_based <- readxl::read_xls(here::here('01_data', "raw", 'foreign_residents', file_name),
                                 col_names = FALSE) |> 
      dplyr::slice(3) 
  } else if(year_n <= 2020) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(here::here('01_data', "raw", 'foreign_residents', file_name),
                                  col_names = FALSE) |> 
      dplyr::slice(3)
  } else if(year_n >= 2021) {
    
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(here::here('01_data', "raw", 'foreign_residents', file_name),
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

main()