main <- function() {
  
  # read data -----------------------------------
  df_master <- read.csv(here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932")

  # estimates ------------------------------------
  model_status_total <- create_lm_status_total(df_master, top_n = 1)

  model_except_five <- lm_five_except(df_master)
  model_except_five
  table_except_five <- create_lm_kable(model_except_five, "特別永住者以外")

  table_except_five

  write.xlsx(table_except_five, file = "04_analyze/foreign_status/table/modified/except_five.xlsx")


model_total_five <- lm_five_total(df_master)
model_total_five
table_total_five <- create_lm_kable(model_total_five, "外国人　総人口")

table_total_five

write.xlsx(table_total_five, file = "04_analyze/foreign_status/table/modified/total_five.xlsx")


  model_total_three <- lm_three_total(df_master)
  model_total_five <- lm_five_total(df_master)
  table_total_five <- create_lm_kable(model_total_five, "外国人　総人口")
  table_total_five
  # table_total_three <- create_lm_kable(model_total_three, "外国人　総人口")

  table_total_three
  write.xlsx(table_total_three, file = here::here("04_output", "tables", "figure_7.xlsx"))
  
  model_except_three <- lm_three_except(df_master)
  table_except_three <- create_lm_kable(model_except_three, "特別永住者以外")

  model_total_three |> View()
  
  write.xlsx(table_except_three, file = here::here("04_output", "tables", "figure_7.xlsx"))
  
  model_total_five <- lm_five_total(df_master)
  model_total_five
  table_total_five <- create_lm_kable(model_total_five, "外国人　総人口")
  
  table_total_five
  write.xlsx(table_total_five, file = "04_analyze/foreign_status/table/total_five.xlsx")
  
  
  model_except_five <- lm_five_except(df_master)
  model_except_five
  table_except_five <- create_lm_kable(model_except_five, "特別永住者以外")
  
  table_except_five
  
  write.xlsx(table_except_five, file = "04_analyze/foreign_status/table/except_five.xlsx")
}

create_lm_kable <- function (model_input, title_n, col_n) {

  gm <- tibble(
    raw = c("nobs", "r.squared", "adj.r.squared"),
    clean = c("N", "R2", "R2 adj"),
    fmt = c(0, 3, 3)
  )
  
  model_based <- modelsummary::msummary(model_input, fmt = "%.4f", 
                                        estimate =  "{estimate}{stars}",
                                        stars = c('*' = .1, '**' = .05, '***' = .01),
                                        coef_rename = c("ln_lag_total" = "β1"),
                                        gof_map = gm,
                                        output = "data.frame")
  
  results_model <- model_based 
    # add_header_above(c(setNames(col_n, title_n)))
  
  return(results_model)
  
}



lm_three_total <- function(df_input) {
  
  list_output <- list(
  
  "2012 - 2015" = lm_robust(formula = ln_change_rate_foreign_3 ~  lag_ln_foreign_3,
                            data = dplyr::filter(df_input, year == 2015)),
  "2016 - 2019" = lm_robust(formula = ln_change_rate_foreign_3 ~  lag_ln_foreign_3,
                            data = dplyr::filter(df_input, year == 2019)),
  "2020 - 2022" = lm_robust(formula = ln_change_rate_foreign_2 ~  lag_ln_foreign_2,
                            data = dplyr::filter(df_input, year == 2022))
  
  )
  
  return(list_output)
}

lm_three_except <- function(df_input) {
  
  list_output <- list(
    
    "2012 - 2015" = lm_robust(formula = ln_change_rate_except_specific_3 ~  lag_ln_except_specific_3,
                              data = dplyr::filter(df_input, year == 2015)),
    "2016 - 2019" = lm_robust(formula = ln_change_rate_except_specific_3 ~  lag_ln_except_specific_3,
                              data = dplyr::filter(df_input, year == 2019)),
    "2020 - 2022" = lm_robust(formula = ln_change_rate_except_specific_2 ~  lag_ln_except_specific_2,
                              data = dplyr::filter(df_input, year == 2022))
    
  )
  
  return(list_output)
}


lm_five_total <- function(df_input) {
  
  list_output <- list(
  
  "2010 - 2015" = lm_robust(formula = ln_change_rate_foreign_5 ~  lag_ln_foreign_5,
                            data = dplyr::filter(df_input, year == 2015)),
  "2015 - 2020" = lm_robust(formula = ln_change_rate_foreign_5 ~  lag_ln_foreign_5,
                            data = dplyr::filter(df_input, year == 2020)),
  "2010 - 2020" = lm_robust(formula = ln_change_rate_foreign_10 ~  lag_ln_foreign_10,
                            data = dplyr::filter(df_input, year == 2020))
  
  )
  
  return(list_output)
}

lm_five_except <- function(df_input) {
  
  list_output <- list(
    
    "2010 - 2015" = lm_robust(formula = ln_change_rate_except_specific_5 ~  lag_ln_except_specific_5,
                              data = dplyr::filter(df_input, year == 2015)),
    "2015 - 2020" = lm_robust(formula = ln_change_rate_except_specific_5 ~  lag_ln_except_specific_5,
                              data = dplyr::filter(df_input, year == 2020)),
    "2010 - 2020" = lm_robust(formula = ln_change_rate_except_specific_10 ~  lag_ln_except_specific_10,
                              data = dplyr::filter(df_input, year == 2020))
    
  )
  
  return(list_output)
}

main()