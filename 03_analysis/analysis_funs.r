
#' @description Select columns for Japan data
select_cols_jp <- function(df_jp) {

  df_output <- df_jp |>
    select(
      county_id = city_id, county_name = city_name, prefecture_name, year, total,
      add = moving_in_total,
      add_internal = moving_in_dom,
      add_external = moving_in_int,
      add_others = moving_in_others,
      birth,
      exit = moving_out_total,
      exit_internal = moving_out_dom,
      exit_external = moving_out_int,
      exit_others = moving_out_others,
      death = mortality
    )
}

#' @description Select columns for Japan data
select_cols_de <- function(df_de) {

  df_output <- df_de |>
    select(
      county_id, county_name, year, total = total_value,
      add = additions,
      add_internal = addition_internal,
      add_external = addition_international,
      birth = addition_by_birth,
      exit = exits,
      exit_internal = exit_internal,
      exit_external = extis_international,
      death = exit_by_death
    )
}

#' @title Create analysis data
#' @description Add a regressor and outcome with lagged data.
#' At that time, winsorize the top and bottom 1%
create_scatter_df <- function(input_df, var_y, cutoff = 1, lag_i = 1) {

  var_y <- enquo(var_y)
  
  plot_based_df <- input_df |> 
    arrange(county_id, year) |> 
    mutate(
      ln_total = log(!!var_y),
      lag_ln_total = dplyr::lag(log(!!var_y), n = lag_i),
      ln_change_rate_total = log(!!var_y) - dplyr::lag(log(!!var_y), n = lag_i),
      .by = county_id
    ) |>  
    mutate(ln_change_rate_total = Winsorize(ln_change_rate_total, val = quantile(ln_change_rate_total, probs = c(0.01, 0.99), na.rm = TRUE)), .by = year)  |> 
    dplyr::slice_max(prop = cutoff, order_by = total, by = "year")
}

#' @title Create a basic scatter plot
#' 
#' @param lag_i The number of lags
create_scatter <-  function(input_df, var_x, cutoff = 1, lag_i = 1){

  var_x <- enquo(var_x)

  output_plot <- ggplot(input_df,
                       aes(x = !!var_x, y = ln_change_rate_total)) +
    geom_point(alpha = 0.5, colour = "#333333",
               fill = "#333333") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      panel.grid.major.y = element_line(color = "lightgray"),
      strip.background = element_blank(),
      strip.text = element_text(size = 22),
      strip.text.x = element_text(size = 20)
      ) +
    geom_hline(yintercept = 0,
               linewidth = 0.6,
               colour = "black",
               linetype = "solid") +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.3) +
    facet_wrap(~ year,
               scales = "free") 
    # scale_x_continuous(breaks = c(5, 10)) +
    # scale_y_continuous(
    #   breaks = c(-2, -1, 0, 1, 2),
    #   limits = c(-2, 2)
    # )
  
  return(output_plot)
}


#' @title execute a basic lm model
create_lm <- function(df_input, var_y, var_x) {

  df <- df_input

  formula_i <- paste0(var_y, "~", var_x)
  
  model_output <- list(
    "2014" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2014)),
    "2015" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2015)),
    "2016" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2016)),
    "2017" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2017)),
    "2018" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2018)),
    "2019" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2019)),
    "2020" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2020)),
    "2021" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2021)),
    "2022" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2022))
    # "2023" = lm_robust(formula = as.formula(formula_i), 
    #                     data = dplyr::filter(df, year == 2023))
    )
  return(model_output)
}


#' @title Create a model summary
#' @param model_input List : list of the result of lm_robust()
create_model_summary <- function (model_input, title_n) {
  
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
                                        gof_omit = 'AIC|BIC|RMSE',
                                        # output = "data.frame")
                                        output = "html")
  
  results_model <- model_based 
  
  return(results_model)
  
}