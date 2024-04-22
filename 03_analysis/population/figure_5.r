main <- function(){

  # load data ####  
  df_for <- read.csv(here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932") # 外国人
  

# create plot -------------------------------------------------------------
  
  plot_for <- create_scatter(df_for,
                             nationality = "overseas",
                             title_x = "日本人 人口(ln)")

# save plot ---------------------------------------------------------------
  
  ggsave(plot_for, filename = here::here("04_output", "figures", "figure_5.png"),
         width = 10,
         height = 12)
 
}

create_scatter <-  function(input_df, 
                            nationality,
                            title_x){
  
  plot_based_df <- input_df |> 
    dplyr::filter(!year %in% c(2013, 2014))
  
  based_plot <- ggplot(plot_based_df,
                       aes(x = lag_ln_total, y = ln_change_rate_total)) +
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
    labs(
      x = title_x,
      y = "対数変化率（ln）") +
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
  
  if (nationality == "japanese") {
    output_plot <- based_plot +
      scale_x_continuous(breaks = c(5, 10, 15)) +
      scale_y_continuous(breaks = c(-0.1, -0.05, 0,
                                    0.05, 0.1),
                         limits = c(-0.1, 0.1))
      
  } else if (nationality == "overseas") {
    output_plot <- based_plot +
      scale_x_continuous(
        breaks = c(5, 10)) +
      scale_y_continuous(
        # breaks = c(-1, -0.5, 0,0.5, 1),
        breaks = c(-2, -1, 0, 1, 2),
        limits = c(-2, 2)
        )
    
    
  } else if (nationality == "both") {
    output_plot <- based_plot +
      scale_x_continuous(breaks = c(5, 10, 15),
                         limits = c(4.5, 15.5)) +
      scale_y_continuous(breaks = c(-1, -0.5, 0,
                                    0.5, 1),
                         limits = c(-1, 1))
    
    
  }
  
  return(output_plot)
}


