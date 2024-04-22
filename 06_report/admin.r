# Setup -----------------------------------------
source(here::here("05_config", "library.r"))


# Clean -----------------------------------------

# 「住民基本台帳」関連分析
source(here::here("02_clean", "population", "aggregate_pop.r"))
source(here::here("02_clean", "population", "mini_id_adjusted.r"))
source(here::here("02_clean", "population", "master.r"))

# 「在留外国人統計」関連分析
source(here::here("02_clean", "foreign_status", "aggregate_pop_for_foreign_status.r"))
source(here::here("02_clean", "foreign_status", "city_to_prefecture.r"))
source(here::here("02_clean", "foreign_status", "status_year.r"))
source(here::here("02_clean", "foreign_status", "foreign_master.r"))
# source(here::here("02_clean", "foreign_status", ".r"))


# Analyze ---------------------------------------

# 「住民基本台帳」関連分析
source(here::here("03_analysis", "population", "figure_5.r"))
source(here::here("03_analysis", "population", "figure_6.r"))
source(here::here("03_analysis", "population", "table_5.r"))
source(here::here("03_analysis", "population", "table_6.r"))
source(here::here("03_analysis", "population", "applendix_figure_3.r"))

# 「在留外国人統計」関連分析
source(here::here("03_analysis", "foreign_status", "figure_7.r"))
source(here::here("03_analysis", "foreign_status", "figure_8.r"))
source(here::here("03_analysis", "foreign_status", "table_5.r"))
source(here::here("03_analysis", "foreign_status", "appendix_table3.r"))

