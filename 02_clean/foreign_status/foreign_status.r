main <- function() {
  
  # generate constants ---------------------------------------------------------------------
  # 年ごとに在留区分が変わるため列名を分けている
  list_cols <- generate_colnames_by_year()

  df_filenames <- generate_df_filenames()

  # 2010 - 2021(Dec), 2022(Jun)
  list_year <- rev(seq(2010, 2022))

  df_status <- purrr::map(list_year, create_df_foreign, list_cols, df_filenames) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate_all(~stringr::str_replace_all(., "-", "0")) |> 
    dplyr::mutate_at(dplyr::vars(-prefecture_name), as.numeric) |> 
    dplyr::filter(!prefecture_name  %in% c("第５表　都道別　在留資格別　在留外国人　（総　数）",
                                           "総数",
                                           "第５表　都道府県別　在留資格別　在留外国人　（総　数）",
                                           "未定・不詳",
                                           "未定・不祥"))

  df_status <- streamline_status(df_status)

  df_status <- add_log_variables(df_status) 

  write.csv(df_status, here::here("01_data", "intermediate", "foreign_status", "foreign_status.csv"), fileEncoding = "cp932", row.names = FALSE)
}


generate_colnames_by_year <- function() {

  # 年ごとに在留区分が変わるため列名を分けている
  l_col_1 <- create_list_06_09()
  l_col_2 <- create_list_10_11()
  l_col_3 <- create_list_12_14()
  l_col_4 <- create_list_15_16()
  l_col_5 <- create_list_17_18()
  l_col_6 <- create_list_19_22()
  
  list_cols <- list(
    "year_06_09" = l_col_1,
    "year_10_11" = l_col_2,
    "year_12_14" = l_col_3,
    "year_15_16" = l_col_4,
    "year_17_18" = l_col_5,
    "year_19_22" = l_col_6
    )

  return(list_cols)

}


generate_df_filenames <- function() {

    df_filenames <- dplyr::tribble(
    ~"year", ~"file_name",
    "2010", "10-99-04-0.xls",
    "2011", "11-99-04-0.xls",
    "2012", "12-12-05-0.xls",
    "2013", "13-12-05-0.xlsx",
    "2014", "14-12-05-0.xlsx",
    "2015", "15-12-05-0.xlsx",
    "2016", "16-12-05-0.xlsx",
    "2017", "17-12-05-0.xlsx",
    "2018", "18-12-05-0.xlsx",
    "2019", "19-12-05.xlsx",
    "2020", "20-12-05.xlsx",
    "2021", "21-12-03-2.xlsx",
    "2022", "22-06-03-2.xlsx" # 2022年のみ6月
  )

  return(df_filenames)
}


create_df_foreign <- function(year_n, list_cols, df_filenames) {
  print(year_n)

  file_name <- df_filenames |>
    dplyr::filter(year == year_n) |>
    dplyr::pull(file_name)
  
  if(year_n <= 2010) {

    df_based <- readxl::read_xls(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                 col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_10_11"]])
    
  } else if (year_n <= 2011) {

    df_based <- readxl::read_xls(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                 col_names = FALSE) |> 
      dplyr::select(-1) 
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_10_11"]])
    
  } else if (year_n <= 2012) {

      df_based <- readxl::read_xls(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                   col_names = FALSE) |> 
        dplyr::select(-1)
      
      colnames(df_based) <- c("prefecture_name", list_cols[["year_12_14"]])
  } else if (year_n <= 2014) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_12_14"]])
  } else if (year_n <= 2016) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_15_16"]])
    
  } else if (year_n <= 2018) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_17_18"]])
    
  } else if (year_n <= 2020) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) 
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_19_22"]])
    
    df_based <- df_based |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "県", "")) |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "府", "")) |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京"))
  } else if (year_n <= 2022) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                  col_names = FALSE) |> 
      dplyr::select(-1)

    colnames(df_based) <- c("prefecture_name", list_cols[["year_19_22"]])

  df_based <- df_based |> 
    dplyr::filter(stringr::str_detect(prefecture_name, "県|京都府|大阪府|東京都|北海道")) |> 
    dplyr::filter(!stringr::str_detect(prefecture_name, "都道府県市区町村|山県市")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "県", "")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "府", "")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京"))
  }

  df_output <- df_based |> 
    tidyr::drop_na(prefecture_name) |>
    dplyr::mutate(year = year_n, .after = prefecture_name)

  return(df_output)
}


create_list_06_09 <- function() {

  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Investor_or_Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer",
                   "Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Cultural_Activities",
                   "Temporary_Visitor",
                   "Student",
                   "Student_High_School",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Specific_Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Unacquired",
                   "Temporary_Protect",
                   "Other")
  
  
  return(list_output)  
  
}


create_list_10_11 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Investor_or_Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer",
                   "Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Cultural_Activities",
                   "Temporary_Visitor",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Specific_Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Unacquired",
                   "Temporary_Protect",
                   "Other")
  
  
  return(list_output)  
  
}


create_list_12_14 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Investor_or_Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer",
                   "Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  
  
  return(list_output)  
  
}


create_list_15_16 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Highly_Skilled_Professional_1_a",
                   "Highly_Skilled_Professional_1_b",
                   "Highly_Skilled_Professional_1_c",
                   "Highly_Skilled_Professional_2",
                   "Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer_or_Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  
  
  return(list_output)  
  
}


create_list_17_18 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Highly_Skilled_Professional_1_a",
                   "Highly_Skilled_Professional_1_b",
                   "Highly_Skilled_Professional_1_c",
                   "Highly_Skilled_Professional_2",
                   "Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer_or_Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Nursing_Care",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Technical_Intern_Training_3_a",
                   "Technical_Intern_Training_3_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  

  return(list_output)  
  
}


create_list_19_22 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Highly_Skilled_Professional_1_a",
                   "Highly_Skilled_Professional_1_b",
                   "Highly_Skilled_Professional_1_c",
                   "Highly_Skilled_Professional_2",
                   "Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer_or_Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Nursing_Care",
                   "Entertainer",
                   "Skill",
                   "Specified_Skilled_Worker_1",
                   "Specified_Skilled_Worker_2",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Technical_Intern_Training_3_a",
                   "Technical_Intern_Training_3_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  

  return(list_output)  
  
}


create_status_jap <- function(df_foreign) {
  
  df_status_eng <- dplyr::tibble(
    eng = colnames(df_foreign)
    ) |> 
    dplyr::mutate(id = row_number(), .before = eng)
  
  list_status_jap <- c("prefecture_name",
                       "year",
                       "総数",
                       "教授",
                       "芸術",
                       "宗教",
                       "報道",
                       "高度専門職１号イ",
                       "高度専門職１号ロ",
                       "高度専門職１号ハ",
                       "高度専門職２号",
                       "経営_管理",
                       "法律_会計業務",
                       "医療",
                       "研究",
                       "教育",
                       "技術_人文知識_国際",
                       "企業内転勤",
                       "介護",
                       "興行",
                       "技能",
                       "特定技能１号",
                       "特定技能２号",
                       "技能実習１号イ",
                       "技能実習１号ロ",
                       "技能実習２号イ",
                       "技能実習２号ロ",
                       "技能実習３号イ",
                       "技能実習３号ロ",
                       "文化活動",
                       "留学",
                       "研修",
                       "家族滞在",
                       "特定活動",
                       "永住者",
                       "日本人の配偶者等",
                       "永住者の配偶者等",
                       "定住者",
                       "特別永住者",
                       "投資_経営",
                       "技術",
                       "人文知識_国際",
                       "短期滞在",
                       "未取得者",
                       "一時庇護",
                       "その他")
  
  df_status_jap <- tibble(jap = list_status_jap) |> 
    dplyr::mutate(id = row_number(), .before = jap)
  
  df_output <- df_status_eng |> 
    dplyr::left_join(df_status_jap) 
    # pivot_wider(names_from = c(eng), values_from = c(jap))
  
  return(df_output)
  
}


streamline_status <- function(df_foreign) {
  
  # 「技術」と「人文知識・国際業務」を統合
  #「経営・管理」と「投資・経営」
  df_based <- df_foreign |> 
    dplyr::group_by(prefecture_name, year) |>  
    dplyr::mutate(Engineer_or_Specialist_in_Humanities_or_International_Services = 
                    if_else(is.na(Engineer), 
                            Engineer_or_Specialist_in_Humanities_or_International_Services,
                            sum(Engineer, Specialist_in_Humanities_or_International_Services, na.rm = TRUE))) |> 
    dplyr::select(-Engineer, -Specialist_in_Humanities_or_International_Services) |> 
    dplyr::mutate(Business_Manager = 
                    if_else(is.na(Investor_or_Business_Manager), 
                            Business_Manager,
                            Investor_or_Business_Manager)) |> 
    dplyr::select(-Investor_or_Business_Manager) |>
    dplyr::ungroup()
  

  df_except <- df_based |> 
    dplyr::select(-Total)
  
  # 区分に分けて合計を算出
  df_output <- df_based |> 
    dplyr::summarise(total_foreign = 
                       Total,
                     high_skill = 
                       sum(Professor,
                           Art,
                           Religion,
                           Report,
                           Highly_Skilled_Professional_1_a,
                           Highly_Skilled_Professional_1_b,
                           Highly_Skilled_Professional_1_c,
                           Highly_Skilled_Professional_2,
                           Business_Manager,
                           Legal_and_Accounting_Services,
                           Medical_Services,
                           Research,
                           Educate,
                           Engineer_or_Specialist_in_Humanities_or_International_Services,
                           Intra_Company_Transferee,
                           Nursing_Care,
                           Skill, na.rm = TRUE),
                     low_skill =
                       sum(Specified_Skilled_Worker_1,
                           Specified_Skilled_Worker_2,
                           Technical_Intern_Training_1_a,
                           Technical_Intern_Training_1_b,
                           Technical_Intern_Training_2_a,
                           Technical_Intern_Training_2_b,
                           Technical_Intern_Training_3_a,
                           Technical_Intern_Training_3_b, na.rm = TRUE),
                     training =
                       sum(Technical_Intern_Training_1_a,
                           Technical_Intern_Training_1_b,
                           Technical_Intern_Training_2_a,
                           Technical_Intern_Training_2_b,
                           Technical_Intern_Training_3_a,
                           Technical_Intern_Training_3_b, na.rm = TRUE),
                     status =
                       sum(Permanent_Resident,
                           Spouse_or_Child_of_Japanese_National,
                           Spouse_or_Child_of_Permanent_Resident,
                           Long_Term_Resident, na.rm = TRUE),
                     specific_permanent_resident = Specific_Permanent_Resident,
                     other = sum(Unacquired, 
                                 Student, 
                                 Temporary_Protect,
                                 Entertainer,
                                 Cultural_Activities,
                                 Trainee, 
                                 Designated_Activities,
                                 Dependent,
                                 Other, 
                                 Temporary_Visitor, na.rm = TRUE),
                                 .by = c("prefecture_name", "year")) |> 
    dplyr::mutate(except_specific = total_foreign - specific_permanent_resident) |>
    ungroup()

  return(df_output)
  
}


add_log_variables <- function(df_input) {
  
  df_based <- df_input |> 
    dplyr::group_by(prefecture_name) |> 
    dplyr::arrange(prefecture_name, year) |>
    dplyr::mutate(
      ln_foreign        = log(total_foreign),
      lag_ln_foreign    = dplyr::lag(ln_foreign),
      lag_ln_foreign_2  = dplyr::lag(ln_foreign, n = 2),
      lag_ln_foreign_3  = dplyr::lag(ln_foreign, n = 3),
      lag_ln_foreign_5  = dplyr::lag(ln_foreign, n = 5),
      lag_ln_foreign_10 = dplyr::lag(ln_foreign, n = 10),
      ln_change_rate_foreign    = ln_foreign - lag_ln_foreign,
      ln_change_rate_foreign_2  = ln_foreign - lag_ln_foreign_2,
      ln_change_rate_foreign_3  = ln_foreign - lag_ln_foreign_3,
      ln_change_rate_foreign_5  = ln_foreign - lag_ln_foreign_5,
      ln_change_rate_foreign_10 = ln_foreign - lag_ln_foreign_10,
      .after = total_foreign
    ) |>
    dplyr::mutate(
      ln_high = log(high_skill),
      lag_ln_high = dplyr::lag(ln_high),
      lag_ln_high_2 = dplyr::lag(ln_high, n = 2),
      lag_ln_high_3 = dplyr::lag(ln_high, n = 3),
      lag_ln_high_5 = dplyr::lag(ln_high, n = 5),
      ln_change_rate_high = ln_high - lag_ln_high,
      ln_change_rate_high_2 = ln_high - lag_ln_high_2,
      ln_change_rate_high_3 = ln_high - lag_ln_high_3,
      ln_change_rate_high_5 = ln_high - lag_ln_high_5,
      .after = high_skill
    ) |>
    dplyr::mutate(
      ln_low = log(low_skill),
      lag_ln_low = dplyr::lag(ln_low),
      lag_ln_low_2 = dplyr::lag(ln_low, n = 2),
      lag_ln_low_3 = dplyr::lag(ln_low, n = 3),
      lag_ln_low_5 = dplyr::lag(ln_low, n = 5),
      ln_change_rate_low = ln_low - lag_ln_low,
      ln_change_rate_low_2 = ln_low - lag_ln_low_2,
      ln_change_rate_low_3 = ln_low - lag_ln_low_3,
      ln_change_rate_low_5 = ln_low - lag_ln_low_5,
      .after = low_skill
    ) |>
    dplyr::mutate(
      ln_status = log(status),
      lag_ln_status = dplyr::lag(ln_status),
      lag_ln_status_2 = dplyr::lag(ln_status, n = 2),
      lag_ln_status_3 = dplyr::lag(ln_status, n = 3),
      lag_ln_status_5 = dplyr::lag(ln_status, n = 5),
      ln_change_rate_status = ln_status - lag_ln_status,
      ln_change_rate_status_2 = ln_status - lag_ln_status_2,
      ln_change_rate_status_3 = ln_status - lag_ln_status_3,
      ln_change_rate_status_5 = ln_status - lag_ln_status_5,
      .after = status
    ) |>
    dplyr::mutate(
      ln_sp_residents = log(specific_permanent_resident),
      lag_ln_sp_residents = dplyr::lag(ln_sp_residents),
      lag_ln_sp_residents_2 = dplyr::lag(ln_sp_residents, n = 2),
      lag_ln_sp_residents_3 = dplyr::lag(ln_sp_residents, n = 3),
      lag_ln_sp_residents_5 = dplyr::lag(ln_sp_residents, n = 5),
      ln_change_rate_sp_residents = ln_sp_residents - lag_ln_sp_residents,
      ln_change_rate_sp_residents_2 = ln_sp_residents - lag_ln_sp_residents_2,
      ln_change_rate_sp_residents_3 = ln_sp_residents - lag_ln_sp_residents_3,
      ln_change_rate_sp_residents_5 = ln_sp_residents - lag_ln_sp_residents_5,
      .after = specific_permanent_resident
    ) |>
    dplyr::mutate(
      ln_training = log(training),
      lag_ln_training = dplyr::lag(ln_training),
      lag_ln_training_2 = dplyr::lag(ln_training, n = 2),
      lag_ln_training_3 = dplyr::lag(ln_training, n = 3),
      lag_ln_training_5 = dplyr::lag(ln_training, n = 5),
      ln_change_rate_training = ln_training - lag_ln_training,
      ln_change_rate_training_2 = ln_training - lag_ln_training_2,
      ln_change_rate_training_3 = ln_training - lag_ln_training_3,
      ln_change_rate_training_5 = ln_training - lag_ln_training_5,
      .after = training
    ) |>
    dplyr::mutate(
      status_total = status + specific_permanent_resident,
      # 身分系に特別永住者を含めた変数
      ln_status_total = log(status),
      lag_ln_status_total = dplyr::lag(ln_status_total),
      lag_ln_status_total_2 = dplyr::lag(ln_status_total, n = 2),
      lag_ln_status_total_3 = dplyr::lag(ln_status_total, n = 3),
      lag_ln_status_total_5 = dplyr::lag(ln_status_total, n = 5),
      ln_change_rate_status_total = ln_status_total - lag_ln_status_total,
      ln_change_rate_status_total_2 = ln_status_total - lag_ln_status_total_2,
      ln_change_rate_status_total_3 = ln_status_total - lag_ln_status_total_3,
      ln_change_rate_status_total_5 = ln_status_total - lag_ln_status_total_5,
      .after = ln_change_rate_sp_residents_5
    ) |>
    dplyr::mutate(
      ln_except_specific = log(except_specific),
      lag_ln_except_specific = dplyr::lag(ln_except_specific),
      lag_ln_except_specific_2 = dplyr::lag(ln_except_specific, n = 2),
      lag_ln_except_specific_3 = dplyr::lag(ln_except_specific, n = 3),
      lag_ln_except_specific_5 = dplyr::lag(ln_except_specific, n = 5),
      lag_ln_except_specific_10 = dplyr::lag(ln_except_specific, n = 10),
      ln_change_rate_except_specific = ln_except_specific - lag_ln_except_specific,
      ln_change_rate_except_specific_2 = ln_except_specific - lag_ln_except_specific_2,
      ln_change_rate_except_specific_3 = ln_except_specific - lag_ln_except_specific_3,
      ln_change_rate_except_specific_5 = ln_except_specific - lag_ln_except_specific_5,
      ln_change_rate_except_specific_10 = ln_except_specific - lag_ln_except_specific_10,
      .after = ln_change_rate_except_specific_5
    ) |>
    dplyr::ungroup()
  
  return(df_based)
  
}



main()
