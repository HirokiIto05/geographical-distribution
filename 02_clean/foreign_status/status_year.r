main <- function() {
  
  # generate constants ---------------------------------------------------------------------
  l_col_1 <- create_list_06_09()
  l_col_2 <- create_list_10_11()
  l_col_3 <- create_list_12_14()
  l_col_4 <- create_list_15_16()
  l_col_5 <- create_list_17_18()
  l_col_6 <- create_list_19_22()
  
  list_cols <- list("year_06_09" = l_col_1,
                    "year_10_11" = l_col_2,
                    "year_12_14" = l_col_3,
                    "year_15_16" = l_col_4,
                    "year_17_18" = l_col_5,
                    "year_19_22" = l_col_6)
  
  list_year <- rev(seq(2010, 2022))
  
  list_prefecture_name <- readxl::read_xls("01_data/raw/prefecture_code_name/prefecture_data.xls") |> 
    dplyr::select(2) |> 
    dplyr::distinct() |> 
    dplyr::pull()
  
  df_foreign <- purrr::map(list_year, create_df_foreign, list_cols,
                           list_prefecture_name) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate_all(~stringr::str_replace_all(., "-", "0")) |> 
    dplyr::mutate_at(dplyr::vars(-prefecture_name), as.numeric) |> 
    dplyr::filter(!prefecture_name  %in% c("第５表　都道別　在留資格別　在留外国人　（総　数）",
                                           "総数",
                                           "第５表　都道府県別　在留資格別　在留外国人　（総　数）"))
  
  write.csv(df_foreign, here::here("01_data", "intermediate", "foreign_status", "foreign_status.csv"), fileEncoding = "cp932", row.names = FALSE)
  openxlsx::write.xlsx(df_foreign, here::here("01_data", "intermediate", "foreign_status", "foreign_status.xlsx"))
  
}


create_df_foreign <- function(year_n, list_cols, list_prefecture_name) {
  print(year_n)
  
  if(year_n <= 2010) {
    
    file_name <- paste0(year_n, ".xls")
    df_based <- readxl::read_xls(paste0("01_data/raw/foreign_residents/", file_name),
                                 col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_10_11"]])
    
  } else if (year_n <= 2011) {
    file_name <- paste0(year_n, ".xls")
    df_based <- readxl::read_xls(paste0("01_data/raw/foreign_residents/", file_name),
                                 col_names = FALSE) |> 
      dplyr::select(-1) 
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_10_11"]])
    
  } else if (year_n <= 2012) {
      file_name <- paste0(year_n, ".xls")
      df_based <- readxl::read_xls(paste0("01_data/raw/foreign_residents/", file_name),
                                   col_names = FALSE) |> 
        dplyr::select(-1)
      
      colnames(df_based) <- c("prefecture_name", list_cols[["year_12_14"]])
  } else if (year_n <= 2014) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(paste0("01_data/raw/foreign_residents/", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_12_14"]])
  } else if (year_n <= 2016) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(paste0("01_data/raw/foreign_residents/", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_15_16"]])
    
  } else if (year_n <= 2018) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(paste0("01_data/raw/foreign_residents/", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_17_18"]])
    
  } else if (year_n <= 2020) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(paste0("01_data/raw/foreign_residents/", file_name),
                      col_names = FALSE) 
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_19_22"]])
    
    df_based <- df_based |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "県", "")) |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "府", "")) |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京"))
  } else if (year_n <= 2022) {
    file_name <- paste0(year_n, ".xlsx")
    df_based <- readxl::read_xlsx(paste0("01_data/raw/foreign_residents/", file_name),
                                  col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_19_22"]])
    
  df_based <- df_based |> 
    dplyr::filter(prefecture_name %in% list_prefecture_name) |> 
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
  
  df_status_eng <- tibble(eng = colnames(df_foreign)) |> 
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
  
  

main()




