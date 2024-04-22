main <- function() {
  
  df_foreign <- read.csv(here::here("01_data", "intermediate", "foreign_status", "foreign_status.csv"), fileEncoding = "cp932")

  df_classified <- classify_cols(df_foreign) 
  
  write.csv(df_classified, here::here("01_data", "intermediate", "foreign_status", "foreign_classified.csv"), fileEncoding = "cp932", row.names = FALSE)
 
}


classify_cols <- function(df_foreign) {
  
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
    select(-Investor_or_Business_Manager) 
  

  df_except <- df_based |> 
    dplyr::select(-Total)
  
  # 区分に分けて合計を算出
  df_classified <- df_based |> 
    ungroup() |> 
    dplyr::group_by(prefecture_name, year) |> 
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
                                 Temporary_Visitor, na.rm = TRUE)) |> 
    dplyr::mutate(except_specific = total_foreign - specific_permanent_resident)

  return(df_classified)
  
}

main()
