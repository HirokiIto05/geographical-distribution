appid <- "12604ebf628834b46b3867721893644141fe9a34"

pacman::p_load(
    dplyr, 
    tidyr, 
    ggplot2,
    stringr,
    stringi,
    here,
    openxlsx,
    readxl,
    httpgd,
    estimatr,
    modelsummary,
    kableExtra,
    webshot2,
    patchwork,
    cowplot,
    plyr,
    broom,
    openxlsx
    )

conflicted::conflict_prefer("here", "here")
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)
