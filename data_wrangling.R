# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "data.table",
             "readxl",
             "tidyverse",
             "R.utils",
             "tidylog",
             "lubridate",
             "comorbidity",
             "psych",
             "ggplot2",
             "ggplotgui",
             "ggthemes",
             "arsenal",
             "survival")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

getwd()
rm(list=ls())

# Data import -------------------------------------------------------------

## patient data

patient_data <- read_csv("input/2021102512_1_Patient_data_2021_002_SCE.csv.gz",
                         locale = locale(encoding = "cp932"))

patient_data %>% glimpse()

## disease data

emr_disease_data <- read_csv("input/2021102512_2_EMR_Disease_data_2021_002_SCE.csv.gz",
                             locale = locale(encoding = "cp932"))
emr_disease_data %>% glimpse()

## drug data

emr_drug_data <- read_csv("input/2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz",
                          locale = locale(encoding = "cp932"))
emr_drug_data %>% glimpse()

## admission data

emr_admission_data <- read_csv("input/2021102512_4_EMR_Admission_data_2021_002_SCE.csv.gz",
                               locale = locale(encoding = "cp932"))
emr_admission_data %>% glimpse()

## laboratory data

emr_laboratory_data <- read_csv("input/2021102512_5_EMR_Laboratory_data_2021_002_SCE.csv.gz",
                                locale = locale(encoding = "cp932"))
emr_laboratory_data %>% glimpse()

## Claim disease data

claim_disease_data <- read_csv("input/2021102512_51_Claim_Disease_data_2021_002_SCE.csv.gz",
                               locale = locale(encoding = "cp932"))
claim_disease_data %>% glimpse()

## Claim procedure data

claim_procedure_data <- read_csv("input/2021102512_52_Claim_Procedure_data_2021_002_SCE.csv.gz",
                                 locale = locale(encoding = "cp932"))
claim_procedure_data %>% glimpse()

## EF1

ef1 <- read_csv("input/2021102512_71_DPC_FF1_data_2021_002_SCE.csv.gz",
                locale = locale(encoding = "cp932"))
ef1 %>% glimpse()

# Code lists --------------------------------------------------------------

## drug code

drug_code <- read_csv("input/2021102512_101_Drug_codelist_2021_002_SCE.csv.gz",
                 locale = locale(encoding = "cp932"))
drug_code %>% glimpse()

## disease code

disease_code <- read_csv("input/2021102512_103_Disease_codelist_2021_002_SCE.csv.gz",
                         locale = locale(encoding = "cp932"))
disease_code %>% glimpse()

## procedure code

procedure_code <- read_csv("input/2021102512_105_Procedure_codelist_2021_002_SCE.csv.gz",
                           locale = locale(encoding = "cp932"))
procedure_code %>% glimpse()

# Patient selection -------------------------------------------------------

ef1 %>% glimpse()
ef1 %>% colnames()

## algorithm 1

df1 <- ef1 %>% 
  filter((str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J12")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J13")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J14")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J15")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J16")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J18")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J69")) |
         (str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"P23"))) 

df1_comfil <- ef1 %>%
  filter((str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J441")) |
         (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J449")))

df1_after_comfil <- inner_join(df1, df1_comfil, by = c("患者ID", "入院日")) %>% 
  select(患者ID, 入院日) %>% 
  distinct(患者ID, 入院日)

df1_after_comfil %>% write_rds("output/df1.rds", compress = "gz")

## algorithm 2

df2 <- ef1 %>% 
  filter(str_detect(ef1$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(ef1$データ,"J441"))
        
df2_comfil <- ef1 %>%
  filter((str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J12")) |
           (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J13")) |
           (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J14")) |
           (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J15")) |
           (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J18")) |
           (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J69")) |
           (str_detect(ef1$項目名,"入院時併存症名に対するICD10コード") & str_detect(ef1$データ,"J23")))

df2_after_comfil <- inner_join(df2, df2_comfil, by = c("患者ID", "入院日")) %>% 
  select(患者ID, 入院日) %>% 
  distinct(患者ID, 入院日)

df2_after_comfil %>% write_rds("output/df2.rds", compress = "gz")

# EF1 ---------------------------------------------------------------------

df1 <- read_rds("output/df1.rds")
df2 <- read_rds("output/df2.rds")

df <- full_join(df1, df2, by = c("患者ID", "入院日"))　# match = 同一日入院

select_dpc <- function(code, payload, name){
  data <- ef1 %>% 
    arrange(`患者ID`, `入院日`) %>% 
    select(`患者ID`, `入院日`, `コード`, `ペイロード番号`, `データ`) %>%  
    filter(`コード` == code, `ペイロード番号` == payload) %>% 
    distinct(`患者ID`, `入院日`, .keep_all=TRUE) %>% 
    pivot_wider(names_from = c(`コード`, `ペイロード番号`),
                values_from = `データ`,
                values_fill = list(value = NA_character_)) 
  data <- data %>% rename(!!name := colnames(data)[3])
  df <<- left_join(df, data, by = c("患者ID", "入院日"))
}

select_dpc2 <- function(code, payload, serial, name){
  data <- ef1 %>% 
    arrange(`患者ID`, `入院日`) %>% 
    select(`患者ID`, `入院日`, `コード`, `ペイロード番号`, `連番`, `データ`) %>%  
    filter(`コード` == code, `ペイロード番号` == payload, `連番` == serial) %>% 
    distinct(`患者ID`, `入院日`, .keep_all=TRUE) %>% 
    pivot_wider(names_from = c(`コード`, `ペイロード番号`, `連番`),
                values_from = `データ`,
                values_fill = list(value = NA_character_)) 
  data <- data %>% rename(!!name := colnames(data)[3])
  after <<- left_join(df, data, by = c("患者ID", "入院日"))
}

select_dpc("A000010", "1", "birth_date")  
select_dpc("A000010", "2", "sex")  
select_dpc("A000030", "1", "disc_date")  
select_dpc("A000030", "2", "disc_to")  
select_dpc("A000030", "3", "disc_prognosis")  
select_dpc("A000030", "4", "death24h")  

select_dpc2("A006020", "2", "0", "precip")  

select_dpc2("A006040", "2", "1", "com1")  
select_dpc2("A006040", "2", "2", "com2")  
select_dpc2("A006040", "2", "3", "com3")  
select_dpc2("A006040", "2", "4", "com4")  
select_dpc2("A006040", "2", "5", "com5")  
select_dpc2("A006040", "2", "6", "com6")  
select_dpc2("A006040", "2", "7", "com7")  
select_dpc2("A006040", "2", "8", "com8")  
select_dpc2("A006040", "2", "9", "com9")  
select_dpc2("A006040", "2", "10", "com10") 

select_dpc2("A006050", "2", "1", "subs1") 
select_dpc2("A006050", "2", "2", "subs2") 
select_dpc2("A006050", "2", "3", "subs3") 
select_dpc2("A006050", "2", "4", "subs4") 
select_dpc2("A006050", "2", "5", "subs5") 

select_dpc("ADL0010", "2", "adm_adl") 
select_dpc("ADL0020", "2", "disc_adl") 
select_dpc("JCS0010", "2", "adm_jcs") 
select_dpc("JCS0020", "2", "disc_jcs") 
select_dpc("M040020", "2", "severity_score") 
select_dpc("M040020", "3", "cap_hap") 

## CCI  

df <- df %>% 
  mutate(dummy_id = row_number())

cci <- df %>% 
  select(`患者ID`, dummy_id, `入院日`, starts_with("com")) %>% 
  pivot_longer(
  cols = starts_with("com"),
  names_to = "item",
  values_to = "value"
)

charlson <- comorbidity::comorbidity(x = cci, id = "dummy_id", code = "value", map = "charlson_icd10_quan", assign0 = FALSE)
charlson
cci_score <- score(charlson, weights = "quan", assign0 = FALSE)

df <- cbind(df, cci_score) 

df$adm_adl <- sapply(strsplit(df$adm_adl,""), function(x) sum(as.numeric(x))) 
df$disc_adl <- sapply(strsplit(df$disc_adl,""), function(x) sum(as.numeric(x))) 

df <- df %>% 
  mutate(bun_cat = str_sub(severity_score, 1, -7),
         spo2 = str_sub(severity_score, 2, -6),
         ams = str_sub(severity_score, 3, -5),
         sbp = str_sub(severity_score, 4, -4),
         immunodef = str_sub(severity_score, 5, -3),
         crp = str_sub(severity_score, 6, -2),
         dev_place = str_sub(severity_score, 7, -1))

df <- df %>% 
  mutate(birth_date = if_else(birth_date == "1931年以下出生", "19310101", birth_date),
         birth_date = str_sub(birth_date, 1, -5),
         adm_year = str_sub(`入院日`, 1, -5),
         age = as.numeric(adm_year) - as.numeric(birth_date) + 1, 
         los = trunc(time_length(interval(ymd(`入院日`),
                                          ymd(disc_date)),"day"))) %>% 
  select(-adm_year) %>% 
  filter(!is.na(los))

df <- df %>% 
  filter(age >= 40)

## last follow-up

last_follow_up <- patient_data %>% 
  select(`患者ID`, `観察期間終了日(EMR)`, `死亡の有無`, `死亡日`)
df <- left_join(df, last_follow_up, by = "患者ID") %>% 
  arrange(`患者ID`, `入院日`)


df %>% write_rds("output/df_ef1.rds", compress = "gz")

# Drug data ---------------------------------------------------------------

df <- read_rds("output/df_ef1.rds")
emr_drug_data %>% glimpse()
drug_code %>% glimpse()

atc <- drug_code %>% 
  select(`薬価基準収載医薬品コード`, `WHO-ATCコード`)
emr_drug_data <- emr_drug_data %>% 
  left_join(atc, by = c("薬価コード" = "薬価基準収載医薬品コード"))

id_unique <- df %>% select(`患者ID`) %>% pull() %>% unique()
emr_drug_data_filtered <- emr_drug_data %>% 
  filter(`患者ID` %in% id_unique)

drug_select <- function(.data){
  v <- map(1:nrow(.data), ~{
    d <- slice(.data,.)
    c(d$`開始日`:d$`終了日`)
  }) %>% 
    unlist() %>%
    unique()
  
  r <- tibble(x = v) %>%
    arrange(x) %>%
    mutate(diff = x - lag(x)) %>%
    replace_na(list(diff=1)) %>%
    mutate(prescid = cumsum(diff > 1)) %>%
    group_by(prescid) %>%
    summarise(s = min(x), e = max(x))
  
  return(r)
　}

drug_list <- emr_drug_data_filtered %>% 
  group_by(`患者ID`, `WHO-ATCコード`) %>% 
  nest() %>% 
  mutate(res = map(data, drug_select)) %>% 
  unnest(res) %>%
  select(-prescid, -data) %>% 
  ungroup()

drug_list <- drug_list %>% 
  rename(`開始日` = s,
         `終了日` = e)

drug_list %>% write_rds("output/drug_list.rds", compress = "gz")

drug_list <- read_rds("output/drug_list.rds")

abx_list <- c("J01A", "J01B", "J01C", "J01D", "J01E", "J01F", "J01G", "J01M", "J01R", "J01X")
abx <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, paste(abx_list, collapse = "|")))

steroid <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "H02"))

tetra <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01A"))

anphe <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01B"))

betal <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01C"))

other_beta <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01D"))

st <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01E"))

macro <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01F"))

amino <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01G"))

quino <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01M"))

combi <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01R"))

others <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "J01X"))

antac <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "A02"))

vaso <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "C01C"))

diure <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "C02"))

dm_drug <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, "A01"))

drug_select <- function(.data1, .data2, name1, name2, name3){
  .data1 <- .data1 %>%
    arrange(`患者ID`, `入院日`) %>% 
    mutate(h_interv = interval(ymd(`入院日`), ymd(`disc_date`))) %>% 
    select(`患者ID`, `入院日`, h_interv) 
  
  .data2 <- .data2 %>% 
    arrange(`患者ID`, `開始日`) %>%
    mutate(p_interv = interval(ymd(`開始日`), ymd(`終了日`))) %>% 
    select(-`WHO-ATCコード`)
  
  comb <- .data1 %>% 
    left_join(.data2, by = "患者ID") %>% 
    mutate(overlap = int_overlaps(p_interv, h_interv)) %>% 
    filter(overlap == "TRUE") %>% 
    select(`患者ID`, `入院日`, `開始日`, `終了日`) %>% 
    mutate(name = 1) %>% 
    distinct(`患者ID`, `入院日`,.keep_all=TRUE) %>% 
    rename(!!name1 := "name",
           !!name2 := "開始日",
           !!name3 := "終了日")
  
  df <<- left_join(df, comb, by = c("患者ID", "入院日")) 
}

drug_select(df, abx, "abx", "abx_start", "abx_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, tetra, "tetra", "tetra_start", "tetra_end")
drug_select(df, anphe, "anphe", "anphe_start", "anphe_end")
drug_select(df, betal, "betal", "betal_start", "betal_end")
drug_select(df, other_beta, "other_beta", "other_beta_start", "other_beta_end")
drug_select(df, st, "st", "st_start", "st_end")
drug_select(df, macro, "macro", "macro_start", "macro_end")
drug_select(df, amino, "amino", "amino_start", "amino_end")
drug_select(df, quino, "quino", "quino_start", "quino_end")
drug_select(df, combi, "combi", "combi_start", "combi_end")
drug_select(df, others, "others", "others_start", "others_end")
drug_select(df, antac, "antac", "antac_start", "antac_end")
drug_select(df, vaso, "vaso", "vaso_start", "vaso_end")
drug_select(df, diure, "diure", "diure_start", "diure_end")
drug_select(df, dm_drug, "dm_drug", "dm_drug_start", "dm_drug_end")

df <- df %>% 
  mutate(emp_abx = if_else((ymd(`入院日`) == ymd(abx_start) | ymd(`入院日`) == ymd(abx_start+1)), 1, 0),
         emp_tetra = if_else((ymd(`入院日`) == ymd(tetra_start) | ymd(`入院日`) == ymd(tetra_start+1)), 1, 0),
         emp_anphe = if_else((ymd(`入院日`) == ymd(anphe_start) | ymd(`入院日`) == ymd(anphe_start+1)), 1, 0),
         emp_betal = if_else((ymd(`入院日`) == ymd(betal_start) | ymd(`入院日`) == ymd(betal_start+1)), 1, 0),
         emp_other_beta = if_else((ymd(`入院日`) == ymd(other_beta_start) | ymd(`入院日`) == ymd(other_beta_start+1)), 1, 0),
         emp_st = if_else((ymd(`入院日`) == ymd(st_start) | ymd(`入院日`) == ymd(st_start+1)), 1, 0),
         emp_amino = if_else((ymd(`入院日`) == ymd(amino_start) | ymd(`入院日`) == ymd(amino_start+1)), 1, 0),
         emp_macro = if_else((ymd(`入院日`) == ymd(macro_start) | ymd(`入院日`) == ymd(macro_start+1)), 1, 0),
         emp_quino = if_else((ymd(`入院日`) == ymd(quino_start) | ymd(`入院日`) == ymd(quino_start+1)), 1, 0),
         emp_combi = if_else((ymd(`入院日`) == ymd(combi_start) | ymd(`入院日`) == ymd(combi_start+1)), 1, 0),
         emp_others = if_else((ymd(`入院日`) == ymd(others_start) | ymd(`入院日`) == ymd(others_start+1)), 1, 0),
         emp_steroid = if_else((ymd(`入院日`) == ymd(steroid_start) | ymd(`入院日`) == ymd(steroid_start+1)), 1, 0)) %>% 
  mutate(across(abx_start:emp_steroid, ~ replace_na(.x, 0)))

df %>% write_rds("output/df_ef1_drug.rds", compress = "gz")

# Laboratory data ---------------------------------------------------------

df <- read_rds("output/df_ef1_drug.rds")
emr_laboratory_data %>% glimpse()

lab_names <- emr_laboratory_data %>% select(`検査名`) %>% pull(1) %>% unique()

df <- df %>% 
  mutate(`入院日` = ymd(`入院日`)) %>% 
  arrange(`患者ID`, `入院日`)

emr_laboratory_data <- emr_laboratory_data %>% 
  mutate(`検査日` = ymd(`検査日`)) %>% 
  arrange(`患者ID`, `検査日`)

lab_combine <- function(name, newname){
  .data2 <- emr_laboratory_data %>% 
    filter(`検査名` == name)
  
  indx1 <- neardate(df$`患者ID`, .data2$`患者ID`, df$`入院日`, .data2$`検査日`, 
                   best="after")
  indx1 <- ifelse((.data2$`検査日`[indx1] - df$`入院日`) > 2, NA, indx1)
  indx2 <- neardate(df$`患者ID`, .data2$`患者ID`, df$`入院日`, .data2$`検査日`, 
                    best="prior")
  indx2 <- ifelse((df$`入院日` - .data2$`検査日`[indx2]) > 2, NA, indx2)
  indx3 <- ifelse(is.na(indx1), indx2, # none after, take before
         ifelse(is.na(indx2), indx1, #none before
                ifelse(abs(.data2$`検査日`[indx2]- df$`入院日`) <
                         abs(.data2$`検査日`[indx1]- df$`入院日`), indx2, indx1)))
  df <<- df %>% cbind(.data2[indx3, "結果"]) %>% rename(!!newname := "結果")
}

lab_combine("白血球数;WBC", "wbc")
lab_combine("アルブミン;alb", "alb1")
lab_combine("尿素窒素;BUN", "bun")
lab_combine("C反応性タンパク;CRP", "crp1")
lab_combine("白血球分画:好酸球;Eosino", "eo")
lab_combine("白血球分画:好中球;Neutr", "neut")
lab_combine("白血球分画:単球;Mono", "mono")
lab_combine("白血球分画:リンパ球;Lymp", "lym")
lab_combine("白血球分画:好塩基球;Baso", "baso")
lab_combine("蛋白分画 - アルブミン分画", "alb2")
lab_combine("C反応性タンパク;高感度CRP", "crp2")
lab_combine("尿素窒素;BUN(透析前)", "bun2")
lab_combine("白血球数;WBC(透析前)", "wbc2")

df <- df %>% 
  mutate(alb = if_else(is.na(alb1), alb2, alb1),
         crp = if_else(is.na(crp1), crp2, crp1))

df %>% write_rds("output/df_ef1_drug_lab.rds", compress = "gz")

# Procedure ---------------------------------------------------------------

df <- read_rds("output/df_ef1_drug.rds")
claim_procedure_data %>% glimpse()
procedure_code %>% glimpse()

pcode <- procedure_code %>% 
  select(`診療点数早見表区分コード`, `診療行為コード`)
claim_procedure_data <- claim_procedure_data %>% 
  left_join(pcode, by = c("診療行為コード"))

id_unique <- df %>% select(`患者ID`) %>% pull() %>% unique()
claim_procedure_data_filtered <- claim_procedure_data %>% 
  filter(`患者ID` %in% id_unique)

procedure_select <- function(.data){
  c <- .data %>% 
    distinct(`対象日`, .keep_all=TRUE) %>% 
    mutate(diff = `対象日` - lag(`対象日`)) %>% 
    replace_na(list(diff=1)) %>% 
    mutate(procid = cumsum(diff > 1)) %>% 
    group_by(procid) %>% 
    mutate(start = head(`対象日`, 1),
           end = tail(`対象日`, 1)
           ) %>% 
    slice(1)
}

claim_procedure_data_filtered <- claim_procedure_data_filtered %>% 
  group_by(`患者ID`, `診療点数早見表区分コード`) %>% 
  nest() %>% 
  mutate(res = map(data, procedure_select)) %>% 
  unnest(res) %>%
  select(-procid, -data) %>% 
  ungroup()

claim_procedure_data_filtered %>% write_rds("output/procedure_list.rds", compress = "gz")

procedure_list <- read_rds("output/procedure_list.rds")

intubation <- procedure_list %>% 
  filter(str_detect(`診療点数早見表区分コード`, "J044"))

oxy <- procedure_list %>% 
  filter(str_detect(`診療点数早見表区分コード`, "J024"))

mecha_intu <- procedure_list %>% 
  filter(str_detect(`診療点数早見表区分コード`, "J045"))

dialysis <- procedure_list %>% 
  filter(str_detect(`診療点数早見表区分コード`, "J038"))

procedure_select <- function(.data1, .data2, name1, name2, name3){
  .data1 <- .data1 %>%
    arrange(`患者ID`, `入院日`) %>% 
    mutate(h_interv = interval(ymd(`入院日`), ymd(`disc_date`))) %>% 
    select(`患者ID`, `入院日`, h_interv) 
  
  .data2 <- .data2 %>% 
    arrange(`患者ID`, `start`) %>%
    mutate(p_interv = interval(ymd(`start`), ymd(`end`))) %>% 
    select(`患者ID`, `診療点数早見表区分コード`, `診療行為コード`, start, end, p_interv)
  
  comb <- .data1 %>% 
    left_join(.data2, by = "患者ID") %>% 
    mutate(overlap = int_overlaps(p_interv, h_interv)) %>% 
    filter(overlap == "TRUE") %>% 
    select(`患者ID`, `入院日`, `start`, `end`) %>% 
    mutate(name = 1) %>% 
    distinct(`患者ID`, `入院日`,.keep_all=TRUE) %>% 
    rename(!!name1 := "name",
           !!name2 := "start",
           !!name3 := "end")
  
  df <<- left_join(df, comb, by = c("患者ID", "入院日")) 
}

procedure_select(df, intubation, "intubation", "intubation_start", "intubation_end")
procedure_select(df, oxy, "oxy", "oxy_start", "oxy_end")
procedure_select(df, mecha_intu, "mecha_intu", "mecha_intu_start", "mecha_intu_end")
procedure_select(df, dialysis, "dialysis", "dialysis_start", "dialysis_end")


df <- df %>% 
  mutate(emp_oxy = if_else(ymd(`入院日`) == ymd(oxy_start), 1, 0)) %>% 
  mutate(across(intubation_start:emp_oxy, ~ replace_na(.x, 0)))

df %>% write_rds("output/df_ef1_drug_proc.rds", compress = "gz")


# Baseline ----------------------------------------------------------------

df <- read_rds("output/df_ef1_drug_proc.rds")

### baseline drug

drug_list <- read_rds("output/drug_list.rds")

drug_baseline <- function(name, newname){
  .data2 <- drug_list %>% 
    filter(str_detect(`WHO-ATCコード`, !!name)) %>% 
    group_by(`患者ID`) %>% 
    nest() %>% 
    mutate(head = map(data, ~{min(.$`開始日`)}),
           tail = map(data, ~{max(.$`終了日`)})) %>% 
    unnest(head, tail) %>% 
    select(-data) %>% 
    ungroup() 
  df <- df %>%  
    left_join(.data2, by = "患者ID")
  df <- df %>% 
    mutate(toss_name = if_else((ymd(`入院日`) < ymd(head) + 30) & (ymd(head) < ymd(`入院日`)), "1", "0")) %>% 
    select(-head, -tail) 
  df <<- df %>% left_join(.data2, by = "患者ID") %>% rename(!!newname := "toss_name") %>% select(-head, -tail)
}

drug_baseline("H02", "base_steroid")
drug_baseline("R03", "base_inhaler")
drug_baseline("L04A", "base_immuno")
drug_baseline("A01", "base_dm_drug")

### baseline procedure

procedure_list <- read_rds("output/procedure_list.rds")

procedure_baseline <- function(name, newname){
  .data2 <- procedure_list %>% 
    filter(str_detect(`診療点数早見表区分コード`, !!name)) %>% 
    group_by(`患者ID`) %>% 
    nest() %>% 
    mutate(head = map(data, ~{min(.$start)}),
           tail = map(data, ~{max(.$end)})) %>% 
    unnest(head, tail) %>% 
    select(-data) %>% 
    ungroup() 
  df <- df %>%  
    left_join(.data2, by = "患者ID")
  df <- df %>% 
    mutate(toss_name = if_else((ymd(`入院日`) < ymd(head) + 30) & (ymd(head) < ymd(`入院日`)), "1", "0")) %>% 
    select(-head, -tail) 
  df <<- df %>% left_join(.data2, by = "患者ID") %>% rename(!!newname := "toss_name") %>% select(-head, -tail)
}

procedure_baseline("J038", "base_dialysis")

df <- df %>% 
  mutate(across(base_steroid:base_dialysis, ~ as.numeric(.x))) %>% 
  mutate(across(base_steroid:base_dialysis, ~ replace_na(.x, 0)))

df %>% write_rds("output/df_ef1_drug_proc_base.rds", compress = "gz")

# Last cleaning ----------------------------------------------------------------

df <- read_rds("output/df_ef1_drug_proc_base.rds")
df %>% glimpse()

df <- df %>% 
  rename(id = `患者ID`,
         adm_date = `入院日`,
         last_follow = `観察期間終了日(EMR)`,
         last_death = `死亡の有無`,
         death_date = `死亡日`) %>% 
  select(id, age, sex, adm_adl, cap_hap, spo2, ams, sbp, immunodef, crp, dev_place, adm_jcs, cci,
         abx, emp_abx, steroid, emp_steroid, base_steroid, tetra, anphe, betal, other_beta, st, macro, amino, quino, combi, others,
         emp_tetra, emp_anphe, emp_betal, emp_other_beta, emp_st, emp_macro, emp_amino, emp_quino, emp_combi, emp_others,
         antac, base_inhaler, vaso, diure, base_dm_drug, base_immuno, 
         intubation, oxy, emp_oxy, mecha_intu, dialysis,
         los, adm_date, last_follow, disc_date, death_date, disc_prognosis, death24h, last_death) %>% 
  mutate(ams2 = if_else(adm_jcs == "0", "0", "1"),
         ams = if_else(ams == "9" | is.na(ams), ams2, ams),
         transfer24h = if_else(los == 0, 1, 0),
         inhosp_death = if_else((disc_prognosis == 6 | disc_prognosis == 7), 1, 0),
         death30d = case_when(is.na(`death_date`) ~ "0",
                              ymd(`death_date`) - ymd(`death_date`) < 30 ~ NA_character_,
                              ymd(`death_date`) - ymd(`death_date`) >= 30 ~ "1")) %>% 
  mutate(spo2 = if_else(dev_place == "8" | dev_place == "9" | spo2 == "9", NA_character_, spo2),
         sbp = if_else(dev_place == "8" | dev_place == "9" | sbp == "9", NA_character_, sbp),
         immunodef = if_else(dev_place == "8" | dev_place == "9" | immunodef == "9", NA_character_, immunodef),
         dev_plave = if_else(dev_place == "8" | dev_place == "9", NA_character_, dev_place)) %>% 
  mutate(sex = factor(sex,
                      levels = c("1", "2"),
                      labels = c("Male", "Female")),
         spo2 = factor(spo2,
                      levels = c("0", "1", "2"),
                      labels = c("Normal", "FiO2 < 35%","FiO2 >= 35%")),
         ams = factor(ams,
                      levels = c("0", "1"),
                      labels = c("Normal", "Altered mental status")),
         sbp = factor(spo2,
                      levels = c("0", "1"),
                      labels = c("Not", "Shock")),
         immunodef = factor(spo2,
                            levels = c("0", "1"),
                            labels = c("Not", "Immunodeficiency")),
         dev_place = factor(spo2,
                            levels = c("3", "5"),
                            labels = c("HAP", "CAP")),
         emp_steroid = factor(emp_steroid,
                              levels = c("0", "1"),
                              labels = c("Non-steroid group", "Steroid group")),
         emp_abx = factor(emp_abx,
                          levels = c("0", "1"),
                          labels = c("Not", "Empirical abx")),
         inhosp_death = factor(inhosp_death,
                              levels = c("0", "1"),
                              labels = c("Not", "In-hospital death")),
         death30d = factor(death30d,
                           levels = c("0", "1"),
                           labels = c("Not", "30-day death")),
         emp_tetra = factor(emp_tetra,
                          levels = c("0", "1"),
                          labels = c("Not", "Tetracyclines")),
         emp_anphe = factor(emp_anphe,
                          levels = c("0", "1"),
                          labels = c("Not", "Amphenicols")),
         emp_betal = factor(emp_betal,
                          levels = c("0", "1"),
                          labels = c("Not", "Beta-lactum antibacterials, penicillins")),
         emp_other_beta = factor(emp_other_beta,
                          levels = c("0", "1"),
                          labels = c("Not", "Other beta-lactam antibacterials")),
         emp_amino = factor(emp_amino,
                            levels = c("0", "1"),
                            labels = c("Not", "Aminoglycoside antibacterials")),
         emp_st = factor(emp_st,
                          levels = c("0", "1"),
                          labels = c("Not", "Sulfamethoxazole and trimethoprim")),
         emp_macro = factor(emp_macro,
                          levels = c("0", "1"),
                          labels = c("Not", "Macrolide, lincosamides and streptogramins")),
         emp_quino = factor(emp_quino,
                          levels = c("0", "1"),
                          labels = c("Not", "Quinolone antibacterials")),
         emp_combi = factor(emp_combi,
                          levels = c("0", "1"),
                          labels = c("Not", "Combinations of antibiotics")),
         emp_others = factor(emp_others,
                          levels = c("0", "1"),
                          labels = c("Not", "Other antibacterials")),
         vaso = factor(vaso,
                       levels = c("0", "1"),
                       labels = c("Not", "Vasopressors")),
         diure = factor(diure,
                        levels = c("0", "1"),
                        labels = c("Not", "Diuretics")),
         intubation = factor(intubation,
                             levels = c("0", "1"),
                             labels = c("Not", "Tracheal intubation")),
         emp_oxy = factor(emp_oxy,
                          levels = c("0", "1"),
                          labels = c("Not", "Oxygen use on admission")),
         base_dialysis = factor(dialysis,
                          levels = c("0", "1"),
                          labels = c("Not", "Dialysis"))
         ) %>% 
  #mutate(across(cap_hap:dev_place, ~ as.numeric(.x))) %>% 
  #mutate(across(disc_prognosis:death24h, ~ as.numeric(.x))) %>% 
  mutate(across(adm_date:death_date, ~ ymd(.x)))
         
df %>% write_rds("output/df_ef1_drug_proc_base_cleaned.rds", compress = "gz")


