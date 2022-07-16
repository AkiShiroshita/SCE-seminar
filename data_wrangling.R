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
drug_code %>% glimpse()

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

drug_memo <- read_csv("memo/drug_memo.csv", 
                      locale = locale(encoding = "SHIFT-JIS")) %>% 
  select(1:2)

atc <- drug_memo %>% select(2) %>% pull() %>% unique()

df <- df %>% 
  arrange(`患者ID`, `入院日`) %>%
  mutate(p_interv = interval(ymd(`入院日`), ymd(`disc_date`))) 
  
demo <- map(atc, ~{
   selected <- drug_list %>% 
     filter(str_detect(`WHO-ATCコード`, .)) %>% 
     arrange(`患者ID`, `開始日`) %>% 
     mutate(h_interv = interval(ymd(`開始日`), ymd(`終了日`))) %>% 
     select(`患者ID`, `開始日`, `終了日`, h_interv)
   
   df <- df %>% 
     left_join(selected, by = "患者ID") %>% 
     mutate(overlap = int_overlaps(p_interv, h_interv)) %>% 
     filter(overlap == "TRUE") %>% 
     mutate(. = 1) %>% 
     select(`患者ID`, `入院日`, ., `開始日`, `終了日`) %>% 
     mutate(name = 1) %>% 
     distinct(`患者ID`, `入院日`,.keep_all=TRUE) 
})

###  
abx_list <- c("J01A", "J01B", "J01C", "J01D", "J01E", "J01F", "J01G", "J01M", "J01R", "J01X")
abx <- drug_list %>% 
  filter(str_detect(`WHO-ATCコード`, paste(abx_list, collapse = "|")))



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
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")
drug_select(df, steroid, "steroid", "steroid_start", "steroid_end")

df %>% glimpse()


###
p <- map(1:length(id_unique), ~{
  d <- .data %>% 
    filter(`患者ID` == id_unique[.]) %>% 
    
  
})

df %>% filter(`患者ID` == "10017")

.data <- .data %>%
  group_by(`患者ID`) %>% 
  
  
  left_join(.data, by = c("入院日" == "開始日"))

near_combine <- function(.data1, .data2){
  .data1 <- .data1 %>% 
    mutate(`入院日` = ymd(`入院日`)) %>% 
    arrange(`患者ID`, `入院日`)
  .data2 <- .data2 %>% 
    mutate(`開始日` = ymd(`開始日`),
           `終了日` = ymd(`終了日`)) %>% 
    arrange(`患者ID`, `開始日`)
  
  indx <- neardate(.data1$`患者ID`, .data2$`患者ID`, .data1$`入院日`, .data2$`開始日`, 
                    best="after")
  d <- .data2[indx, ] %>% 
    drop_na(`開始日`)
  
}

# Laboratory data ---------------------------------------------------------


