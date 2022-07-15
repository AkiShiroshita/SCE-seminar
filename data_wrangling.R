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
  df <<- left_join(df, data, by = c("患者ID", "入院日"))
}

select_dpc("A000010", "1", "birth_date")  
select_dpc("A000010", "2", "sex")  
select_dpc("A000030", "1", "disc_date")  
select_dpc("A000030", "2", "disc_to")  
select_dpc("A000030", "3", "disc_prognosis")  
select_dpc("A000030", "4", "death24h")  

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

