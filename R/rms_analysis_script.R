library(tidyverse)
library(srvyr)
library(labelled)
library(lubridate)

source("R/composite_indicators.R")
source("R/support_functions.R")

# clean data with weights
data_path <- "inputs/clean_data_unhcr_rms.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "RMS Uganda 2022 UNHCR_cleaned"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "RMS Uganda 2022 UNHCR_cleaned", col_types = c_types, na = "NA")
df_roster_clean_data <- readxl::read_excel(path = data_path, sheet = "hh_roster_cleaned", na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/RMS_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English (en)`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_uga_rms.csv")


# add composite indicators ------------------------------------------------

#Create function that turn character values into numeric 
labelled_chr2dbl <- function(x) { 
  varlab <- var_label(x) 
  vallab <- val_labels(x) 
  vallab <- setNames(as.numeric(vallab), names(vallab)) 
  x <- as.numeric(as.character(x)) 
  var_label(x) <- varlab 
  val_labels(x) <- vallab 
  x 
}

# main

df_rms_clean_data_composites <- create_composite_indicators_rms(df_main_clean_data) |> 
  mutate( # primary citizenship from REF01 and REF02     
    citizenship = REF02 
  ) |> 
  mutate(citizenship = labelled(citizenship, labels = val_labels(df_main_clean_data$REF02), label = var_label(df_main_clean_data$REF02))) |> 
  mutate( 
    # disability identifier variables according to Washington Group standards 
    disaux1_234 = DIS01 %in% c("2","3","4"), # indicator variables for all 6 domains with value TRUE if SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL 
    disaux2_234 = DIS02 %in% c("2","3","4"), 
    disaux3_234 = DIS03 %in% c("2","3","4"), 
    disaux4_234 = DIS04 %in% c("2","3","4"), 
    disaux5_234 = DIS05 %in% c("2","3","4"), 
    disaux6_234 = DIS06 %in% c("2","3","4"), 
    disaux1_34 = DIS01 %in% c("3","4"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL 
    disaux2_34 = DIS02 %in% c("3","4"), 
    disaux3_34 = DIS03 %in% c("3","4"), 
    disaux4_34 = DIS04 %in% c("3","4"), 
    disaux5_34 = DIS05 %in% c("3","4"), 
    disaux6_34 = DIS06 %in% c("3","4") ) |> 
  rowwise() |>
  mutate( disSum234 = sum(c_across(disaux1_234:disaux6_234)), # count number of TRUE indicator variables over 6 domains 
          disSum34 = sum(c_across(disaux1_34:disaux6_34)) # count number of TRUE indicator variables over 6 domains 
  ) |>
  dplyr::ungroup() |> 
  mutate( DISABILITY1 = case_when( # : the level of inclusion is at least one domain/question is coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL. 
    disSum234 >= 1 ~ 1, 
    disSum234 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY2 = case_when( # : the level of inclusion is at least two domains/questions are coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL or any 1 domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL 
    disSum234 >= 2 | disSum34 >=1 ~ 1, 
    disSum234 < 2 & disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY3 = case_when( # : the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL. 
    disSum34 >= 1 ~ 1, disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY4 = case_when( # : the level of inclusion is at least one domain/question is coded CANNOT DO AT ALL. 
    DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4" ~ 1, 
    !(DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4") & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY1 = labelled(DISABILITY1, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 1"), 
          DISABILITY2 = labelled(DISABILITY2, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 2"), 
          DISABILITY3 = labelled(DISABILITY3, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 3"), 
          i.DISABILITY3 = to_factor(DISABILITY3), 
          DISABILITY4 = labelled(DISABILITY4, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 4")) |> 
  ###Calculate having at least one disability identifier among 4 categories 
  mutate(disab = case_when(DISABILITY1==1 | DISABILITY2==1 | DISABILITY3==1 | DISABILITY4==1 ~ 1, DISABILITY1==0 | DISABILITY2==0 | DISABILITY3==0 | DISABILITY4==0 ~ 0, TRUE ~ NA_real_) ) |> 
  mutate(disab = labelled(disab, labels = c( "Without disability" = 0, "With disability" = 1) ),
         i.disab = to_factor(disab)
         ) |> 
  mutate(citizenship_com   = citizenship) # adding this that is mentioned in aggregation

# add extra indicators accordining to the guide
df_rms_main_composites_extra <- df_rms_clean_data_composites |> 
  mutate(HEA01_num = labelled_chr2dbl(HEA01),
         health_acc = case_when(HEA01_num!=98 & HEA03 <= 60 ~ 1,
                                TRUE ~ 0),
         health_acc = labelled(health_acc,
                               labels = c("Health facility is available" = 1,
                                          "Health facilitiy is not available" = 0),
                               label = "Access to health facilities"),
         i.health_acc = to_factor(health_acc),
         LIGHT01_num = labelled_chr2dbl(LIGHT01),
         LIGHT02_num = labelled_chr2dbl(LIGHT02),
         LIGHT03_num = labelled_chr2dbl(LIGHT03),
         electricity = case_when(LIGHT01_num ==1 & (LIGHT02_num==1 |LIGHT02_num==3 | 
                                                      LIGHT02_num==5 | LIGHT02_num==6 
                                                    | LIGHT02_num==7 | LIGHT02_num==8) & (LIGHT03_num!=1 | LIGHT03_num!=96 | LIGHT03_num!=98 ) ~ 1,
                                 TRUE ~ 0),
         electricity = labelled(electricity, labels = c("Yes" = 1, "No" = 0),
                                label = "Access to electricity"),
         i.electricity = to_factor(electricity),
         DWA03a_num = labelled_chr2dbl(DWA03a),
         DWA02_num = labelled_chr2dbl(DWA02),
         DWA01_num = labelled_chr2dbl(DWA01),
         DWA04_num = labelled_chr2dbl(DWA04),
         time_DWA = case_when( DWA03a_num==1 ~ 1, DWA03a_num==2 ~ 60),
         time_tot = time_DWA*DWA03b,
         dwa_cond1=case_when( time_tot > 30 ~ 0, TRUE ~ 1), ##Accessible under 30 minutes
         dwa_cond2=case_when(DWA01_num!=7 |DWA01_num !=9 |DWA01_num != 13 | DWA01_num != 96 |DWA01_num !=98 ~ 1,
                             TRUE ~ 0), ## protected source
         dwa_cond3=case_when(DWA04_num==1 ~ 0, TRUE ~ 1), ## It was available in the last 30 days
         drinkingwater=case_when((dwa_cond1==1 & dwa_cond2==1 & dwa_cond3==1) ~ 1, TRUE ~ 0),
         drinkingwater = labelled(drinkingwater,  labels = c("Yes" = 1, "No" = 0),
                                  label = "Access to drinking water"),
         i.drinkingwater = to_factor(drinkingwater),
         DWE01_num = labelled_chr2dbl(DWE01),
         DWE02_num = labelled_chr2dbl(DWE02),
         DWE03_num = labelled_chr2dbl(DWE03),
         DWE04_num = labelled_chr2dbl(DWE04),
         dwe01_cat=case_when( #Only apartment and house
           (DWE01_num==1 | DWE01_num==2) ~ 1, TRUE ~ 0 ),
         dwe02_cat=case_when( #unimproved floor when earth,sand,clay,mud, dung or other
           (DWE02_num==1 | DWE02_num==2 | DWE02_num==96) ~ 0, TRUE ~ 1 ),
         dwe03_cat=case_when( #unimproved roof all options except metal,wood,ceramic tiles, cement, roofing shingles/sheets
           (DWE03_num==8 |DWE03_num==9 | DWE03_num==10 | DWE03_num==11 |
              DWE03_num==12 | DWE03_num==13 | DWE03_num==8) ~ 1 , TRUE ~ 0),
         dwe04_cat=case_when( #improved wall: cement,stone,bricks,cement blocks, covered adobe, wood planks
           (DWE04_num==10| DWE04_num==11| DWE04_num==12| DWE04_num==13| DWE04_num==14| DWE04_num==15) ~ 1,
           TRUE ~ 0),
         crowding = DWE05/HH01,
         dwe05_cat = case_when( ##if crowding < 3 
           crowding < 3 ~ 1, TRUE ~ 0),
         DWE08_num = labelled_chr2dbl(DWE08),
         DWE09_num = labelled_chr2dbl(DWE09),
         dwe09_cat = case_when( #affordable if HH pays rent and often and always without financial distress
           (DWE08_num==1 & (DWE09_num==1 | DWE09_num==2)) ~ 1, 
           (DWE08_num==1 & (DWE09_num==3 | DWE09_num==4)) ~ 0,  DWE08_num==0 ~ NA_real_),
         shelter=case_when(
           dwe01_cat==0 | dwe02_cat==0 | dwe03_cat==0 | dwe04_cat==0 | dwe05_cat==0 | dwe09_cat==0  ~ 0, 
           dwe01_cat==1 & dwe02_cat==1 & dwe03_cat==1 & dwe04_cat==1 & dwe05_cat==1 & dwe09_cat==1 ~ 1),
         shelter = labelled(shelter, labels = c("Yes" = 1, "No" = 0),
                            label = "Habitable and affordable shelter"),
         i.shelter = to_factor(shelter),
         impact2_2=case_when(
           shelter==0 | electricity==0 | drinkingwater==0 | health_acc==0 ~ 0,
           shelter==1 & electricity==1 & drinkingwater==1 & health_acc==1 ~ 1),
         impact2_2 = labelled(impact2_2, labels =c("Yes"=1, "No"=0),
                            label="PoCs residing in physically safe and secure settlements with access to basic facilities"),
         i.impact2_2 = to_factor(impact2_2),
         HACC01_num = labelled_chr2dbl(HACC01),
         HACC03_num = labelled_chr2dbl(HACC03),
         health_NOacc=case_when(
           HACC03_num==1 & (`HACC04/7`==1 | `HACC04/8`==1 | `HACC04/96`==1 ) ~ 0,
           HACC03_num==1 & (`HACC04/1`==1 | `HACC04/2`==1 | `HACC04/3`==1 |`HACC04/4`==1 |`HACC04/5`==1 |
                              `HACC04/6`==1 | `HACC04/9`==1 | `HACC04/10`==1) ~ 1, TRUE ~ 0),
         HACC_need=HACC01_num + health_NOacc,
         impact2_3=HACC01_num/HACC_need,
         impact2_3=labelled(impact2_3,
                            labels =c("Yes"=1, "No"=0.5, "No"=0),
                            label="PoC has access to health services in the last 30 days when needed"),
         # impact2_3 = ifelse(is.nan(impact2_3), NA, impact2_3),
         i.impact2_3 = to_factor(impact2_3),
         SAF01_num = labelled_chr2dbl(SAF01),
         impact3_3=case_when(
           SAF01_num==1 | SAF01_num==2 ~ 1,
           SAF01_num==3 | SAF01_num==4 | SAF01_num==98 ~ 0, SAF01_num==99 ~ NA_real_),
         impact3_3=labelled(impact3_3, labels =c( "Yes"=1, "No"=0),
                            label="PoC feeling safe walking alone"),
         i.impact3_3 = to_factor(impact3_3),
         GBV01a_num = labelled_chr2dbl(GBV01a), # health services
         GBV01b_num = labelled_chr2dbl(GBV01b), # psycho-social services
         GBV01c_num = labelled_chr2dbl(GBV01c), # safety and security services
         GBV01d_num = labelled_chr2dbl(GBV01d), # legal assistance
         outcome4_1 = case_when(GBV01a_num==1 |  GBV01b_num==1 |  GBV01c_num==1 |  GBV01d_num==1 ~ 1,
                                TRUE ~ 0),
         outcome4_1 = labelled(outcome4_1,
                             labels=c("Yes"=1, "No"=0),
                             label="Poc who know where to access available GBV services"
         ),
         i.outcome4_1 = to_factor(outcome4_1),
         DWE01_num = labelled_chr2dbl(DWE01),
         DWE02_num = labelled_chr2dbl(DWE02),
         DWE03_num = labelled_chr2dbl(DWE03),
         DWE04_num = labelled_chr2dbl(DWE04),
         dwe01_cat=case_when( #Only apartment and house
           (DWE01_num==1 | DWE01_num==2) ~ 1, TRUE ~ 0 ),
         dwe02_cat=case_when( #unimproved floor when earth,sand,clay,mud, dung or other
           (DWE02_num==1 | DWE02_num==2 | DWE02_num==96) ~ 0, TRUE ~ 1 ),
         dwe03_cat=case_when( #unimproved roof all options except metal,wood,ceramic tiles, cement, roofing shingles/sheets
           (DWE03_num==8 |DWE03_num==9 | DWE03_num==10 | DWE03_num==11 |
              DWE03_num==12 | DWE03_num==13 | DWE03_num==8) ~ 1 , TRUE ~ 0),
         dwe04_cat=case_when( #improved wall: cement,stone,bricks,cement blocks, covered adobe, wood planks
           (DWE04_num==10| DWE04_num==11| DWE04_num==12| DWE04_num==13| DWE04_num==14| DWE04_num==15) ~ 1,
           TRUE ~ 0),
         crowding=DWE05/HH01, ############ repeated
         dwe05_cat =case_when( ##if crowding < 3 
           crowding < 3 ~ 1, TRUE ~ 0), ############ repeated
         DWE08_num = labelled_chr2dbl(DWE08),
         DWE09_num = labelled_chr2dbl(DWE09),
         dwe09_cat=case_when( #affordable if HH pays rent and often and always without financial distress
           (DWE08_num==1 & (DWE09_num==1 | DWE09_num==2)) ~ 1, 
           (DWE08_num==1 & (DWE09_num==3 | DWE09_num==4)) ~ 0,  DWE08_num==0 ~ NA_real_),
         outcome9_1=case_when(
           dwe01_cat==0 | dwe02_cat==0 | dwe03_cat==0 | dwe04_cat==0 | dwe05_cat==0 | dwe09_cat==0  ~ 0, 
           dwe01_cat==1 & dwe02_cat==1 & dwe03_cat==1 & dwe04_cat==1 & dwe05_cat==1 & dwe09_cat==1 ~ 1),
         outcome9_1 = labelled(outcome9_1, labels = c("Yes" = 1, "No" = 0),
                               label = "PoC living in habitable and affordable housing"),
         i.outcome9_1 = to_factor(outcome9_1),
         LIGHT01_num = labelled_chr2dbl(LIGHT01), ############ repeated
         LIGHT02_num = labelled_chr2dbl(LIGHT02), ############ repeated
         LIGHT03_num = labelled_chr2dbl(LIGHT03), ############ repeated
         outcome9_2=
           case_when(LIGHT01_num==1 & (LIGHT02_num==1 |LIGHT02_num==3 | LIGHT02_num==5 | LIGHT02_num==6 
                                       | LIGHT02_num==7 | LIGHT02_num==8) & 
                       (LIGHT03_num!=1 | LIGHT03_num!=96 | LIGHT03_num!=98 ) ~ 1,
                     TRUE ~ 0),
         outcome9_2 = labelled(outcome9_2, labels = c("Yes" = 1, "No" = 0),
                               label = "PoC that have energy to ensure lighting"),
         i.outcome9_2 = to_factor(outcome9_2),
         DWA03a_num = labelled_chr2dbl(DWA03a), ############ repeated
         DWA02_num = labelled_chr2dbl(DWA02), ############ repeated
         DWA01_num = labelled_chr2dbl(DWA01), ############ repeated
         DWA04_num = labelled_chr2dbl(DWA04), ############ repeated
         time_DWA=case_when(
           DWA03a_num==1~1, DWA03a_num==2~60), ############ repeated
         time_tot=time_DWA*DWA03b, ############ repeated
         dwa_cond1=case_when( time_tot > 30 ~ 0, TRUE ~ 1), ## accessible under 30 minutes
         dwa_cond2=case_when(DWA01_num!=7 |DWA01_num !=9 |DWA01_num != 13 | DWA01_num != 96 |DWA01_num !=98 ~ 1,
                             TRUE ~ 0), ## protected source
         dwa_cond3=case_when(DWA04_num==1 ~ 0, TRUE ~ 1), ## drinking water was available in the last 30 days
         outcome12_1=case_when(
           (dwa_cond1==1 & dwa_cond2==1 & dwa_cond3==1) ~ 1, TRUE ~ 0),
         outcome12_1 = labelled(outcome12_1, labels = c("Yes" = 1, "No" = 0),
                                label = "PoC using at least basic drinking water services"),
         i.outcome12_1 = to_factor(outcome12_1),
         INC01_num = labelled_chr2dbl(INC01),
         outcome13_2=case_when(INC01_num==1 ~ 1,
                               INC01_num==2 |INC01_num==3 |INC01_num==98 ~ 0 ),
         outcome13_2 = labelled(outcome13_2, labels = c("Yes" = 1, "No" = 0),
                                label = "PoC who self-report positive changes in their income compared
                                to previous year"),
         i.outcome13_2 = to_factor(outcome13_2),
         UNEM01_num = labelled_chr2dbl(UNEM01),
         UNEM02_num = labelled_chr2dbl(UNEM02),
         UNEM03_num = labelled_chr2dbl(UNEM03),
         UNEM04_num = labelled_chr2dbl(UNEM04),
         UNEM05_num = labelled_chr2dbl(UNEM05),
         UNEM06_num = labelled_chr2dbl(UNEM06),
         UNEM07_num = labelled_chr2dbl(UNEM07),
         UNEM08_num = labelled_chr2dbl(UNEM08),
         UNEM09_num = labelled_chr2dbl(UNEM09),
         UNEM10_num = labelled_chr2dbl(UNEM10),
         employed = case_when(UNEM01_num==1 ~ 1,
                              UNEM02_num==1 & UNEM07_num==3 ~ 1,
                              UNEM04_num==1 ~ 1,
                              UNEM02_num==1 & UNEM07_num==1 & (UNEM08_num==1 | UNEM08_num==2) ~ 1,
                              UNEM05_num==1 & UNEM06_num==3 ~ 1,
                              UNEM05_num==1 & (UNEM06_num==1 | UNEM06_num==2) & (UNEM08_num==1 | UNEM08_num==2) ~ 1,
                              filter_elderly==1 ~ NA_real_,
                              TRUE ~ 0),
         i.employed = case_when(employed == 1 ~ "Yes", employed == 0 ~ "No"),
         unemployed = case_when(employed==0 & UNEM09_num==1 & UNEM10_num==1 ~ 1,
                                filter_elderly==1 ~ NA_real_,
                                TRUE ~ 0),
         i.unemployed = case_when(unemployed == 1 ~ "Yes", unemployed == 0 ~ "No"),
         labour_force = case_when(employed==1 | unemployed==1 ~ 1),
         outcome13_3 = unemployed/labour_force,
         outcome13_3 = labelled(outcome13_3, labels = c("Yes" = 1, "No" = 0),
                                label = "Proportion of PoC (working age) who are unemployed"),
         i.outcome13_3 = to_factor(outcome13_3)
  ) |> 
  mutate(
    ii.HH01 = HH01,
    i.REF02 = REF02,
    # i.REF12 = REF12, # already calculated
    i.DISABILITY1 = to_factor(DISABILITY1),
    i.DISABILITY2 = to_factor(DISABILITY2),
    # i.DISABILITY3 = DISABILITY3, already calculated
    i.DISABILITY4 = to_factor(DISABILITY4),
    i.HACC01 = case_when(HACC01 == 1 ~ "Yes",
                         HACC01 == 0 ~ "No"),
    # i.HACC02 = case_when(HACC02 == 1 ~ 'Illness', # select multiple
    #                      HACC02 == 2 ~ 'Injury',
    #                      HACC02 == 3 ~ 'General check-up (not for pregnancy)',
    #                      HACC02 == 4 ~ 'Pre/Postnatal check-up',
    #                      HACC02 == 5 ~ 'Giving birth',
    #                      HACC02 == 99 ~ 'Prefer not to respond',
    #                      HACC02 == 96 ~ 'Other (Specify) |_____|',
    #                      TRUE ~ HACC02),
    # i.HACC02 = HACC02,
    i.HACC03 = case_when(HACC03 == 1 ~ "Yes",
                         HACC03 == 0 ~ "No"),
    # i.HACC04 = case_when(HACC04 == 1 ~ 'Lack of money to pay for care', # select multiple
    #                      HACC04 == 2 ~ 'No medical personal available',
    #                      HACC04 == 3 ~ 'Turned away because facility was full',
    #                      HACC04 == 4 ~ 'Turned away because facility was closed',
    #                      HACC04 == 5 ~ 'Hospital/Clinic not having enough supplies or tests',
    #                      HACC04 == 6 ~ 'Health facility is too far',
    #                      HACC04 == 7 ~ 'Fear of contracting a communicable disease (e.g. COVID-19)',
    #                      HACC04 == 8 ~ 'Lockdown/Travel restrictions',
    #                      HACC04 == 9 ~ 'Cost of transport is too high',
    #                      HACC04 == 10 ~ 'Health facility is destroyed',
    #                      HACC04 == 99 ~ 'Prefer not to answer',
    #                      HACC04 == 96 ~ 'Other (Specify)',
    #                      TRUE ~ HACC04),
    # i.HACC04 = HACC04, 
    i.DWE01 = case_when(DWE01 == 1 ~ 'Apartment',
                        DWE01 == 2 ~ 'House',
                        DWE01 == 3 ~ 'Tent',
                        DWE01 == 4 ~ 'Caravan',
                        DWE01 == 5 ~ 'Collective Center',
                        DWE01 == 6 ~ 'Worksite/Unfinished Home/ Abandoned Building',
                        DWE01 == 7 ~ 'Farm Building',
                        DWE01 == 8 ~ 'School, mosque, church or other religious building',
                        DWE01 == 9 ~ 'Garage, shop, workshop, or other structure not meant as residential space',
                        DWE01 == 96 ~ 'Other (Specify)',
                        TRUE ~ as.character(DWE01)),
    i.DWE02 = case_when(DWE02 == 1 ~ 'Earth/sand',
                        DWE02 == 2 ~ 'Dung',
                        DWE02 == 3 ~ 'Wood planks',
                        DWE02 == 4 ~ 'Palm/bamboo',
                        DWE02 == 5 ~ 'Parquet or polished wood',
                        DWE02 == 6 ~ 'Vinyl or asphalt strips',
                        DWE02 == 7 ~ 'Ceramic tiles',
                        DWE02 == 8 ~ 'Cement',
                        DWE02 == 9 ~ 'Carpet',
                        DWE02 == 96 ~ 'Other (Specify)',
                        TRUE ~ as.character(DWE02)),
    i.DWE03 = case_when(DWE03 == 1 ~ 'No roof',
                        DWE03 == 2 ~ 'Thatch/Palm leaf',
                        DWE03 == 3 ~ 'Sod',
                        DWE03 == 4 ~ 'Rustic mat',
                        DWE03 == 5 ~ 'Palm/bamboo',
                        DWE03 == 6 ~ 'Wood planks',
                        DWE03 == 7 ~ 'Cardboard',
                        DWE03 == 8 ~ 'Metal/tin',
                        DWE03 == 9 ~ 'Wood',
                        DWE03 == 10 ~ 'Calamine/Cement fibre',
                        DWE03 == 11 ~ 'Ceramic tiles',
                        DWE03 == 12 ~ 'Cement',
                        DWE03 == 13 ~ 'Roofing shingles',
                        DWE03 == 96 ~ 'Other (Specify)',
                        TRUE ~ as.character(DWE03)),
    i.DWE04 = case_when(DWE04 == 1 ~ 'No walls',
                        DWE04 == 2 ~ 'Cane/Palm/ Trunks',
                        DWE04 == 3 ~ 'Dirt',
                        DWE04 == 4 ~ 'Bamboo with mud',
                        DWE04 == 5 ~ 'Stone with mud',
                        DWE04 == 6 ~ 'Uncovered adobe',
                        DWE04 == 7 ~ 'Plywood',
                        DWE04 == 8 ~ 'Cardboard',
                        DWE04 == 9 ~ 'Reused wood',
                        DWE04 == 10 ~ 'Cement',
                        DWE04 == 11 ~ 'Stone with lime/ cement',
                        DWE04 == 12 ~ 'Bricks',
                        DWE04 == 13 ~ 'Cement blocks',
                        DWE04 == 14 ~ 'Covered adobe',
                        DWE04 == 15 ~ 'Wood planks/shingles',
                        DWE04 == 96 ~ 'Other (Specify)',
                        TRUE ~ as.character(DWE04)),
    i.DWE05 = DWE05,
    i.DWE08 = case_when(DWE08 == 1 ~ "Yes",
                        DWE08 == 0 ~ "No"),
    i.DWE09 = case_when(DWE09 == 1 ~ 'Always',
                        DWE09 == 2 ~ 'Often',
                        DWE09 == 3 ~ 'Sometimes',
                        DWE09 == 4 ~ 'Never',
                        TRUE ~ as.character(DWE09)),
    i.LIGHT02 = case_when(LIGHT02 == 1 ~ 'Electricity (including solar mini-grids, hybrid mini-grids and national grid)',
                          LIGHT02 == 2 ~ 'Electricity (from diesel generator)',
                          LIGHT02 == 3 ~ 'Solar home system',
                          LIGHT02 == 4 ~ 'Solar-powered lantern or flashlight',
                          LIGHT02 == 5 ~ 'Rechargeable flashlight, mobile, torch or lantern',
                          LIGHT02 == 6 ~ 'Battery powered flashlight, torch or lantern',
                          LIGHT02 == 7 ~ 'Biogas lamp',
                          LIGHT02 == 8 ~ 'LPG lamp',
                          LIGHT02 == 9 ~ 'Gasoline lamp',
                          LIGHT02 == 10 ~ 'Kerosene or paraffin lamp',
                          LIGHT02 == 11 ~ 'Oil lamp',
                          LIGHT02 == 12 ~ 'Candle',
                          LIGHT02 == 13 ~ 'Open fire',
                          LIGHT02 == 96 ~ 'Other, specify',
                          TRUE ~ as.character(LIGHT02)),
    i.LIGHT03 = case_when(LIGHT03 == 1 ~ 'No electricity in household',
                          LIGHT03 == 2 ~ 'National grid connection',
                          LIGHT03 == 3 ~ 'Local mini grid',
                          LIGHT03 == 4 ~ 'Solar home system',
                          LIGHT03 == 5 ~ 'Solar lantern',
                          LIGHT03 == 6 ~ 'Electric generator',
                          LIGHT03 == 7 ~ 'Rechargeable battery',
                          LIGHT03 == 8 ~ 'Dry cell battery / torch',
                          LIGHT03 == 96 ~ 'Other, specify',
                          LIGHT03 == 98 ~ 'Don\'t know',
                          TRUE ~ as.character(LIGHT03)),
    i.DWA01 = case_when(DWA01 == 1 ~ 'Piped Into Dwelling',
                        DWA01 == 2 ~ 'Piped Into Yard/Plot',
                        DWA01 == 3 ~ 'Piped To Neighbor',
                        DWA01 == 4 ~ 'Public Tap/Standpipe',
                        DWA01 == 5 ~ 'Tube Well/Borehole',
                        DWA01 == 6 ~ 'Protected Dug Well',
                        DWA01 == 7 ~ 'Unprotected Dug Well',
                        DWA01 == 8 ~ 'Protected Spring',
                        DWA01 == 9 ~ 'Unprotected Spring',
                        DWA01 == 10 ~ 'Rain Water Collection',
                        DWA01 == 11 ~ 'Tanker Truck/Water Vendor',
                        DWA01 == 12 ~ 'Cart With Small Tank/Drum',
                        DWA01 == 13 ~ 'Surface Water (River, Stream, Pond, Dam, Canal)',
                        DWA01 == 14 ~ 'Bottled Water',
                        DWA01 == 15 ~ 'Sachet Water',
                        DWA01 == 16 ~ 'Water Kiosk',
                        DWA01 == 96 ~ 'Other (Specify)',
                        DWA01 == 98 ~ 'Don\'t Know',
                        TRUE ~ as.character(DWA01)),
    # i.DWA03 = DWA03,
    i.DWA03b = DWA03b,
    i.DWA04 = case_when(DWA04 == 1 ~ "Yes",
                        DWA04 == 0 ~ "No"),
    i.HEA01 = case_when(HEA01 == 1 ~ 'NGO facility (charity, faith-based organization)',
                        HEA01 == 2 ~ 'UNHCR Health Partner (MTI, AHA, IRC)',
                        HEA01 == 3 ~ 'Public Clinics / Hospitals',
                        HEA01 == 4 ~ 'Private Clinics / Hospitals',
                        HEA01 == 5 ~ 'Pharmacy',
                        HEA01 == 6 ~ 'Traditional healer',
                        HEA01 == 96 ~ 'Other, specify',
                        HEA01 == 98 ~ 'Don\'t know',
                        TRUE ~ as.character(HEA01)),
    i.HEA02 = case_when(HEA02 == 1 ~ 'By walk',
                        HEA02 == 2 ~ 'Private car',
                        HEA02 == 3 ~ 'Public taxi',
                        HEA02 == 4 ~ 'Bicycle',
                        HEA02 == 5 ~ 'Boda boda',
                        HEA02 == 96 ~ 'Other, specify',
                        TRUE ~ as.character(HEA02)),
    ii.HEA03 = HEA03,
    i.GBV01a = case_when(GBV01a == 1 ~ 'Yes',
                         GBV01a == 0 ~ 'No',
                         GBV01a == 98 ~ 'Don\'t know',
                         TRUE ~ as.character(GBV01a)),
    i.GBV01b = case_when(GBV01b == 1 ~ 'Yes',
                         GBV01b == 0 ~ 'No',
                         GBV01b == 98 ~ 'Don\'t know',
                         TRUE ~ as.character(GBV01b)),
    i.GBV01c = case_when(GBV01c == 1 ~ 'Yes',
                         GBV01c == 0 ~ 'No',
                         GBV01c == 98 ~ 'Don\'t know',
                         TRUE ~ as.character(GBV01c)),
    i.GBV01d = case_when(GBV01d == 1 ~ 'Yes',
                         GBV01d == 0 ~ 'No',
                         GBV01d == 98 ~ 'Don\'t know',
                         TRUE ~ as.character(GBV01d))
  )

# roster

df_rms_roster_up_age <- df_roster_clean_data |> 
  mutate(HH07 = ifelse((is.na(HH07)|HH07 %in% c("NA")) & !(is.na(HH07_months)|HH07_months %in% c("NA")), ceiling(as.numeric(HH07_months)/12), HH07)) |> # update HH07 based on HH07_months
  mutate(HH07 = ifelse((is.na(HH07)|HH07 %in% c("NA")) & !(is.na(age)|age %in% c("NA")), age, HH07)) |> # update HH07 based on age column
  mutate(HH07_cat = cut(as.numeric(HH07), breaks = c(-1, 4, 17, 59, Inf), labels = c("age_0-4", "age_5-17", "age_18-59", "age_60+"))) |> 
  mutate(HH07_cat2 = cut(as.numeric(HH07), breaks = c(-1, 17, Inf), labels = c("age_0-17", "age_18-60+"))) |> 
  mutate(i.HH04 = case_when(HH04 == 1 ~ "Female",
                            HH04 == 2 ~ "Male",
                            HH04 == 96 ~ "Other",
                            HH04 == 99 ~ "Prefer not to respond",
                            TRUE ~ as.character(HH04)),
         i.HH03 = case_when(HH03 == 1 ~ 'Household Head',
                            HH03 == 2 ~ 'Spouse/Partner',
                            HH03 == 3 ~ 'Son/Daughter',
                            HH03 == 4 ~ 'Son-in-law / Daughter-in-law',
                            HH03 == 5 ~ 'Grandchild',
                            HH03 == 6 ~ 'Parent',
                            HH03 == 7 ~ 'Parent-in-law',
                            HH03 == 8 ~ 'Brother/Sister',
                            HH03 == 9 ~ 'Brother-in-law/Sister-in-law',
                            HH03 == 10 ~ 'Uncle/Aunt',
                            HH03 == 11 ~ 'Niece/Nephew',
                            HH03 == 12 ~ 'Other relative',
                            HH03 == 13 ~ 'Adopted/Foster child',
                            HH03 == 14 ~ 'Servant (live-in)',
                            HH03 == 15 ~ 'Other (not-related)',
                            HH03 == 98 ~ 'Don’t know',
                            HH03 == 99 ~ 'Prefer not to respond',
                            TRUE ~ as.character(HH03)
  ))


# Analysis ----------------------------------------------------------------

# combine main data and loop

df_main_to_combine <- df_rms_main_composites_extra |> 
  mutate(uuid_respodent = paste0(uuid, "_", name_respondent))

df_roster_to_combine <- df_rms_roster_up_age |> 
  mutate(uuid_respodent = paste0(`_submission__uuid`, "_fam_name", personId)) |> 
  select(uuid_respodent, i.HH04, HH07_cat, i.HH03)

df_combined_main_roster <- df_main_to_combine |> 
  left_join(df_roster_to_combine, by = "uuid_respodent") |> 
  select(-uuid_respodent)

openxlsx::write.xlsx(df_combined_main_roster, file = paste0("outputs/", butteR::date_file_prefix(), 
                                                            "_clean_data_with_composites_unhcr_rms.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")
# set up design objects
ref_svy <- as_survey(.data = df_combined_main_roster)

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy,
                                                           input_dap = dap)


# prepare analysis output ----------------------------------------------------------

combined_analysis <- df_main_analysis
 
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^ii\\."), str_replace(string = variable, pattern = "^ii\\.", replacement = ""), variable)
         ) %>% 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) %>% 
  relocate(label, .after = variable) %>% 
  mutate(label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse((select_type %in% c("integer") | str_detect(string = variable, pattern = "^ii\\.")) & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>%
  select(`Question`= label, variable, `choices/options` = variable_val, `Results(mean/percentage)` = `mean/pct`, n_unweighted, population, subset_1_name, subset_1_val)

write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_rms.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_rms.csv"), na="")
