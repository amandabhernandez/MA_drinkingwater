### AUTHOR: AHz
### LAST EDIT: 2022-05-13
### WRITTEN IN: R version 4.0.5
### Purpose: generate an upset plot of co-occurring drinking water 
### contaminants for all PWSs.


###############################################################################
# 0. SETUP  ###################################################################
###############################################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(ComplexUpset)

###############################################################################
# 1. LOAD DATA  ###############################################################
###############################################################################

#read in MA water data
waterdat_df <- read_csv("data/MA DW data.csv") %>%
  mutate(`Raw or Finished` == "F") %>% 
  #set compound groups
  mutate(chem_group = case_when(`Chemical Name` %in% c("TOTAL TRIHALOMETHANES",
                                                       "HALOACETIC ACIDS",
                                                       "BROMATE") ~ "DBP",
                                `Chemical Name` %in% c("ALACHLOR", "BENZO(A)PYRENE", "CHLORDANE", "DI(2-ETHYLHEXYL)PHTHALATE",
                                                       "ETHYLENE DIBROMIDE (EDB)", "HEPTACHLOR", "HEPTACHLOR EPOXIDE",
                                                       "HEXACHLOROBENZENE", "PENTACHLOROPHENOL", "TOXAPHENE", "DIBROMOCHLOROPROPANE") ~ "SOC",
                                `Chemical Name` %in% c("1,2-DICHLOROETHANE", "1,2-DICHLOROPROPANE",
                                                       "BENZENE", "CARBON TETRACHLORIDE", "DICHLOROMETHANE",
                                                       "TETRACHLOROETHYLENE", "TRICHLOROETHYLENE", "VINYL CHLORIDE") ~ "VOC",
                                `Chemical Name` == "ARSENIC" ~ "INORGANICS",
                                `Chemical Name` == "URANIUM" ~ "RADIONUCLIDES",
                                TRUE ~ `Chemical Name`))


###############################################################################
# 2. GET DETECTION FREQUENCIES  ###############################################
###############################################################################


chem_det_freq <- waterdat_df %>% 
  group_by(chem_group, `Chemical Name`) %>% 
  summarize(n = n(), 
            n_nd = length(unique(samp_id[which(nd_flag == "ND")])),
            n_detect = length(unique(samp_id[which(nd_flag == "Detect")])),
            n_exceed_MCL = length(unique(samp_id[which(as.numeric(Result)>MCL)])),
            pct_nd = n_nd/n(),
            pct_det = round((n_detect/n())*100, 2),
            pct_exceedMCL = round((n_exceed_MCL/n())*100, 2)) %>% 
  select(chem_group, `Chemical Name`, n, pct_det, pct_exceedMCL) %>% 
  rename(`Chemical Group` = chem_group,
         `Number of samples` = n,
         `Percent detected` = pct_det,
         `Percent exceeding an MCL` = pct_exceedMCL)

#write_csv(chem_det_freq, "water study detection frequencies (table 1).csv")



###############################################################################
# 3. CREATE UPSET PLOT  #######################################################
###############################################################################

chems_det_per_yr <- waterdat_df %>%
  group_by(PWSID, year(date), chem_group, `Chemical Name`) %>%
  summarize(n_det = length(unique(`Location ID`[which(nd_flag == "Detect")]))) %>%
  mutate(hasdet = case_when(n_det > 0 ~ "1",
                            TRUE ~ "0")) %>%
  rename(year = `year(date)`) %>% 
  ungroup()


#set up dataframe for upset plot
upset_df <- chems_det_per_yr %>%
  filter(year == "2019") %>% 
  select(-n_det, -chem_group) %>% 
  mutate(hasdet = as.integer(hasdet)) %>%
  pivot_wider(names_from = `Chemical Name`, values_from = hasdet, values_fill = 0) %>% 
  data.frame() %>% 
  clean_names(case = c("upper_camel")) %>% 
  rename(`1,2-Dichloroethane` = X1_2Dichloroethane,
         `1,2-Dichloropropane` = X1_2Dichloropropane,
         `Di(2-Ethylhexyl)Phthalate` = Di2EthylhexylPhthalate,
         `Ethylene Dibromide (EDB)` = EthyleneDibromideEdb)

#get list of compounds to include
upset_chems_include <- upset_df %>%
  adorn_totals("row") %>% 
  filter(Pwsid == "Total") %>% 
  select(-c(Pwsid, Year)) %>% 
  pivot_longer(names_to = "Chemical Name", values_to = "n_det", everything()) %>% 
  filter(n_det > 0) %>% 
  pull(`Chemical Name`) %>% 
  unique()


# make upset plot (note: only works if chemical is in set -- comment out chemicals 
  # that do not appear)
ComplexUpset::upset(
  upset_df,
  upset_chems_include, 
  base_annotations=list(
    'Total # of PWSs'=intersection_size(
      counts=FALSE,
    ) 
  ),
  matrix=(
    intersection_matrix(geom=geom_point(shape='circle filled', size=3))
    + scale_color_manual(
      values=c('TotalTrihalomethanes'='#88D080', 
               'HaloaceticAcids'='#88D080', 
               'Bromate'='#88D080', 
               '1,2-Dichloropropane'='#3CA8BC',
               #'CarbonTetrachloride'='#3CA8BC',
               'Dichloromethane'='#3CA8BC',
               'Tetrachloroethylene'='#3CA8BC',
               'Trichloroethylene'='#3CA8BC',
               #'VinylChloride'='#3CA8BC',
               'Arsenic'='#98D9E4',
               'Di(2-Ethylhexyl)Phthalate'='#EF8A0D',
               'Pentachlorophenol'='#EF8A0D',
               'Toxaphene'='#EF8A0D',
               'Uranium'='#FCC76F',
               'Hexachlorobenzene'='#EF8A0D',
               'Ethylene Dibromide (EDB)'='#EF8A0D'),
      guide='none'
    )
  ),
  queries=list(
    upset_query(set='TotalTrihalomethanes', fill='#88D080'),
    upset_query(set='HaloaceticAcids', fill='#88D080'),
    upset_query(set='Bromate', fill='#88D080'),
    upset_query(set='1,2-Dichloropropane', fill='#3CA8BC'),
    #upset_query(set='CarbonTetrachloride', fill='#3CA8BC'),
    upset_query(set='Dichloromethane', fill='#3CA8BC'),
    upset_query(set='Tetrachloroethylene', fill='#3CA8BC'),
    upset_query(set='Trichloroethylene', fill='#3CA8BC'),
    # upset_query(set='VinylChloride', fill='#3CA8BC'),
    # upset_query(set='1,2-Dichloroethane', fill='#3CA8BC'),
    # upset_query(set='Benzene', fill='#3CA8BC'),
    upset_query(set='Arsenic', fill='#98D9E4'),
    # upset_query(set='Alachlor', fill='#EF8A0D'),
    # upset_query(set='BenzoAPyrene', fill='#EF8A0D'),
    # upset_query(set='Chlordane', fill='#EF8A0D'),
    upset_query(set='Di(2-Ethylhexyl)Phthalate', fill='#EF8A0D'),
    upset_query(set='Ethylene Dibromide (EDB)', fill='#EF8A0D'),
    # upset_query(set='Heptachlor', fill='#EF8A0D'),
    # upset_query(set='HeptachlorEpoxide', fill='#EF8A0D'),
    upset_query(set='Hexachlorobenzene', fill='#EF8A0D'),
    upset_query(set='Pentachlorophenol', fill='#EF8A0D'),
    upset_query(set='Toxaphene', fill='#EF8A0D'),
    upset_query(set='Uranium', fill='#FCC76F')
  ),
  stripes='white',
  width_ratio=0.1,
  sort_intersections='descending',
  min_degree=1,
)

ggsave("upset plot 2019.png", height = 9, width = 16, units = "in")

#get numerical counts of sets
upset_counts <- chems_det_per_yr %>%
  ungroup() %>% 
  filter(year == "2019") %>% 
  filter(hasdet == "1") %>% 
  group_by(PWSID, year) %>% 
  arrange(`Chemical Name`) %>% 
  summarize(set = paste0(unique(`Chemical Name`), collapse = ",")) %>% 
  group_by(set) %>% 
  count() %>%
  arrange(desc(n))


