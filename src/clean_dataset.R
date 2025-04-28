library("readxl")
library("dplyr")
library("stringr")

## Columns: "year"  "id_"   "hrs2"  "occ10"   "indus10"  "age"   "usetech"
## Dimensions: 3544    7
gss_raw <- read_excel("~/lab-2-team-no-l-s/data/raw/GSS_age_usetech.xlsx")

## Columns: "Category"   "Code"   "Job_Title"
## Dimensions: 451   3
occ_titles_raw <- read.csv("~/lab-2-team-no-l-s/data/raw/BLS_Occupation_Titles.csv")

## Columns: "Industry_Names"
## Dimensions: 115   1
ind_names_raw <- read.csv("~/lab-2-team-no-l-s/data/raw/BLS_Industry_Names.csv")

## Slice NAICS code from "Industry_Names" column and add it to new column "NAICS_cd".
ind_names_NAICS_cd <- ind_names_raw %>%
  mutate(NAICS_cd = as.numeric(substring(str_extract(Industry_Names, "NAICS [0-9]+"), 7))) %>%
  mutate(Industry_Names = str_trim(str_remove(Industry_Names, "\\(NAICS [0-9]+\\)")))

## Filter rows with values that can cause issues with analysis
## Columns: "year"  "id_"  "wrkstat"  "hrs1"  "occ10"  "indus10"  "age"  "usetech"
## Dimensions: 1381   8
gss_filtered <- gss_raw %>%
      filter(year == "2022") %>%
      filter(wrkstat == "Working full time") %>%
      filter((grepl("^[0-9]+$", age)) & (grepl("^[0-9.]+$", usetech))) %>%
      mutate(hrs1 = if_else(hrs1=="89+ hrs", "90", hrs1)) %>%
      filter(grepl("^[0-9]+$", hrs1)) %>%
      filter(!( (indus10 %in% c(".n:  No answer", ".i:  Inapplicable")) | 
                  (occ10 %in% c(".n:  No answer", ".i:  Inapplicable")) )) %>%
      mutate(across(c("hrs1", "age", "usetech"), as.numeric))

## Classification of respondents' occupation into: Blue Collar, White Collar and Service

# Initialize an empty vector to store matched job codes
matched_job_codes <- character(nrow(gss_filtered))

# Loop through each occupation in A
for (i in seq_len(nrow(gss_filtered))) {
  occ <- gss_filtered$occ10[i]
  
  # Compute string distance with all job titles
  distances <- stringdist::stringdist(occ, occ_titles_raw$Job_Title, method = "jw")  # Jaro-Winkler string match test
  
  # Find the index of the closest match
  best_match_index <- which.min(distances)
  
  # Assign the corresponding job code
  matched_job_codes[i] <- occ_titles_raw$Code[best_match_index]
}

# Add it as a new column
gss_job_code <- gss_filtered %>%
  mutate(job_code = matched_job_codes)

## Manual correction for job codes that were not assigned correctly
gss_job_code<- gss_job_code %>% 
  mutate(job_code = case_when(
    occ10=="Personal care aides" ~ "K469",
    occ10=="Stock clerks and order fillers" ~ "D374",
    occ10=="Paralegals and legal assistants" ~ "A234",
    occ10=="Computer and information systems managers" ~ "B022",
    occ10=="Military, rank not specified" ~ "K427",
    occ10=="First-line supervisors of production and operating workers" ~ "E628",
    occ10=="Aircraft pilots and flight engineers" ~ "A226",
    occ10=="Flight attendants" ~ "K463",
    occ10=="Food processing workers, all other" ~ "E685",
    occ10=="Office clerks, general" ~ "D379",
    occ10=="Automotive service technicians and mechanics" ~ "E506",
    occ10=="Retail salespersons" ~ "C274",
    occ10=="Massage therapists" ~ "K105",
    occ10=="Marketing and sales managers" ~ "B013",
    occ10=="Preschool and kindergarten teachers" ~ "A155",
    occ10=="Social and community service managers" ~ "B021",
    occ10=="Police and sheriff's patrol officers" ~ "K418",
    occ10=="Forest and conservation workers" ~ "H495",
    occ10=="Human Resources Workers" ~ "B027",
    occ10=="Postsecondary teachers" ~ "A113",
    occ10=="Producers and directors" ~ "A187",
    occ10=="Welding, soldering, and brazing workers" ~ "F784",
    occ10=="Miscellaneous legal support workers" ~ "A234",
    occ10=="Information security analysts" ~ "A064",
    TRUE ~ job_code
  ))

# Add new column job_ctg: Blue Collar, White Collar and Service
gss_job_ctg <- gss_job_code %>%
  mutate(job_ctg = if_else(substr(job_code, 1, 1) %in% c("A", "B", "C", "D"), "White Collar", 
                           if_else(substr(job_code, 1, 1) %in% c("E", "F", "G", "H"), "Blue Collar", "Service")))


## Classification of respondents' industry into 10 super sector groups: 
## 1. "Natural Resources and Mining", 2. "Construction", 3. "Manufacturing", 
## 4. "Trade, Transportation, and Utilities", 5. "Information", 6. "Financial Activities",
## 7. "Professional and Business Services", 8. "Education and Health Services", 9. "Leisure and Hospitality",
## 10. "Other Services (except Public Administration)"

# Initialize an empty vector to store matched job codes
matched_NAICS_codes <- character(nrow(gss_job_ctg))

# Loop through each industry name in gss_filtered
for (i in seq_len(nrow(gss_job_ctg))) {
  indus <- gss_job_ctg$indus10[i]
  
  # Compute string distance with all industry
  distances <- stringdist::stringdist(indus, ind_names_NAICS_cd$Industry_Names, method = "jw")  # Jaro-Winkler string match test for fuzzy matching
  
  # Find the index of the closest match
  best_match_index <- which.min(distances)
  
  # Assign the corresponding NAICS code
  matched_NAICS_codes[i] <- ind_names_NAICS_cd$NAICS_cd[best_match_index]
}

# Add it as a new column
gss_ind_NAICS_cd <- gss_job_ctg %>%
  mutate(ind_sup_sect_cd = as.numeric(matched_NAICS_codes))

## Manual correction for industry super sectory codes that were not assigned correctly
gss_ind_NAICS_cd <- gss_ind_NAICS_cd %>% 
  mutate(ind_sup_sect_cd = case_when(
    indus10 %in% c("Not specified retail trade") ~ 44,
    indus10 %in% c("Ship and boat building") ~ 336,
    indus10 %in% c("Grocery stores") ~ 445,
    indus10 %in% c("Pharmacies and drug stores") ~ 446,
    indus10 %in% c("Miscellaneous general merchandise stores") ~ 452,
    indus10 %in% c("Mail order houses") ~ 454,
    indus10 %in% c("Home health care services", "Outpatient care centers") ~ 621,
    indus10 %in% c("Residential care facilities, without nursing") ~ 623,
    indus10 %in% c("Other health care services", "Individual and family services") ~ 624,
    indus10 %in% c("Private households") ~ 814,
    indus10 %in% c("Legal services", "Computer systems design and related services", "Architectural, engineering, and related services") ~ 54,
    indus10 %in% c("Active duty military personnel, all other", "Car washes") ~ 81,
    indus10 %in% c("Beauty salons", "Barber shops") ~ 812,
    indus10 %in% c("Child day care services") ~ 62,
    indus10 %in% c("Community food and housing, and emergency services") ~ 624,
    TRUE ~ ind_sup_sect_cd
  ))
  
# Add new column industry
gss_final <- gss_ind_NAICS_cd %>%
  mutate(industry = case_when(
    ind_sup_sect_cd %in% c(11, 111, 112, 113, 114, 115, 21, 211, 212, 213) ~ "Natural Resources and Mining",
    ind_sup_sect_cd %in% c(23, 236, 237, 238) ~ "Construction",
    ind_sup_sect_cd %in% c(31, 32, 33, 311, 312, 313, 314, 315, 316, 321, 322, 323, 324, 325, 326, 327, 331, 332, 333, 334, 335, 336, 337, 339) ~ "Manufacturing",
    ind_sup_sect_cd %in% c(42, 423, 424, 425, 44, 45, 441, 442, 443, 444, 445, 446, 447, 448, 451, 452, 453, 454, 48, 49, 481, 482, 483, 484, 485, 486, 487, 488, 491, 492, 493, 22) ~ "Trade, Transportation, and Utilities",
    ind_sup_sect_cd %in% c(51, 511, 512, 515, 516, 517, 518, 519) ~ "Information",
    ind_sup_sect_cd %in% c(52, 521, 522, 523, 524, 525, 53, 531, 532, 533) ~ "Financial Activities",
    ind_sup_sect_cd %in% c(54, 55, 56, 561, 562) ~ "Professional and Business Services",
    ind_sup_sect_cd %in% c(61, 62, 621, 622, 623, 624) ~ "Education and Health Services",
    ind_sup_sect_cd %in% c(71, 711, 712, 713, 72, 721, 722) ~ "Leisure and Hospitality",
    ind_sup_sect_cd %in% c(81, 811, 812, 813, 814) ~ "Other Services (except Public Administration)",
    TRUE ~ "No Match"
  ))

write.csv(gss_final, "~/lab-2-team-no-l-s/data/interim/GSS_final_set.csv", row.names = FALSE)

## Divide the dataset gss_final into exploration set and confirmation set
row_ct = nrow(gss_final) ## 1381
row_indexes = sample(row_ct, row_ct*0.3) ## 414
row_indexes_excluded = setdiff(seq_len(row_ct), row_indexes) ## 967

## Exlploratory dataset
## Columns:  "year", "id_", "wrkstat", "hrs1", "occ10", "indus10", "age"
## "usetech", "job_code", "job_ctg", "ind_sup_sect_cd", "industry" 
## Dimensions 414  12
gss_explore_ds <- gss_final[row_indexes, ]
write.csv(gss_explore_ds, "~/lab-2-team-no-l-s/data/interim/GSS_exploration_set.csv", row.names = FALSE)

## Confirmation dataset
## Columns:  "year", "id_", "wrkstat", "hrs1", "occ10", "indus10", "age"
## "usetech", "job_code", "job_ctg", "ind_sup_sect_cd", "industry" 
## Dimensions: 967  12  
gss_confirm_ds <- gss_final[row_indexes_excluded, ]
write.csv(gss_confirm_ds, "~/lab-2-team-no-l-s/data/interim/GSS_confirmation_set.csv", row.names = FALSE)
