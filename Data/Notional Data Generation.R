#### Analysis of ST542 Consulting Project
## Authors: Tyler Pollard, Mahmoud Harding, Maxwell Su, Umesh Rao
## Date Created: 31 May 2024
## Date Modified:

# Project Description ----
## Project Overview:
### The goal of this NSF funded project is to provide community-based engineering design experiences for
### underserved middle school students from rural NC aimed to improve their cognitive (STEM content
### knowledge and career awareness) and noncognitive (interest, self-efficacy and STEM identity) outcomes, 
### and ultimately lead to their increased participation in STEM fields, particularly engineering. This project
### will also explore the impact of the program on middle school teachers' STEM content and pedagogical 
### knowledge and awareness of STEM career pathways. The project team aims to accomplish the overarching 
### project goal by: 
###   1) developing a 3-part engineering design elective course for middle school students 
###   2) provide mentoring of middle school students by undergraduate engineering students in the 
###      Minority Engineering Program at NCSU 
###   3) provide supplemental STEM experiences where students engage in STEM enrichment activities outside
###      the classroom such as STEM Saturday Academy and Summer Camp programs and industry and university tours.

## Research Questions:
###   1) How and to what degree does the engineering design-focused program impact students' disciplinary 
###      based knowledge and practices in STEM?
###   2) How and to what degree does the engineering design-focused program impact students' STEM interest,
###      STEM identity, and STEM self-efficacy?
###   3) How and to what degree does the engineering design-focused program impact teacher STEM content and 
###      pedagogical knowledge and awareness of STEM educational pathways and occupations?

## Data Sources:
### Quantitative:
####  pre/post S-STEM student survey
####  pre/post T-STEM teacher survey
### Qualitative:
####  student artifacts (work samples)
####  observations
####  student focus groups
####  teacher interviews

## Task:
### We have collected some quantitative and qualitative data (see above) and need assistance with the 
### analysis. In addition, we are in the process of a slight redesign of the study and would like some help
### with developing the statistical design. 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Libraries ----
## Data Reading
library(googlesheets4)
library(readxl)

## Data Writing
library(writexl)

## Data Manipulation
library(stringr) # Manipulate strings

## Data Analysis
library(DescTools)

## Load this package last to reduce package conflictions with dplyr
library(tidyverse) 

# Socio-Demographic Data ===================================================================================
##  School: West Edgecombe Middle School (WEMS), Phillips Middle School (PSM)
##  Grade: 6th, 7th, 8th
##  Teacher: Mr.O'Shea, Ms.Manuel, Mr.Suitter, Other
##  Year: 2021, 2022, 2023, 2024
##  Semester: Fall, Spring
##  Gender: Male, Female, Other
##  Race: American Indian/Alaska Native, Asian, Black/African American, Native Hawaiian/Other Pacific Islander
##        White/Caucasian, Hispanic/Latino, Multracial, Other

## Generate Data frame ----
nClass <- 15
nSchools <- 2
nGrades <- 3
nSTEM <- nClass*nSchools*nGrades

# Gender and race distribution was pulled from 2021 statistics of 
# https://www.codewizardshq.com/stem-statistics/
set.seed(52)
SocioDem_df <- data.frame(
  ID = 1:nSTEM,
  School = sample(c("WEMS", "PSM"), size = nSTEM, prob = c(0.5, 0.5), replace = TRUE),
  Grade = sample(c("6th", "7th", "8th"), size = nSTEM, prob = c(1/3, 1/3, 1/3), replace = TRUE),
  Gender = sample(c("Male", "Female"), size = nSTEM, prob = c(.65, .35), replace = TRUE), 
  Race = sample(c("American Indian/Alaska Native", # 0.6%
                  "Asian", # 10%
                  "Black/African American", # 9%
                  "Native Hawaiian/Other Pacific Islander", # 1.4%/3
                  "White/Caucasian", # 64%
                  "Hispanic/Latino", # 15%
                  "Multracial", # 1.4%/3
                  "Other"), # 1.4%/3
                size = nSTEM, 
                prob = c(0.006, 0.10, 0.09, 0.014/3, 0.64, 0.15, 0.014/3, 0.014/3),
                replace = TRUE)
)
unique(SocioDem_df$School)
unique(SocioDem_df$Grade)
unique(SocioDem_df$Gender)
unique(SocioDem_df$Race)

## Unique Combos of SocDem ----
## Create a data frame of all possible socio demographic combinations to generate fixed effects 
SocioDem_unique <- expand.grid(
  School = unique(SocioDem_df$School),
  Grade = unique(SocioDem_df$Grade),
  Gender = unique(SocioDem_df$Gender),
  Race = unique(SocioDem_df$Race)
) |>
  arrange(School, Grade, Gender, Race)

## Add Fixed Effects ----
### Math ----
SocioDem_unique$StartingMathScore <- NA
set.seed(52)
for(i in 1:nrow(SocioDem_unique)){
  # School Impact
  if(SocioDem_unique$School[i] == "WEMS"){
    StartingMathScoreSchool <- rbeta(1, 2.5, 2)*5
  }else if(SocioDem_unique$School[i] == "PSM"){
    StartingMathScoreSchool <- rbeta(1, 4, 1)*5
  }
  
  # Grade Impact
  if(SocioDem_unique$Grade[i] == "6th"){
    StartingMathScoreGrade <- StartingMathScoreSchool - 2
  }else if(SocioDem_unique$Grade[i] == "7th"){
    StartingMathScoreGrade <- StartingMathScoreSchool - 1
  }else if(SocioDem_unique$Grade[i] == "8th"){
    StartingMathScoreGrade <- StartingMathScoreSchool
  }
  
  # Gender Impact
  if(SocioDem_unique$Gender[i] == "Male"){
    StartingMathScoreGender <- StartingMathScoreGrade + 0.9
  }else if(SocioDem_unique$Gender[i] == "Female"){
    StartingMathScoreGender <- StartingMathScoreGrade + 0.5
  }
  
  # Race Impact
  if(SocioDem_unique$Race[i] == "American Indian/Alaska Native"){
    StartingMathScoreRace <- StartingMathScoreGender + 0.1
  }else if(SocioDem_unique$Race[i] == "Asian"){
    StartingMathScoreRace <- StartingMathScoreGender + 0.9
  }else if(SocioDem_unique$Race[i] == "Black/African American"){
    StartingMathScoreRace <- StartingMathScoreGender + 0.4
  }else if(SocioDem_unique$Race[i] == "Native Hawaiian/Other Pacific Islander"){
    StartingMathScoreRace <- StartingMathScoreGender
  }else if(SocioDem_unique$Race[i] == "White/Caucasian"){
    StartingMathScoreRace <- StartingMathScoreGender + 0.5
  }else if(SocioDem_unique$Race[i] == "Hispanic/Latino"){
    StartingMathScoreRace <- StartingMathScoreGender + 0.2
  }else if(SocioDem_unique$Race[i] == "Multracial"){
    StartingMathScoreRace <- StartingMathScoreGender
  }else if(SocioDem_unique$Race[i] == "Other"){
    StartingMathScoreRace <- StartingMathScoreGender
  }
  
  # Math Q1 score
  StartingMathScore_temp <- round(StartingMathScoreRace)
  SocioDem_unique$StartingMathScore[i] <- ifelse(StartingMathScore_temp <= 1, 1, 
                                                 ifelse(StartingMathScore_temp >= 5, 5, StartingMathScore_temp))
}


### Science ----
SocioDem_unique$StartingScienceScore <- NA
set.seed(52)
for(i in 1:nrow(SocioDem_unique)){
  # School Impact
  if(SocioDem_unique$School[i] == "WEMS"){
    StartingScienceScoreSchool <- rbeta(1, 2.5, 2)*5
  }else if(SocioDem_unique$School[i] == "PSM"){
    StartingScienceScoreSchool <- rbeta(1, 3.5, 1)*5
  }
  
  # Grade Impact
  if(SocioDem_unique$Grade[i] == "6th"){
    StartingScienceScoreGrade <- StartingScienceScoreSchool - 1.8
  }else if(SocioDem_unique$Grade[i] == "7th"){
    StartingScienceScoreGrade <- StartingScienceScoreSchool - 1.2
  }else if(SocioDem_unique$Grade[i] == "8th"){
    StartingScienceScoreGrade <- StartingScienceScoreSchool
  }
  
  # Gender Impact
  if(SocioDem_unique$Gender[i] == "Male"){
    StartingScienceScoreGender <- StartingScienceScoreGrade + 0.1
  }else if(SocioDem_unique$Gender[i] == "Female"){
    StartingScienceScoreGender <- StartingScienceScoreGrade + 0.6
  }
  
  # Race Impact
  if(SocioDem_unique$Race[i] == "American Indian/Alaska Native"){
    StartingScienceScoreRace <- StartingScienceScoreGender + 0.15
  }else if(SocioDem_unique$Race[i] == "Asian"){
    StartingScienceScoreRace <- StartingScienceScoreGender + 0.85
  }else if(SocioDem_unique$Race[i] == "Black/African American"){
    StartingScienceScoreRace <- StartingScienceScoreGender + 0.45
  }else if(SocioDem_unique$Race[i] == "Native Hawaiian/Other Pacific Islander"){
    StartingScienceScoreRace <- StartingScienceScoreGender
  }else if(SocioDem_unique$Race[i] == "White/Caucasian"){
    StartingScienceScoreRace <- StartingScienceScoreGender + 0.5
  }else if(SocioDem_unique$Race[i] == "Hispanic/Latino"){
    StartingScienceScoreRace <- StartingScienceScoreGender + 0.2
  }else if(SocioDem_unique$Race[i] == "Multracial"){
    StartingScienceScoreRace <- StartingScienceScoreGender
  }else if(SocioDem_unique$Race[i] == "Other"){
    StartingScienceScoreRace <- StartingScienceScoreGender
  }
  
  # Science Q1 score
  StartingScienceScore_temp <- round(StartingScienceScoreRace)
  SocioDem_unique$StartingScienceScore[i] <- ifelse(StartingScienceScore_temp <= 1, 1, 
                                                    ifelse(StartingScienceScore_temp >= 5, 5, StartingScienceScore_temp))
}


### Engineering and Tech ----
SocioDem_unique$StartingEngTechScore <- NA
set.seed(52)
for(i in 1:nrow(SocioDem_unique)){
  # School Impact
  if(SocioDem_unique$School[i] == "WEMS"){
    StartingEngTechScoreSchool <- rbeta(1, 3, 2)*5
  }else if(SocioDem_unique$School[i] == "PSM"){
    StartingEngTechScoreSchool <- rbeta(1, 4, 1)*5
  }
  
  # Grade Impact
  if(SocioDem_unique$Grade[i] == "6th"){
    StartingEngTechScoreGrade <- StartingEngTechScoreSchool - 2.2
  }else if(SocioDem_unique$Grade[i] == "7th"){
    StartingEngTechScoreGrade <- StartingEngTechScoreSchool - 1.2
  }else if(SocioDem_unique$Grade[i] == "8th"){
    StartingEngTechScoreGrade <- StartingEngTechScoreSchool
  }
  
  # Gender Impact
  if(SocioDem_unique$Gender[i] == "Male"){
    StartingEngTechScoreGender <- StartingEngTechScoreGrade + 0.3
  }else if(SocioDem_unique$Gender[i] == "Female"){
    StartingEngTechScoreGender <- StartingEngTechScoreGrade + 0.8
  }
  
  # Race Impact
  if(SocioDem_unique$Race[i] == "American Indian/Alaska Native"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender + 0.05
  }else if(SocioDem_unique$Race[i] == "Asian"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender + 0.95
  }else if(SocioDem_unique$Race[i] == "Black/African American"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender + 0.4
  }else if(SocioDem_unique$Race[i] == "Native Hawaiian/Other Pacific Islander"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender
  }else if(SocioDem_unique$Race[i] == "White/Caucasian"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender + 0.55
  }else if(SocioDem_unique$Race[i] == "Hispanic/Latino"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender + 0.15
  }else if(SocioDem_unique$Race[i] == "Multracial"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender
  }else if(SocioDem_unique$Race[i] == "Other"){
    StartingEngTechScoreRace <- StartingEngTechScoreGender
  }
  
  # EngTech Q1 score
  StartingEngTechScore_temp <- round(StartingEngTechScoreRace)
  SocioDem_unique$StartingEngTechScore[i] <- ifelse(StartingEngTechScore_temp <= 1, 1, 
                                                    ifelse(StartingEngTechScore_temp >= 5, 5, StartingEngTechScore_temp))
}

# S-STEM Survey ===========================================================================================
## Read in empty STEM survey ----
emptySTEM <- read_excel("Data/S-STEM+-+DeSIRE_June+6,+2024_09.13_blank.xlsx")
STEMquestions <- unlist(emptySTEM[1,])
STEMquestions

STEMcolnames <- colnames(emptySTEM)

## Generate Data ----
notionalSSTEM1 <- data.frame(
  matrix(vector(), nSTEM, 92)
)
colnames(notionalSSTEM1) <- STEMcolnames

### Populate Socio-Demographic Data ----
notionalSSTEM2 <- notionalSSTEM1 |>
  mutate(
    ResponseId = SocioDem_df$ID,
    Q2 = SocioDem_df$School,
    Q3 = SocioDem_df$Grade,
    Q21 = SocioDem_df$Gender,
    Q22 = SocioDem_df$Race
  )

### Generate Question Responses ----
SocioDem_unique2 <- SocioDem_unique |>
  rename(
    "Q2" = School,
    "Q3" = Grade,
    "Q21" = Gender,
    "Q22" = Race,
    "Q6_1" = StartingMathScore,
    "Q23_1" = StartingScienceScore,
    "Q24_1" = StartingEngTechScore
  )

notionalSSTEM3 <- left_join(
  notionalSSTEM2 |> select(-Q6_1, -Q23_1, -Q24_1),
  SocioDem_unique2
) |>
  select(
    STEMcolnames
  )

notionalSSTEM4 <- notionalSSTEM3 |>
  mutate(
    #### Update Starting Values ----
    ## Adding some variance so each level of Q1 is the exact same
    Q6_1 = Q6_1 + sample(c(-1, 0, 1), size = nSTEM, prob = c(1/3, 1/3, 1/3), replace = TRUE),
    Q23_1 = Q23_1 + sample(c(-1, 0, 1), size = nSTEM, prob = c(1/3, 1/3, 1/3), replace = TRUE),
    Q24_1 = Q24_1 + sample(c(-1, 0, 1), size = nSTEM, prob = c(1/3, 1/3, 1/3), replace = TRUE)
  ) |>
  mutate(
    Q6_1 = ifelse(Q6_1 <= 1, 1,
                  ifelse(Q6_1 >= 5, 5, Q6_1)),
    Q23_1 = ifelse(Q23_1 <= 1, 1,
                  ifelse(Q23_1 >= 5, 5, Q23_1)),
    Q24_1 = ifelse(Q24_1 <= 1, 1,
                  ifelse(Q24_1 >= 5, 5, Q24_1))
  ) |>
  mutate(
    #### Math ----
    Q6_2 = Q6_1 + round(rnorm(nSTEM, 0, 1)),
    Q6_3 = Q6_1 + round(rnorm(nSTEM, 0, 1.5)), # Negative worded
    Q6_4 = Q6_1 + round(rnorm(nSTEM, 0, 1.4)),
    Q6_5 = Q6_1 + round(rnorm(nSTEM, 0, 1.3)), # Negative worded
    Q6_6 = Q6_1 + round(rnorm(nSTEM, 0, 1.2)),
    Q6_7 = Q6_1 + round(rnorm(nSTEM, 0, 1.1)),
    Q6_8 = Q6_1 + round(rnorm(nSTEM, 0, 1))
  ) |>
  mutate(
    Q6_2 = ifelse(Q6_2 <= 1, 1, 
                  ifelse(Q6_2 >= 5, 5, Q6_2)),
    Q6_3 = ifelse(Q6_3 <= 1, 1, 
                  ifelse(Q6_3 >= 5, 5, Q6_3)), # Negative worded
    Q6_4 = ifelse(Q6_4 <= 1, 1, 
                  ifelse(Q6_4 >= 5, 5, Q6_4)),
    Q6_5 = ifelse(Q6_5 <= 1, 1, 
                  ifelse(Q6_5 >= 5, 5, Q6_5)), # Negative worded
    Q6_6 = ifelse(Q6_6 <= 1, 1, 
                  ifelse(Q6_6 >= 5, 5, Q6_6)),
    Q6_7 = ifelse(Q6_7 <= 1, 1, 
                  ifelse(Q6_7 >= 5, 5, Q6_7)),
    Q6_8 = ifelse(Q6_8 <= 1, 1, 
                  ifelse(Q6_8 >= 5, 5, Q6_8))
  ) |>
  mutate(
    #### Science ----
    Q23_2 = Q23_1 + round(rnorm(nSTEM, 0, 1)),
    Q23_3 = Q23_1 + round(rnorm(nSTEM, 0, 1.5)),
    Q23_4 = Q23_1 + round(rnorm(nSTEM, 0, 1.4)),
    Q23_5 = Q23_1 + round(rnorm(nSTEM, 0, 1.3)),
    Q23_6 = Q23_1 + round(rnorm(nSTEM, 0, 1.2)),
    Q23_7 = Q23_1 + round(rnorm(nSTEM, 0, 1.1)),
    Q23_8 = Q23_1 + round(rnorm(nSTEM, 0, 1.6)), # Negative worded
    Q23_9 = Q23_1 + round(rnorm(nSTEM, 0, 1))
  ) |>
  mutate(
    Q23_2 = ifelse(Q23_2 <= 1, 1, 
                  ifelse(Q23_2 >= 5, 5, Q23_2)),
    Q23_3 = ifelse(Q23_3 <= 1, 1, 
                  ifelse(Q23_3 >= 5, 5, Q23_3)), 
    Q23_4 = ifelse(Q23_4 <= 1, 1, 
                  ifelse(Q23_4 >= 5, 5, Q23_4)),
    Q23_5 = ifelse(Q23_5 <= 1, 1, 
                  ifelse(Q23_5 >= 5, 5, Q23_5)), 
    Q23_6 = ifelse(Q23_6 <= 1, 1, 
                  ifelse(Q23_6 >= 5, 5, Q23_6)),
    Q23_7 = ifelse(Q23_7 <= 1, 1, 
                  ifelse(Q23_7 >= 5, 5, Q23_7)),
    Q23_8 = ifelse(Q23_8 <= 1, 1, 
                  ifelse(Q23_8 >= 5, 5, Q23_8)), # Negative worded
    Q23_9 = ifelse(Q23_9 <= 1, 1, 
                  ifelse(Q23_9 >= 5, 5, Q23_9))
  ) |>
  mutate(
    #### Engineering and Tech ----
    Q24_2 = Q24_1 + round(rnorm(nSTEM, 0, 1)),
    Q24_3 = Q24_1 + round(rnorm(nSTEM, 0, 1.5)), 
    Q24_4 = Q24_1 + round(rnorm(nSTEM, 0, 1.4)),
    Q24_5 = Q24_1 + round(rnorm(nSTEM, 0, 1.3)), 
    Q24_6 = Q24_1 + round(rnorm(nSTEM, 0, 1.2)),
    Q24_7 = Q24_1 + round(rnorm(nSTEM, 0, 1.1)),
    Q24_8 = Q24_1 + round(rnorm(nSTEM, 0, 1.6)),
    Q24_9 = Q24_1 + round(rnorm(nSTEM, 0, 1))
  ) |>
  mutate(
    Q24_2 = ifelse(Q24_2 <= 1, 1, 
                  ifelse(Q24_2 >= 5, 5, Q24_2)),
    Q24_3 = ifelse(Q24_3 <= 1, 1, 
                  ifelse(Q24_3 >= 5, 5, Q24_3)), 
    Q24_4 = ifelse(Q24_4 <= 1, 1, 
                  ifelse(Q24_4 >= 5, 5, Q24_4)),
    Q24_5 = ifelse(Q24_5 <= 1, 1, 
                  ifelse(Q24_5 >= 5, 5, Q24_5)), 
    Q24_6 = ifelse(Q24_6 <= 1, 1, 
                  ifelse(Q24_6 >= 5, 5, Q24_6)),
    Q24_7 = ifelse(Q24_7 <= 1, 1, 
                  ifelse(Q24_7 >= 5, 5, Q24_7)),
    Q24_8 = ifelse(Q24_8 <= 1, 1, 
                  ifelse(Q24_8 >= 5, 5, Q24_8)),
    Q24_9 = ifelse(Q24_9 <= 1, 1, 
                  ifelse(Q24_9 >= 5, 5, Q24_9))
  )

# Check Cronbach Alpha to ensure data is similar to validation
CronbachAlpha(
  notionalSSTEM4 |> select(Q6_1, Q6_2, Q6_3, Q6_4, Q6_5, Q6_6, Q6_7, Q6_8),
  cond = TRUE
)

CronbachAlpha(
  notionalSSTEM4 |> select(Q23_1, Q23_2, Q23_3, Q23_4, Q23_5, Q23_6, Q23_7, Q23_8, Q23_9),
  cond = TRUE
)

CronbachAlpha(
  notionalSSTEM4 |> select(Q24_1, Q24_2, Q24_3, Q24_4, Q24_5, Q24_6, Q24_7, Q24_8, Q24_9),
  cond = TRUE
)

## Write out to excel ----
write_xlsx(notionalSSTEM4, "Data/Notional S-STEM Survey Data Generated.xlsx")







