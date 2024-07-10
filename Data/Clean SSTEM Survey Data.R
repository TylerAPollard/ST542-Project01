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
library(data.table) # Read csv in tibble format
library(readxl) # Read xlsx 

## Data Manipulation
library(stringr) # Manipulate strings
library(plyr) # Produce summary tables/data.frames

## Data Analysis
library(likert)
library(lme4)
library(lmtest)
library(car)
library(emmeans)
library(betareg)
library(caret)

## Bayesian Data Analysis
library(DescTools)
library(rstanarm)
library(brms)
library(posterior)
library(bayesplot)
library(BayesFactor)

## Plotting
library(patchwork)

## Load this package last to reduce package conflictions with dplyr
library(tidyverse) 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S-STEM SURVEY =============================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Read in Data ----
### Questions ----
SSTEMsurvey_questions <- read_excel("Data/S-STEM+-+DeSIRE_June+6,+2024_09.13.xlsx", 
                                    n_max = 1)
SSTEMsurvey_questions <- SSTEMsurvey_questions |> unlist()
view(SSTEMsurvey_questions)
SSTEMsurvey_colnames <- names(SSTEMsurvey_questions)

### Survey Data ----
SSTEMsurvey_data_withQuestions <- read_excel("Data/S-STEM+-+DeSIRE_June+6,+2024_09.13.xlsx",
                                             na = c("", NA), 
                                             skip = 1)
SSTEMsurvey_data_withCodes <- SSTEMsurvey_data_withQuestions
colnames(SSTEMsurvey_data_withCodes) <- SSTEMsurvey_colnames

## Clean Data ----
### Select and Rename ----
SSTEMsurvey_data <- SSTEMsurvey_data_withCodes |>
  select(
    StartDate,
    Progress,
    Finished,
    str_which(SSTEMsurvey_colnames, pattern = "Q")
  ) |>
  #### Socio-Demographics ----
rename(
  "StartDate" = StartDate,
  "School" = Q2,
  "Grade" = Q3,
  "TeacherID" = Q37,
  "TeacherName" = Q37_4_TEXT,
  "SemesterYear" = Q36,
  "FirstName" = Q15,
  "MiddleName" = Q16,
  "LastName" = Q17,
  "BirthDate" = Q18,
  "StudentID" = Q20,
  "Gender" = Q21,
  "GenderOther" = Q21_6_TEXT,
  "Race" = Q22,
  "RaceOther" = Q22_8_TEXT
)

#### Self-Efficacy ----
## Math, Science, and Engineering and Tech Constructs
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q6_", replacement = "Math_Q")
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q23_", replacement = "Science_Q")
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q24_", replacement = "EngTech_Q")

#### Identity ----
## 21st Century Learning Construct
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q26_", replacement = "Learning_Q")

#### Interest ----
## Your Future Items
interestAreas <- c(
  "Physics",             # Q28_1
  "Environmental Work",   # Q28_2
  "Biology and Zoology",     # Q28_3
  "Veterinary Work",      # Q28_4
  "Mathematics",         # Q28_5
  "Medicine",            # Q28_6
  "Earth Science",        # Q28_7
  "Computer Science",     # Q28_8
  "Medical Science",      # Q28_9
  "Chemistry",           # Q28_10
  "Energy",              # Q28_11
  "Engineering"          # Q28_12
)

colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q28_", replacement = "Interest_Q")

#### Career Awareness ----
## More about you Items
careerAwarenessAreas <- c(
  "Scientists",
  "Engineers",
  "Mathematicians",
  "Technologists"
)

colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q35_", replacement = "Awareness_Q")

#### Other Questions ----
##### Expectations ----
expectationAreas <- c(
  "English",
  "Math",
  "Science"
)

colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q29_", replacement = "Expectations_Q")

##### Take Advanced Classes ----
advancedClassesAreas <- c(
  "Mathematics",
  "Science"
)

colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q31_", replacement = "Classes_Q")

##### College ----
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q32", replacement = "PlanCollege")
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q33", replacement = "InterestCollege")
colnames(SSTEMsurvey_data) <- str_replace(colnames(SSTEMsurvey_data), pattern = "Q34", replacement = "FirstCollege")

#### SAVE CHECKPOINT ----
save(SSTEMsurvey_data, file = "Data/S-STEM Survey Data Renamed.RData")

### Recode ----
load(file = "Data/S-STEM Survey Data Renamed.RData")

#### Date ----
SSTEMsurvey_data2 <- SSTEMsurvey_data |>
  mutate(
    Date = as_date(StartDate),
    Year = year(Date),
    Semester = ifelse(semester(Date) == 1, "Spring", "Fall"),
    SchoolYear = ifelse(Semester == "Fall", 
                        paste0(Year, "-", Year+1),
                        paste0(Year-1, "-", Year)),
    YearSemester = paste0(Year, Semester)
  ) |>
  select(
    StartDate,
    Date,
    Year,
    SchoolYear,
    Semester,
    YearSemester,
    everything()
  )

#### Factors of Interest ----
SSTEMsurvey_data3 <- SSTEMsurvey_data2 |>
  mutate(
    ##### School ----
    School = factor(School,
                    labels = c("West Edgecombe Middle School", "Phillips Middle School")),
    ##### Grade ----
    Grade = factor(Grade,
                   labels = c("6th", "7th", "8th")),
    ##### Gender ----
    Gender = factor(Gender,
                    labels = c("Male", "Female", "Other")),
    ##### Race ----
    Race = factor(Race,
                  levels = c(1,2,3,4,5,6,7,8),
                  labels = c("American Indian_Alaska Native",
                             "Asian",
                             "Black_African American",
                             "Native Hawaiian_Other Pacific Islander",
                             "White_Caucasian",
                             "Hispanic_Latino",
                             "Multiracial",
                             "Other")),
    ##### Race2 ----
    # To stay in accordance with Human Subject guidance
    Race2 = factor(Race,
                   levels = c("American Indian_Alaska Native",
                              "Asian",
                              "Black_African American",
                              "Native Hawaiian_Other Pacific Islander",
                              "White_Caucasian",
                              "Hispanic_Latino",
                              "Multiracial",
                              "Other"),
                   labels = c("Other",
                              "Other",
                              "Black_African American",
                              "Other",
                              "White_Caucasian",
                              "Hispanic_Latino",
                              "Other",
                              "Other")) 
  ) |>
  mutate(
    YearSemester = factor(YearSemester, levels = unique(YearSemester))
  ) |>
  select(
    1:21,
    Race2,
    everything()
  )

SSTEMsurvey_data <- SSTEMsurvey_data3

### SAVE DATA ----
save(SSTEMsurvey_data,
     SSTEMsurvey_colnames,
     SSTEMsurvey_questions,
     file = "Data/Cleaned S-STEM Survey Data.RData")


## Sample Sizes of Data before Filtering ----
### Frequency ----
table(SSTEMsurvey_data$SchoolYear)
table(SSTEMsurvey_data$Semester)
table(SSTEMsurvey_data$School)
table(SSTEMsurvey_data$Grade)
table(SSTEMsurvey_data$Gender)
table(SSTEMsurvey_data$Race)
table(SSTEMsurvey_data$Race2)

## Percent ----
round(table(SSTEMsurvey_data$SchoolYear)/nrow(SSTEMsurvey_data)*100,2)
round(table(SSTEMsurvey_data$Semester)/nrow(SSTEMsurvey_data)*100,2)
round(table(SSTEMsurvey_data$School)/nrow(SSTEMsurvey_data)*100,2)
round(table(SSTEMsurvey_data$Grade)/nrow(SSTEMsurvey_data)*100,2)
round(table(SSTEMsurvey_data$Gender)/nrow(SSTEMsurvey_data)*100,2)
round(table(SSTEMsurvey_data$Race)/nrow(SSTEMsurvey_data)*100,2)
round(table(SSTEMsurvey_data$Race2)/nrow(SSTEMsurvey_data)*100,2)

table(SSTEMsurvey_data$SchoolYear, SSTEMsurvey_data$Semester)
table(SSTEMsurvey_data$School, SSTEMsurvey_data$Grade)

