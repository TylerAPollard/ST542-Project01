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
# library(survey)
# library(srvyr)
# library(surveydata)
library(likert)
library(lme4)
library(car)
library(emmeans)

## Bayesian Data Analysis
library(DescTools)
library(rstanarm)
library(posterior)
library(bayesplot)
library(BayesFactor)

## Load this package last to reduce package conflictions with dplyr
library(tidyverse) 


# Read in data =============================================================================================
## Survey Data Question Mapping ----
list.files("Data")
emptySSTEM <- read_excel("Data/S-STEM+-+DeSIRE_June+6,+2024_09.13_blank.xlsx")
SSTEMquestions <- unlist(emptySSTEM[1,])
SSTEMquestions

STEMcolnames <- colnames(emptySSTEM)

## S-STEM Survey Data ----
SSTEMsurvey_data <- read_excel("Data/Notional S-STEM Survey Data Generated.xlsx")

## Clean data ----
##  School: West Edgecombe Middle School (WEMS), Phillips Middle School (PSM)
##  Grade: 6th, 7th, 8th
##  Teacher: Mr.O'Shea, Ms.Manuel, Mr.Suitter, Other
##  Year: 2021, 2022, 2023, 2024
##  Semester: Fall, Spring
##  Gender: Male, Female, Other
##  Race: American Indian/Alaska Native, Asian, Black/African American, Native Hawaiian/Other Pacific Islander
##        White/Caucasian, Hispanic/Latino, Multracial, Other
SSTEMsurvey <- SSTEMsurvey_data |>
  select(
    ResponseId,
    Q2,
    Q3,
    Q21,
    Q22,
    str_which(STEMcolnames, pattern = "Q6"),
    str_which(STEMcolnames, pattern = "Q23"),
    str_which(STEMcolnames, pattern = "Q24")
  ) |>
  rename(
    "School" = Q2,
    "Grade" = Q3,
    "Gender" = Q21,
    "Race" = Q22
  )

### Rename columns for clarity ----
# colnames(SSTEMsurvey) <- str_replace(colnames(SSTEMsurvey), pattern = "Q6", replacement = "Math")
# colnames(SSTEMsurvey) <- str_replace(colnames(SSTEMsurvey), pattern = "Q23", replacement = "Science")
# colnames(SSTEMsurvey) <- str_replace(colnames(SSTEMsurvey), pattern = "Q24", replacement = "EngTech")


### Summarize across constructs ----
SSTEMsurvey2 <- SSTEMsurvey |>
  rowwise() |>
  mutate(
    MathScore = mean(c_across(str_subset(colnames(SSTEMsurvey), pattern = "Q6"))),
    ScienceScore = mean(c_across(str_subset(colnames(SSTEMsurvey), pattern = "Q23"))),
    EngTechScore = mean(c_across(str_subset(colnames(SSTEMsurvey), pattern = "Q24")))
  ) |>
  mutate(
    School = factor(School, levels = c("WEMS", "PSM")),
    Grade = factor(Grade, levels = c("6th", "7th", "8th")),
    Gender = factor(Gender, levels = c("Male", "Female")),
    Race = factor(Race, 
                  levels = c("American Indian/Alaska Native",
                             "Asian",
                             "Black/African American",
                             #"Native Hawaiian/Other Pacific Islander",
                             "White/Caucasian",
                             "Hispanic/Latino"
                             #"Multracial",
                             #"Other")
                             )
    ),
    Race = fct_recode(Race, 
                      "American Indian_Alaska Native" = "American Indian/Alaska Native",
                      "Black_African American" = "Black/African American",
                      "White_Caucasian" = "White/Caucasian",
                      "Hispanic_Latino" = "Hispanic/Latino")
  )

# Analysis =================================================================================================
SSTEMsurvey3 <- SSTEMsurvey2 |>
  mutate(
    across(which(str_detect(colnames(SSTEMsurvey), pattern = "Q")), 
           ~ factor(.x, levels = c(1,2,3,4,5)))
  )

## Quantitative ----
### Summary Statistics ----
table(SSTEMsurvey3$School)
table(SSTEMsurvey3$Grade)
table(SSTEMsurvey3$Gender)
table(SSTEMsurvey3$Race)



### Self-efficacy ----
#### Math ----
##### Survey Response Distributions ----
SSTEM_Math_df <- data.frame(SSTEMsurvey3 |> select(str_subset(colnames(SSTEMsurvey), pattern = "Q6")))
SSTEM_Math_labels <- SSTEMquestions[colnames(SSTEM_Math_df)]
SSTEM_Math_labels <- str_replace(SSTEM_Math_labels, pattern = "Math - ", replacement = "")
colnames(SSTEM_Math_df) <- SSTEM_Math_labels

###### No Grouping ----
SSTEMlikertMath <- likert(SSTEM_Math_df)
plot(SSTEMlikertMath) +
  labs(title = "Math items")

###### Grouping ----
mathGrouping <- SSTEMsurvey3$School
SSTEMlikertMath <- likert(SSTEM_Math_df, grouping = mathGrouping)
plot(SSTEMlikertMath) +
  labs(title = "Math items")



##### Fit Multiple Linear Regression Model ----
mlm1 <- lm(data = SSTEMsurvey2,
           MathScore ~ 
             School 
           + Grade 
           + Gender
           + Race 
           + School:Grade 
           + School:Gender 
           + School:Race 
           + Grade:Gender
           #+ Grade:Race
           #+ Gender:Race
)
summary(mlm1)
Anova(mlm1, type = 2)

mlm1_step <- step(mlm1)

###### Refit with only significant terms ----
mlm1b <- lm(data = SSTEMsurvey2,
            mlm1_step$call$formula)
summary(mlm1b)
Anova(mlm1b, type = 2)
anova(mlm1b)

## Diagnostic checks
plot(mlm1b)

###### Expected Marginal Means -----
mlm1b_emms <- data.frame(emmeans(mlm1b, specs = c("School"), by = c("Grade"), level = 0.95))

###### Create visualization ----
ggplot(data = mlm1b_emms) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL, y = Grade, color = School),
                 position = position_dodge(width = 1)) + 
  geom_point(aes(x = emmean, y = Grade, color = School),
             position = position_dodge(width = 1)) +
  scale_x_continuous(limits = c(1,6), breaks = 1:6) +
  labs(
    title = "S-STEM Self-Efficacy Math Construct Score by School and Grade",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    x = "Math Construct Score"
  ) +
  theme_bw()








