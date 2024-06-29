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
library(psych)
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
# IDENTITY ANALYSIS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load SSTEM Survey Data ----
load("Data/Cleaned S-STEM Survey Data.RData")

# LEARNING CONSTRUCT ====================================================================================
## Filter Data ----
Learningsurvey <- SSTEMsurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_data), pattern = "Learning")
  ) 

## Calculate Aggregate Score ----
Learningsurvey_data <- Learningsurvey |>
  rowwise() |>
  mutate(
    LearningScore = mean(c_across(str_which(colnames(Learningsurvey), pattern = "Learning")), na.rm = TRUE)
  ) |>
  filter(
    complete.cases(LearningScore) 
  ) |>
  mutate(
    LearningScoreScaled = (LearningScore - 1)/4
  ) |>
  mutate(
    LearningScoreScaled2 = ifelse(LearningScoreScaled == 0, 0.00001, 
                                  ifelse(LearningScoreScaled == 1, 0.99999, LearningScoreScaled))
  )
hist((Learningsurvey_data$LearningScore))
plot(density(Learningsurvey_data$LearningScore))
hist((Learningsurvey_data$LearningScoreScaled2))
plot(density(Learningsurvey_data$LearningScoreScaled2))

### Data without Other Gender ----
Learningsurvey_data2 <- Learningsurvey_data |>
  filter(Gender != "Other") |>
  mutate(Gender = droplevels(Gender))

## Cronbach Alpha ----
alpha(Learningsurvey_data |> select(str_which(colnames(Learningsurvey_data), pattern = "Learning_")),
      cumulative = TRUE)

alpha(Learningsurvey_data2 |> select(str_which(colnames(Learningsurvey_data2), pattern = "Learning_")),
      cumulative = TRUE)

## Sample Sizes ----
### By Each Factor ----
table(Learningsurvey_data$YearSemester)
table(Learningsurvey_data$School)
table(Learningsurvey_data$Grade)
table(Learningsurvey_data$Gender)
table(Learningsurvey_data$Race2)

table(Learningsurvey_data2$YearSemester)
table(Learningsurvey_data2$School)
table(Learningsurvey_data2$Grade)
table(Learningsurvey_data2$Gender)
table(Learningsurvey_data2$Race2)

### Interactions ----
#### Full data ----
Learning_SampleSizes_YearSemesterSchool <- ddply(Learningsurvey_data, .(YearSemester, School), summarise, .drop = FALSE,
                                                 n = n())
Learning_SampleSizes_YearSemesterGrade <- ddply(Learningsurvey_data, .(YearSemester, Grade), summarise, .drop = FALSE,
                                                n = n())
Learning_SampleSizes_YearSemesterGender <- ddply(Learningsurvey_data, .(YearSemester, Gender), summarise, .drop = FALSE,
                                                 n = n())
Learning_SampleSizes_YearSemesterRace <- ddply(Learningsurvey_data, .(YearSemester, Race2), summarise, .drop = FALSE,
                                               n = n())
Learning_SampleSizes_SchoolGrade <- ddply(Learningsurvey_data, .(School, Grade), summarise, .drop = FALSE,
                                          n = n())
Learning_SampleSizes_SchoolGender <- ddply(Learningsurvey_data, .(School, Gender), summarise, .drop = FALSE,
                                           n = n())
Learning_SampleSizes_SchoolRace <- ddply(Learningsurvey_data, .(School, Race2), summarise, .drop = FALSE,
                                         n = n())
Learning_SampleSizes_GradeGender <- ddply(Learningsurvey_data, .(Grade, Gender), summarise, .drop = FALSE,
                                          n = n())
Learning_SampleSizes_GradeRace <- ddply(Learningsurvey_data, .(Grade, Race2), summarise, .drop = FALSE,
                                        n = n())
Learning_SampleSizes_GenderRace <- ddply(Learningsurvey_data, .(Gender, Race2), summarise, .drop = FALSE,
                                         n = n())

Learning_SampleSizes_YearSemesterSchool
Learning_SampleSizes_YearSemesterGrade
Learning_SampleSizes_YearSemesterGender
Learning_SampleSizes_YearSemesterRace
Learning_SampleSizes_SchoolGrade
Learning_SampleSizes_SchoolGender
Learning_SampleSizes_SchoolRace
Learning_SampleSizes_GradeGender
Learning_SampleSizes_GradeRace
Learning_SampleSizes_GenderRace

demographics <- c("YearSemester", "School", "Grade", "Gender", "Race")
interactions <- data.frame(t(combn(demographics, 2))) 
colnames(interactions) <- c("Var1", "Var2")
interactions$Missing <- NA

for(i in 1:nrow(interactions)){
  var1 <- interactions$Var1[i]
  var2 <- interactions$Var2[i]
  dat <- paste0("Learning_SampleSizes_", var1, var2)
  
  temp_data <- get(dat)
  temp_miss <- sum(temp_data$n == 0)
  
  interactions$Missing[i] <- temp_miss
}

#### Reduced Gender ----
Learning_SampleSizes_YearSemesterSchool2 <- ddply(Learningsurvey_data2, .(YearSemester, School), summarise, .drop = FALSE,
                                                  n = n())
Learning_SampleSizes_YearSemesterGrade2 <- ddply(Learningsurvey_data2, .(YearSemester, Grade), summarise, .drop = FALSE,
                                                 n = n())
Learning_SampleSizes_YearSemesterGender2 <- ddply(Learningsurvey_data2, .(YearSemester, Gender), summarise, .drop = FALSE,
                                                  n = n())
Learning_SampleSizes_YearSemesterRace2 <- ddply(Learningsurvey_data2, .(YearSemester, Race2), summarise, .drop = FALSE,
                                                n = n())
Learning_SampleSizes_SchoolGrade2 <- ddply(Learningsurvey_data2, .(School, Grade), summarise, .drop = FALSE,
                                           n = n())
Learning_SampleSizes_SchoolGender2 <- ddply(Learningsurvey_data2, .(School, Gender), summarise, .drop = FALSE,
                                            n = n())
Learning_SampleSizes_SchoolRace2 <- ddply(Learningsurvey_data2, .(School, Race2), summarise, .drop = FALSE,
                                          n = n())
Learning_SampleSizes_GradeGender2 <- ddply(Learningsurvey_data2, .(Grade, Gender), summarise, .drop = FALSE,
                                           n = n())
Learning_SampleSizes_GradeRace2 <- ddply(Learningsurvey_data2, .(Grade, Race2), summarise, .drop = FALSE,
                                         n = n())
Learning_SampleSizes_GenderRace2 <- ddply(Learningsurvey_data2, .(Gender, Race2), summarise, .drop = FALSE,
                                          n = n())

Learning_SampleSizes_YearSemesterSchool2
Learning_SampleSizes_YearSemesterGrade2
Learning_SampleSizes_YearSemesterGender2
Learning_SampleSizes_YearSemesterRace2
Learning_SampleSizes_SchoolGrade2
Learning_SampleSizes_SchoolGender2
Learning_SampleSizes_SchoolRace2
Learning_SampleSizes_GradeGender2
Learning_SampleSizes_GradeRace2
Learning_SampleSizes_GenderRace2

demographics2 <- c("YearSemester", "School", "Grade", "Gender", "Race")
interactions2 <- data.frame(t(combn(demographics2, 2))) 
colnames(interactions2) <- c("Var1", "Var2")
interactions2$Missing <- NA

for(i in 1:nrow(interactions2)){
  var1 <- interactions2$Var1[i]
  var2 <- interactions2$Var2[i]
  dat <- paste0("Learning_SampleSizes_", var1, var2, "2")
  
  temp_data <- get(dat)
  temp_miss <- sum(temp_data$n == 0)
  
  interactions2$Missing[i] <- temp_miss
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## FIT MODELS ================
### Full Data ================
Learningsurvey_df <- Learningsurvey_data |>
  select(
    #Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    #Race,
    Race2,
    LearningScore
  ) |>
  mutate(
    YearSemester = factor(YearSemester, levels = unique(Learningsurvey_data$YearSemester))
  )
Learningsurvey_df2 <- Learningsurvey_data2 |>
  select(
    #Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    #Race,
    Race2,
    LearningScore
  )

boxcox_model <- lm(LearningScore ~ 1, data = Learningsurvey_df)
boxcoxLearningScore <- boxCox(boxcox_model)
boxcoxLearningScore_lambda <- boxcoxLearningScore$x[which.max(boxcoxLearningScore$y)]

#### Normal ----
generalBF1 <- generalTestBF(LearningScore ~ . + .^2,
                            data = Learningsurvey_df)
sort(generalBF1)



generalBF2 <- anovaBF(LearningScore ~ 
                              YearSemester
                            + School
                            + Grade
                            + Gender
                            + Race2
                            + School:Grade
                            + School:Gender
                            + School:Race2
                            + Grade:Gender
                            + Grade:Race2
                            + Gender:Race2,
                            whichRandom = "YearSemester",
                            data = Learningsurvey_df)
generalBF2
sort(generalBF2)


linM1 <- brm(LearningScore ~ . + .^2,
             data = Learningsurvey_df,
             family = gaussian(),
             seed = 52)

linM2 <- stan_glm(LearningScore ~ 
                    YearSemester
                  + School
                  + Grade
                  + Gender
                  + Race2
                  + School:Grade
                  + School:Gender
                  + School:Race2
                  + Grade:Gender
                  + Grade:Race2
                  + Gender:Race2
                  ,
                 data = Learningsurvey_df,
                 family = gaussian()
                 #seed = 52
                 )
print(linM2)
summary(linM2)

linM3 <- stan_glmer(LearningScore ~ 
                    (1|YearSemester)
                  + School
                  #+ Grade
                  + Gender
                  #+ Race2
                  #+ School:Grade
                  #+ School:Gender
                  #+ School:Race2
                  #+ Grade:Gender
                  #+ Grade:Race2
                  #+ Gender:Race2
                  ,
                  data = Learningsurvey_df
                  #family = gaussian()
                  #seed = 52
)
linM3
summary(linM3)
plot(linM3)





