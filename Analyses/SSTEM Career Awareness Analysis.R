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
library(agricolae)
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
# CAREER AWARENESS ANALYSIS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load SSTEM Survey Data ----
load("Data/Cleaned S-STEM Survey Data.RData")

# Filter Data ----
CareerAwareness <- SSTEMsurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_data), pattern = "Awareness")
  ) |>
  mutate(
    Awareness_Q1Bin = ifelse(Awareness_Q1 == 1, 1, 0),
    Awareness_Q2Bin = ifelse(Awareness_Q2 == 1, 1, 0),
    Awareness_Q3Bin = ifelse(Awareness_Q3 == 1, 1, 0),
    Awareness_Q4Bin = ifelse(Awareness_Q4 == 1, 1, 0)
  )

hist(CareerAwareness$Awareness_Q1)
hist(CareerAwareness$Awareness_Q2)
hist(CareerAwareness$Awareness_Q3)
hist(CareerAwareness$Awareness_Q4)

hist(CareerAwareness$Awareness_Q1Bin)
hist(CareerAwareness$Awareness_Q2Bin)
hist(CareerAwareness$Awareness_Q3Bin)
hist(CareerAwareness$Awareness_Q4Bin)

## Check Cronbach alpha of all 4 Questions
alpha(CareerAwareness |> select(str_which(colnames(CareerAwareness), pattern = "Bin")),
      cumulative = FALSE, na.rm = TRUE)
### Cannot rollup

# Analyze =======================
## Scientist ===================
awarenessSci_df <- CareerAwareness |>
  filter(
    Gender %in% c("Male", "Female")
  ) |>
  mutate(
    Gender = droplevels(Gender)
  ) |>
  select(
    YearSemester,
    School,
    Grade,
    Gender,
    #Race,
    Race2,
    Awareness_Q1Bin
  )
# awarenessSci_M1 <- glm(Awareness_Q1Bin ~ 
#                          YearSemester
#                        + School
#                        + Grade 
#                        + Gender
#                        + Race2
#                        + YearSemester:School
#                        + YearSemester:Grade
#                        + YearSemester:Gender
#                        + YearSemester:Race2
#                        + School:Grade
#                        + School:Gender
#                        + School:Race2
#                        + Grade:Gender
#                        + Grade:Race2
#                        + Gender:Race2
#                        ,
#                        data = CareerAwareness,
#                        family = binomial(link = "logit")
# )

awarenessSci_M1 <- glm(Awareness_Q1Bin ~ . + .^2,
                       data = awarenessSci_df,
                       family = binomial(link = "logit")
)
summary(awarenessSci_M1)

awarenessSci_M1_step <- step(awarenessSci_M1, direction = "both")

awarenessSci_M2 <- glm(awarenessSci_M1_step,
                       data = awarenessSci_df,
                       family = binomial(link = "logit")
)
summary(awarenessSci_M2)
Anova(awarenessSci_M2, test.statistic = "Wald")

### Predict ----
awarenessSci_M2_preds <- predict(awarenessSci_M2, type = "link")
awarenessSci_M2_preds

awarenessSci_M2_emmeans <- emmeans(awarenessSci_M2, specs = "Gender")
summary(awarenessSci_M2_emmeans)

awarenessSci_M2_emmeans_df <- data.frame(awarenessSci_M2_emmeans) |>
  select(-SE, -df) |>
  mutate(
    across(-Gender, ~invlogit(.x))
  )
awarenessSci_M2_emmeans_df <- awarenessSci_M2_emmeans_df |>
  mutate(
    Gender = factor(Gender, levels = rev(Gender))
  )

awarenessSci_plot <- ggplot(data = awarenessSci_M2_emmeans_df) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = Gender)) +
  geom_point(aes(x = emmean, y = Gender)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, 0.1)) +
  labs(
    title = "DeSIRE Students' Career Awareness of Science by Gender",
    subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    x = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )
awarenessSci_plot

## Engineer ===================
awarenessEng_df <- CareerAwareness |>
  filter(
    Gender %in% c("Male", "Female")
  ) |>
  mutate(
    Gender = droplevels(Gender)
  ) |>
  select(
    YearSemester,
    School,
    Grade,
    Gender,
    #Race,
    Race2,
    Awareness_Q2Bin
  )
awarenessEng_M1 <- glm(Awareness_Q2Bin ~
                         YearSemester
                       + School
                       + Grade
                       + Gender
                       + Race2
                       #+ YearSemester:School
                       #+ YearSemester:Grade
                       + YearSemester:Gender
                       #+ YearSemester:Race2
                       #+ School:Grade
                       + School:Gender
                       #+ School:Race2
                       + Grade:Gender
                       #+ Grade:Race2
                       #+ Gender:Race2
                       ,
                       data = awarenessEng_df,
                       family = binomial(link = "logit")
)
summary(awarenessEng_M1)
vif(awarenessEng_M1)
awarenessEng_M1_step <- step(awarenessEng_M1)

awarenessEng_M2 <- glm(awarenessEng_M1_step$call$formula,
                       data = awarenessEng_df,
                       family = binomial(link = "logit")
)
summary(awarenessEng_M2)
Anova(awarenessEng_M2, test.statistic = "Wald")

awarenessEng_M2 <- glm(Awareness_Q2Bin ~ School,
                       data = awarenessEng_df,
                       family = binomial(link = "logit")
)
summary(awarenessEng_M2)
Anova(awarenessEng_M2, test.statistic = "Wald")

### Predict ----
awarenessEng_M2_preds <- predict(awarenessEng_M2, type = "link")
awarenessEng_M2_preds

awarenessEng_M2_emmeans <- emmeans(awarenessEng_M2, specs = "School")
summary(awarenessEng_M2_emmeans)

awarenessEng_M2_emmeans_df <- data.frame(awarenessEng_M2_emmeans) |>
  select(-SE, -df) |>
  mutate(
    across(-School, ~invlogit(.x))
  )
awarenessEng_M2_emmeans_df <- awarenessEng_M2_emmeans_df |>
  mutate(
    School = factor(School, levels = rev(School))
  )

awarenessEng_plot <- ggplot(data = awarenessEng_M2_emmeans_df) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = School)) +
  geom_point(aes(x = emmean, y = School)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, 0.1)) +
  labs(
    title = "DeSIRE Students' Career Awareness of Engineering by School",
    subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    x = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )
awarenessEng_plot


## Mathentist ===================
awarenessMath_df <- CareerAwareness |>
  filter(
    Gender %in% c("Male", "Female")
  ) |>
  mutate(
    Gender = droplevels(Gender)
  ) |>
  select(
    YearSemester,
    School,
    Grade,
    Gender,
    #Race,
    Race2,
    Awareness_Q3Bin
  )
awarenessMath_M1 <- glm(Awareness_Q3Bin ~
                              YearSemester
                            + School
                            + Grade
                            + Gender
                            + Race2
                            #+ YearSemester:School
                            #+ YearSemester:Grade
                            + YearSemester:Gender
                            #+ YearSemester:Race2
                            #+ School:Grade
                            + School:Gender
                            #+ School:Race2
                            + Grade:Gender
                            #+ Grade:Race2
                            #+ Gender:Race2
                            ,
                       data = awarenessMath_df,
                       family = binomial(link = "logit")
)
summary(awarenessMath_M1)

awarenessMath_M1_step <- step(awarenessMath_M1, direction = "both")

awarenessMath_M2 <- glm(Awareness_Q3Bin ~ #1
                          #YearSemester
                        + School
                        #+ Grade
                        + Gender
                        #+ Race2
                        #+ School:Gender 
                        ,
                       data = awarenessMath_df,
                       family = binomial(link = "logit")
)
summary(awarenessMath_M2)
Anova(awarenessMath_M2, test.statistic = "Wald")
drop1(awarenessMath_M2)

### Predict ----
awarenessMath_M2_emmeans_Gender <- lsmeans(awarenessMath_M2, specs = "Gender")
summary(awarenessMath_M2_emmeans_Gender)

awarenessMath_M2_emmeans_df_Gender <- data.frame(awarenessMath_M2_emmeans_Gender) |>
  select(-SE, -df) |>
  mutate(
    across(-Gender, ~invlogit(.x))
  )
awarenessMath_M2_emmeans_df_Gender
awarenessMath_M2_emmeans_df_Gender <- awarenessMath_M2_emmeans_df_Gender |>
  mutate(
    Gender = factor(Gender, levels = rev(Gender))
  )

awarenessMath_plot <- ggplot(data = awarenessMath_M2_emmeans_df_Gender) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = Gender)) +
  geom_point(aes(x = emmean, y = Gender)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, 0.1)) +
  labs(
    title = "DeSIRE Students' Career Awareness of Mathence by Gender",
    subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    x = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )
awarenessMath_plot





## Tech ===================









