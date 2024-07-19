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
load("Data/S-STEM/Cleaned S-STEM Survey Data.RData")

# Filter Data ----
CareerAwareness <- SSTEMsurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_data), pattern = "Awareness")
  ) |>
  mutate(
    Awareness_Q1Bin = ifelse(Awareness_Q1 == 1, 1, 0),
    Awareness_Q2Bin = ifelse(Awareness_Q2 == 1, 1, 0),
    Awareness_Q3Bin = ifelse(Awareness_Q3 == 1, 1, 0),
    Awareness_Q4Bin = ifelse(Awareness_Q4 == 1, 1, 0),
    Awareness_Q1Bin2 = factor(ifelse(Awareness_Q1 == 1, "Yes", "No")),
    Awareness_Q2Bin2 = factor(ifelse(Awareness_Q2 == 1, "Yes", "No")),
    Awareness_Q3Bin2 = factor(ifelse(Awareness_Q3 == 1, "Yes", "No")),
    Awareness_Q4Bin2 = factor(ifelse(Awareness_Q4 == 1, "Yes", "No"))
  )

hist(CareerAwareness$Awareness_Q1)
hist(CareerAwareness$Awareness_Q2)
hist(CareerAwareness$Awareness_Q3)
hist(CareerAwareness$Awareness_Q4)

hist(CareerAwareness$Awareness_Q1Bin)
hist(CareerAwareness$Awareness_Q2Bin)
hist(CareerAwareness$Awareness_Q3Bin)
hist(CareerAwareness$Awareness_Q4Bin)

CareerAwarenessB <- CareerAwareness |>
  filter(
    complete.cases(Gender2)
  )

## Check Cronbach alpha of all 4 Questions
alpha(CareerAwareness |> select(str_which(colnames(CareerAwareness), pattern = "Bin")),
      cumulative = FALSE, na.rm = TRUE)
### Cannot rollup

# Analyze =======================
## Scientist ===================
# awarenessSci_df_G3 <- CareerAwareness |>
#   # filter(
#   #   Gender %in% c("Male", "Female")
#   # ) |>
#   # mutate(
#   #   Gender = droplevels(Gender)
#   # ) |>
#   select(
#     YearSemester,
#     School,
#     Grade,
#     Gender,
#     #Gender2,
#     #Race,
#     Race2,
#     Awareness_Q1Bin
#   )
# 
# awarenessSci_df_G2 <- CareerAwareness |>
#   # filter(
#   #   Gender %in% c("Male", "Female")
#   # ) |>
#   # mutate(
#   #   Gender = droplevels(Gender)
#   # ) |>
#   select(
#     YearSemester,
#     School,
#     Grade,
#     #Gender,
#     Gender2,
#     #Race,
#     Race2,
#     Awareness_Q1Bin
#   )

### Three level gender ----
awarenessSci_M1 <- glm(Awareness_Q1Bin2 ~
                         School
                       + Grade
                       + Gender
                       + Race2
                       + Gender:Race2
                       #+ (1|YearSemester)
                       ,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)

summary(awarenessSci_M1)
Anova(awarenessSci_M1, test.statistic = "F")

awarenessSci_M1_step <- step(awarenessSci_M1, direction = "both")

awarenessSci_M2 <- glm(awarenessSci_M1_step,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
summary(awarenessSci_M2)
Anova(awarenessSci_M2, test.statistic = "Wald")

#### Random YearSemester ----
awarenessSci_M1R <- glmer(Awareness_Q1Bin2 ~
                         School
                       + Grade
                       + Gender
                       + Race2
                       + Gender:Race2
                       + (1|YearSemester)
                       ,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)

summary(awarenessSci_M1R)
Anova(awarenessSci_M1R, test.statistic = "Chisq")

awarenessSci_M1_step <- step(awarenessSci_M1R, direction = "both")

awarenessSci_M2 <- glm(awarenessSci_M1_step,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
summary(awarenessSci_M2)
Anova(awarenessSci_M2, test.statistic = "Wald")

### Two level gender ----
awarenessSci_M1B <- glm(Awareness_Q1Bin2 ~
                        School
                      + Grade
                      + Gender2
                      + Race2
                      + Gender2:Race2
                      #+ (1|YearSemester)
                      ,
                      data = CareerAwarenessB,
                      family = binomial(link = "logit")
)

summary(awarenessSci_M1B)
Anova(awarenessSci_M1B, test.statistic = "Wald")
Anova(awarenessSci_M1B, test.statistic = "LR")
Anova(awarenessSci_M1B, test.statistic = "F")
vif(awarenessSci_M1B)

awarenessSci_M1_stepB <- step(awarenessSci_M1B, direction = "both")

awarenessSci_M2B <- glm(awarenessSci_M1_stepB,
                       data = CareerAwarenessB,
                       family = binomial(link = "logit")
)
summary(awarenessSci_M2B)
Anova(awarenessSci_M2B, test.statistic = "Wald")

#### Random YearSemester ----
awarenessSci_M1BR <- glmer(Awareness_Q1Bin2 ~
                          School
                        + Grade
                        + Gender2
                        + Race2
                        + Gender2:Race2
                        + (1|YearSemester)
                        ,
                        data = CareerAwarenessB,
                        family = binomial(link = "logit")
)

summary(awarenessSci_M1BR)
Anova(awarenessSci_M1BR, test.statistic = "Chisq")

anova(awarenessSci_M1B, awarenessSci_M1BR)

awarenessSci_M1_stepB <- step(awarenessSci_M1B, direction = "both")

awarenessSci_M2B <- glm(awarenessSci_M1_stepB,
                        data = CareerAwarenessB,
                        family = binomial(link = "logit")
)
summary(awarenessSci_M2B)
Anova(awarenessSci_M2B, test.statistic = "Wald")

awarenessSci_FinalModel <- awarenessSci_M2B

### Predict ----
awarenessSci_preds <- predict(awarenessSci_FinalModel, type = "response")
awarenessSci_preds

awarenessSci_emmeans <- emmeans(awarenessSci_FinalModel, specs = "Gender2")
awarenessSci_emmeans_df <- summary(awarenessSci_emmeans, type = "response")
colnames(awarenessSci_emmeans_df)[1] <- "Gender"

awarenessSci_emmeans_df2 <- data.frame(awarenessSci_emmeans) |>
  select(-SE, -df) |>
  mutate(
    across(-Gender2, ~invlogit(.x))
  )
# awarenessSci_emmeans_df <- awarenessSci_emmeans_df |>
#   mutate(
#     Gender2 = factor(Gender, levels = rev(Gender))
#   )

awarenessSci_plot <- ggplot(data = awarenessSci_emmeans_df) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = Gender)) +
  geom_point(aes(x = prob, y = Gender)) +
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


awarenessSci_plot2 <- ggplot(data = awarenessSci_emmeans_df) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Gender)) +
  geom_point(aes(y = prob, x = Gender)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, 0.1)) +
  labs(
    title = "DeSIRE Students' Career Awareness of Science by Gender",
    subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    y = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )
awarenessSci_plot2

## Engineer ===================
### Three level gender ----
awarenessEng_M1 <- glm(Awareness_Q2Bin2 ~
                         School
                       + Grade
                       + Gender
                       + Race2
                       + Gender:Race2
                       #+ (1|YearSemester)
                       ,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)

summary(awarenessEng_M1)
Anova(awarenessEng_M1, test.statistic = "F")

awarenessEng_M1_step <- step(awarenessEng_M1, direction = "both")

awarenessEng_M2 <- glm(awarenessEng_M1_step,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
summary(awarenessEng_M2)
Anova(awarenessEng_M2, test.statistic = "Wald")

#### Random YearSemester ----
awarenessEng_M1R <- glmer(Awareness_Q2Bin2 ~
                            School
                          + Grade
                          + Gender
                          + Race2
                          + Gender:Race2
                          + (1|YearSemester)
                          ,
                          data = CareerAwareness,
                          family = binomial(link = "logit")
)

summary(awarenessEng_M1R)

awarenessEng_M1_step <- step(awarenessEng_M1R, direction = "both")

awarenessEng_M2 <- glm(awarenessEng_M1_step,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
summary(awarenessEng_M2)
Anova(awarenessEng_M2, test.statistic = "Wald")

### Two level gender ----
awarenessEng_M1B <- glm(Awareness_Q2Bin2 ~
                          School
                        + Grade
                        + Gender2
                        + Race2
                        + Gender2:Race2
                        #+ (1|YearSemester)
                        ,
                        data = CareerAwarenessB,
                        family = binomial(link = "logit")
)

summary(awarenessEng_M1B)
Anova(awarenessEng_M1B, test.statistic = "Wald")
Anova(awarenessEng_M1B, test.statistic = "LR")
Anova(awarenessEng_M1B, test.statistic = "F")
vif(awarenessEng_M1B)

awarenessEng_M1_stepB <- step(awarenessEng_M1B, direction = "both")

awarenessEng_M2B <- glm(Awareness_Q2Bin2 ~
                            School
                          + Grade
                          + Gender2
                          + Race2
                          #+ Gender2:Race2
                          #+ (1|YearSemester)
                          ,
                          data = CareerAwarenessB,
                          family = binomial(link = "logit")
)
summary(awarenessEng_M2B)
Anova(awarenessEng_M2B, test.statistic = "Wald")
Anova(awarenessEng_M2B, test.statistic = "LR")
Anova(awarenessEng_M2B, test.statistic = "F")

awarenessEng_M2_stepB <- step(awarenessEng_M2B, direction = "both")

awarenessEng_M3B <- glm(Awareness_Q2Bin2 ~
                          School
                        + Grade
                        #+ Gender2
                        #+ Race2
                        #+ Gender2:Race2
                        #+ (1|YearSemester)
                        ,
                        data = CareerAwarenessB,
                        family = binomial(link = "logit")
)
summary(awarenessEng_M3B)
Anova(awarenessEng_M3B, test.statistic = "Wald")
Anova(awarenessEng_M3B, test.statistic = "LR")
Anova(awarenessEng_M3B, test.statistic = "F")

#### Random YearSemester ----
awarenessEng_M1BR <- glmer(Awareness_Q1Bin2 ~ 
                             School
                           + Grade
                           + Gender2
                           + Race2
                           + Gender2:Race2
                           + (1|YearSemester)
                           ,
                           data = CareerAwarenessB,
                           family = binomial(link = "logit")
)
print(awarenessEng_M1BR)
summary(awarenessEng_M1BR)
Anova(awarenessEng_M1BR, test.statistic = "Chisq")

anova(awarenessEng_M1B, awarenessEng_M1BR)

awarenessEng_M1_stepB <- step(awarenessEng_M1B, direction = "both")

awarenessEng_M2B <- glm(awarenessEng_M1_stepB,
                        data = CareerAwarenessB,
                        family = binomial(link = "logit")
)
summary(awarenessEng_M2B)
Anova(awarenessEng_M2B, test.statistic = "Wald")

awarenessEng_FinalModel <- awarenessEng_M2B

awarenessEng_M1BR <- stan_glmer(Awareness_Q1Bin2 ~ 
                             School
                           + Grade
                           + Gender2
                           + Race2
                           + Gender2:Race2
                           + (1|YearSemester)
                           ,
                           data = CareerAwarenessB,
                           family = binomial(link = "logit")
)

summary(awarenessEng_M1BR, probs = c(0.025, 0.975))

### Predict ----
awarenessEng_preds <- predict(awarenessEng_FinalModel, type = "response")
awarenessEng_preds

awarenessEng_emmeans <- emmeans(awarenessEng_FinalModel, specs = "Gender2")
awarenessEng_emmeans_df <- summary(awarenessEng_emmeans, type = "response")
colnames(awarenessEng_emmeans_df)[1] <- "Gender"

awarenessEng_emmeans_df2 <- data.frame(awarenessEng_emmeans) |>
  select(-SE, -df) |>
  mutate(
    across(-Gender2, ~invlogit(.x))
  )
# awarenessEng_emmeans_df <- awarenessEng_emmeans_df |>
#   mutate(
#     Gender2 = factor(Gender, levels = rev(Gender))
#   )

awarenessEng_plot <- ggplot(data = awarenessEng_emmeans_df) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = Gender)) +
  geom_point(aes(x = prob, y = Gender)) +
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
awarenessEng_plot


awarenessEng_plot2 <- ggplot(data = awarenessEng_emmeans_df) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Gender)) +
  geom_point(aes(y = prob, x = Gender)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, 0.1)) +
  labs(
    title = "DeSIRE Students' Career Awareness of Science by Gender",
    subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    y = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )
awarenessEng_plot2

### OLD ====
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









