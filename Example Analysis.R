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
library(rjags)

## Plotting
library(patchwork)

## Load this package last to reduce package conflictions with dplyr
library(tidyverse) 


# S-STEM SURVEY =============================================================================================
## Read in Data ----
### Survey Data Question Mapping ----
list.files("Data")
emptySSTEM <- read_excel("Data/S-STEM+-+DeSIRE_June+6,+2024_09.13_blank.xlsx")
SSTEMquestions <- unlist(emptySSTEM[1,])
SSTEMquestions

STEMcolnames <- colnames(emptySSTEM)

### Survey Data ----
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
                             "Native Hawaiian/Other Pacific Islander",
                             "White/Caucasian",
                             "Hispanic/Latino",
                             "Multracial",
                             "Other"
                  )
    )
  ) |>
  mutate(
    Race = droplevels(Race)
  )

levels(SSTEMsurvey2$Race)

# Change level names so they don't include "/" becuase it messes with regression
SSTEMsurvey2$Race = fct_recode(SSTEMsurvey2$Race,
                               "American Indian_Alaska Native" = "American Indian/Alaska Native",
                               "Black_African American" = "Black/African American",
                               "White_Caucasian" = "White/Caucasian",
                               "Hispanic_Latino" = "Hispanic/Latino")
levels(SSTEMsurvey2$Race)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RESEARCH QUESTION 1 =====================================================================================

# RESEARCH QUESTION 2 =====================================================================================
## S-STEM Sample Sizes ----
### By Each Factor ----
table(SSTEMsurvey2$School)
table(SSTEMsurvey2$Grade)
table(SSTEMsurvey2$Gender)
table(SSTEMsurvey2$Race)

### All combinations ----
SSTEM_SampleSizes_All <- ddply(SSTEMsurvey2, .(School, Grade, Gender, Race), summarize,
                               n = n())
SSTEM_SampleSizes_All

### Interactions ----
SSTEM_SampleSizes_SchoolGrade <- ddply(SSTEMsurvey2, .(School, Grade), summarize,
                                       n = n())
SSTEM_SampleSizes_SchoolGender <- ddply(SSTEMsurvey2, .(School, Gender), summarize,
                                        n = n())
SSTEM_SampleSizes_SchoolRace <- ddply(SSTEMsurvey2, .(School, Race), summarize,
                                      n = n())
SSTEM_SampleSizes_GradeGender <- ddply(SSTEMsurvey2, .(Grade, Gender), summarize,
                                       n = n())
SSTEM_SampleSizes_GradeRace <- ddply(SSTEMsurvey2, .(Grade, Race), summarize,
                                     n = n())
SSTEM_SampleSizes_GenderRace <- ddply(SSTEMsurvey2, .(Gender, Race), summarize,
                                      n = n())

SSTEM_SampleSizes_SchoolGrade
SSTEM_SampleSizes_SchoolGender
SSTEM_SampleSizes_SchoolRace
SSTEM_SampleSizes_GradeGender
SSTEM_SampleSizes_GradeRace
SSTEM_SampleSizes_GenderRace

## Self-efficacy ----
### Manipulate Data ----
#### Change Responses to factors ----
## The data for each item is 
SSTEMsurveyRQ2C <- SSTEMsurvey2 |>
  mutate(
    across(which(str_detect(colnames(SSTEMsurvey), pattern = "Q6|Q23|Q24")), 
           ~ factor(.x, levels = c(1,2,3,4,5), ordered = TRUE))
  )
levels(SSTEMsurveyRQ2C$Q6_1)
### MATH ----
#### Survey Distribution Barplots----
SSTEM_Math_df <- data.frame(SSTEMsurveyRQ2C |> select(str_subset(colnames(SSTEMsurvey), pattern = "Q6")))
SSTEM_Math_labels <- SSTEMquestions[colnames(SSTEM_Math_df)]
SSTEM_Math_labels <- str_replace(SSTEM_Math_labels, pattern = "Math - ", replacement = "")
colnames(SSTEM_Math_df) <- SSTEM_Math_labels

##### No Grouping ----
SSTEMlikertMath <- likert(SSTEM_Math_df)
plot(SSTEMlikertMath) +
  labs(title = "Math items",
       subtitle = "Ordered by positive response percentage")

##### Grouping ----
groupingColumn <- "School"
mathGrouping <- SSTEMsurveyRQ2C |> pull(groupingColumn)
SSTEMlikertMath <- likert(SSTEM_Math_df, grouping = mathGrouping)
plot(SSTEMlikertMath) +
  labs(title = paste0("Math items parsed by ", groupingColumn),
       subtitle = "Ordered by positive response percentage")


#### MULTIPLE LINEAR REGRESSION ----
##### Full Model 1 -----
mlm1 <- lm(data = SSTEMsurveyRQ2C,
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

##### Reduced Model 1 ----
# Refit with only significant terms
mlm1b <- lm(data = SSTEMsurveyRQ2C,
            mlm1_step$call$formula)
summary(mlm1b)
Anova(mlm1b, type = 2)
anova(mlm1b)

###### Diagnostic checks ----
plot(mlm1b, 1)

# Normality
shapiro.test(mlm1b$residuals)

# Independence
durbinWatsonTest(mlm1b)

# Constant Variance of Residuals
ncvTest(mlm1b)

###### Expected Marginal Means -----
mlm1b_emms <- data.frame(emmeans(mlm1b, specs = c("School"), by = c("Grade"), level = 0.95))
mlm1b_preds <- predict(mlm1b, 
                       newdata = SSTEMsurveyRQ2C |> select(School, Grade, Gender, Race), 
                       type = "response")
SSTEMsurveyRQ2C$MathScorePredictions <- mlm1b_preds

###### Plot Effects ----
mlm1b_plot <- ggplot(data = mlm1b_emms) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL, y = Grade, color = School),
                 height = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(aes(x = emmean, y = Grade, color = School),
             position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(1,6), breaks = 1:6) +
  labs(
    title = "S-STEM Self-Efficacy Math Construct Score by\nGrade and School",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    x = "Math Construct Score"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1))
  )
mlm1b_plot

##### Full Model 2 ----
# Try removing interactions of disproportionate Sample Sizes
mlm2 <- lm(data = SSTEMsurveyRQ2C,
           MathScore ~ 
             School 
           + Grade 
           + Gender
           + Race 
           + School:Grade 
           + School:Gender 
           #+ School:Race 
           + Grade:Gender
           #+ Grade:Race
           #+ Gender:Race
)
summary(mlm2)
Anova(mlm2, type = 2)

mlm2_step <- step(mlm2)

##### Reduced Model 2 ----
# Refit with only significant terms
mlm2b <- lm(data = SSTEMsurveyRQ2C,
            mlm2_step$call$formula)
mlm2b_sum <- summary(mlm2b)
mlm2b_sum
Anova(mlm2b, type = 2)
anova(mlm2b)

mlm_varImp <- varImp(mlm2b, useModel = "rf") |> arrange(desc(Overall))
mlm_varImp

###### Diagnostic checks ----
plot(mlm2b, 1)

# Normality
shapiro.test(mlm2b$residuals)

# Independence
durbinWatsonTest(mlm2b)

# Constant Variance of Residuals
ncvTest(mlm2b)

###### Expected Marginal Means -----
mlm2b_emms <- data.frame(emmeans(mlm2b, 
                                 specs = c("School"), 
                                 by = c("Grade"), 
                                 level = 0.95))

###### Plot Effects ----
mlm2b_plot <- ggplot(data = mlm2b_emms) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL, y = Grade, color = School),
                 height = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(aes(x = emmean, y = Grade, color = School),
             position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(1,6), breaks = 1:6) +
  labs(
    title = "S-STEM Self-Efficacy Math Construct Score by\nGrade and School",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    x = "Math Construct Score"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1))
  )
mlm2b_plot

#### BAYESIAN MULTIPLE LINEAR REGRESSION ----
##### General Bayes Factor ----
blmBFtest <- generalTestBF(data = SSTEMsurveyRQ2C,
                           MathScore ~ 
                             School 
                           + Grade 
                           + Gender
                           + Race 
                           + School:Grade 
                           + School:Gender 
                           + School:Race 
                           + Grade:Gender
                           + Grade:Race
                           + Gender:Race, )
summary(blmBFtest)
sort(blmBFtest)
plot(blmBFtest)

##### Full Model 1 ----
# using rstanarm
# Set seed for reproducibility
set.seed(52)
blm1 <- stan_glm(data = SSTEMsurveyRQ2C,
                 MathScore ~ 
                   School 
                 + Grade 
                 + Gender
                 + Race 
                 + School:Grade 
                 + School:Gender 
                 #+ School:Race 
                 + Grade:Gender
                 #+ Grade:Race
                 #+ Gender:Race
                 ,
                 #prior = NULL,
                 # chains = 4,
                 # iter = 5000,
                 # cores = 4
)

summary(blm1, digits = 3)

loo(blm1)
waic(blm1)

add_criterion(blm1)

print(blm1, digits = 3)
summary(blm1, digits = 3)

###### Diagnostic Checks ----
pp_check(blm1, nreps = 1000)

##### Full Model 2 ----
# Using brms
blm2 <- brm(data = SSTEMsurveyRQ2C,
            MathScore ~ 
              School 
            + Grade 
            + Gender
            + Race 
            + School:Grade 
            + School:Gender 
            #+ School:Race 
            + Grade:Gender
            #+ Grade:Race
            #+ Gender:Race
            ,
            family = gaussian(),
            seed = 52
)
print(blm2, digits = 3)

loo(blm2)
waic(blm2)

###### Diagnostic Checks ----
pp_check(blm2, ndraws = 1000)

#### BETA REGRESSION ----
##### Manipulate data ----
SSTEMsurveyRQ2C <- SSTEMsurveyRQ2C |>
  mutate(
    MathScoreScaled = (MathScore - 1)/4,
    ScienceScoreScaled = (ScienceScore - 1)/4,
    EngTechScoreScaled = (EngTechScore - 1)/4
  ) |>
  mutate(
    MathScoreScaled2 = ifelse(MathScoreScaled == 0, 0.00001, 
                              ifelse(MathScoreScaled == 1, 0.99999, MathScoreScaled)),
    ScienceScoreScaled2 = ifelse(ScienceScoreScaled == 0, 0.00001, 
                                 ifelse(ScienceScoreScaled == 1, 0.99999, ScienceScoreScaled)),
    EngTechScoreScaled2 = ifelse(EngTechScoreScaled == 0, 0.00001, 
                                 ifelse(EngTechScoreScaled == 1, 0.99999, EngTechScoreScaled))
  )

##### Model 1 ----
which(SSTEMsurveyRQ2C$MathScoreScaled %in% c(0,1))
t <- scale(SSTEMsurveyRQ2C$MathScore, center = FALSE)
var(t)
mean(t)

set.seed(52)
betaM1 <- betareg(data = SSTEMsurveyRQ2C,
                  MathScoreScaled2 ~ 
                    School 
                  + Grade 
                  + Gender
                  + Race 
                  + School:Grade 
                  + School:Gender 
                  #+ School:Race 
                  + Grade:Gender
                  #+ Grade:Race
                  #+ Gender:Race
                  ,
                  #na.action = na.omit
)
summary(betaM1)
betaM1_sum <- summary(betaM1, type = "pearson", phi = NULL)
betaM1_sum
summary(mlm1b)

###### Diagnostic Checks ----
plot(betaM1)

###### Expected Marginal Means ----
betaM1_emms <- data.frame(emmeans(betaM1, specs = c("School"), by = c("Grade"), level = 0.95))
betaM1_emms2 <- betaM1_emms |> 
  select(
    "School", 
    "Grade",
    "emmean",
    "asymp.LCL",
    "asymp.UCL"
  ) |>
  group_by(School, Grade) |>
  summarize(
    across(everything(), ~.x*4+1)
  )
betaM1_emms2

###### Plot Effects ----
betaM1_plot <- ggplot(data = betaM1_emms2) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = Grade, color = School),
                 height = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(aes(x = emmean, y = Grade, color = School),
             position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(1,6), breaks = 1:6) +
  labs(
    title = "S-STEM Self-Efficacy Math Construct Score by\nGrade and School",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    x = "Math Construct Score"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1))
  )
betaM1_plot
mlm2b_plot

#### BAYESIAN BETA REGRESSION ----
##### Model 1 ----
bbetaM1 <- stan_betareg(data = SSTEMsurveyRQ2C,
                        MathScoreScaled2 ~ 
                          School 
                        + Grade 
                        + Gender
                        + Race 
                        + School:Grade 
                        + School:Gender 
                        #+ School:Race 
                        + Grade:Gender
                        #+ Grade:Race
                        #+ Gender:Race
                        ,
)
bbetaM1_sum <- summary(bbetaM1)
bbetaM1_sum
fixef(bbetaM1)

###### Diagnostic Checks ----
pp_check(bbetaM1, nreps = 1000) + xlim(c(-1,2))
bayes_R2(bbetaM1)


###### Expected Marginal Means ----
bbetaM1_emms <- data.frame(emmeans(bbetaM1, 
                                   specs = c("School"), 
                                   by = c("Grade"), 
                                   level = 0.95,
                                   epred = TRUE))
bbetaM1_emms
bbetaM1_emms2 <- bbetaM1_emms |> 
  select(
    "School", 
    "Grade",
    "emmean",
    "lower.HPD",
    "upper.HPD"
  ) |>
  group_by(School, Grade) |>
  summarize(
    across(everything(), ~inv_logit_scaled(.x, lb = 1, ub = 5))
  )
bbetaM1_emms2
betaM1_emms2

###### Plot Effects ----
bbetaM1_plot <- ggplot(data = bbetaM1_emms2) +
  geom_errorbarh(aes(xmin = lower.HPD, xmax = upper.HPD, y = Grade, color = School),
                 height = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(aes(x = emmean, y = Grade, color = School),
             position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(1,6), breaks = 1:6) +
  labs(
    title = "S-STEM Self-Efficacy Math Construct Score by\nGrade and School",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    x = "Math Construct Score"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1))
  )
bbetaM1_plot

##### Model 2 ----
bbetaM2 <- brm(data = SSTEMsurveyRQ2C,
               MathScoreScaled2 ~ 
                 School 
               + Grade 
               + Gender
               + Race 
               + School:Grade 
               + School:Gender 
               #+ School:Race 
               + Grade:Gender
               #+ Grade:Race
               #+ Gender:Race
               , 
               family = Beta()
)

bbetaM2_sum <- summary(bbetaM2)
bbetaM2_sum
fixef(bbetaM2)

###### Diagnostic Checks ----
pp_check(bbetaM2, ndraws = 1000)
bayes_R2(bbetaM2)

###### Expected Marginal Means ----
bbetaM2_emms <- data.frame(emmeans(bbetaM2, 
                                   specs = c("School"), 
                                   by = c("Grade"), 
                                   level = 0.95,
                                   #epred = TRUE
))
bbetaM2_emms
bbetaM2_emms2 <- bbetaM2_emms |> 
  select(
    "School", 
    "Grade",
    "emmean",
    "lower.HPD",
    "upper.HPD"
  ) |>
  group_by(School, Grade) |>
  summarize(
    across(everything(), ~inv_logit_scaled(.x, lb = 1, ub = 5))
  )
bbetaM2_emms2

###### Plot Effects ----
bbetaM2_plot <- ggplot(data = bbetaM2_emms2) +
  geom_errorbarh(aes(xmin = lower.HPD, xmax = upper.HPD, y = Grade, color = School),
                 height = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(aes(x = emmean, y = Grade, color = School),
             position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(1,6), breaks = 1:6) +
  labs(
    title = "S-STEM Self-Efficacy Math Construct Score by\nGrade and School",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    x = "Math Construct Score"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1))
  )
bbetaM2_plot

bbetaM2_fitted <- data.frame(fitted(bbetaM2, scale = "response"))
bbetaM2_residuals <- data.frame(residuals(bbetaM2))

#### COMPARE MODELS ----
##### Log Likelihood ----
mlm2b_logLik <- logLik(mlm2b)
blm1_logLik <- mean(apply(log_lik(blm1), MARGIN = 1, FUN = sum))
blm2_logLik <- mean(apply(log_lik(blm2), MARGIN = 1, FUN = sum))
betaM1_logLik <- logLik(betaM1)
bbetaM1_logLik <- mean(apply(log_lik(bbetaM1), MARGIN = 1, FUN = sum))
bbetaM2_logLik <- mean(apply(log_lik(bbetaM2), MARGIN = 1, FUN = sum))

logLik_df <- data.frame(
  Model = c(
    "MLR",
    "BLR rstanarm",
    "BLR brms",
    "Beta",
    "Bayes Beta rstanarm",
    "Bayes Beta brms"
  ),
  LogLik = c(
    mlm2b_logLik,
    blm1_logLik,
    blm2_logLik,
    betaM1_logLik,
    bbetaM1_logLik,
    bbetaM2_logLik
  )
)
logLik_df |> arrange(desc(LogLik))

##### Psuedo R^2 ----
mlm2b_r2 <- mlm2b_sum$r.squared
blm1_r2 <- mean(bayes_R2(blm1))
blm2_r2 <- mean(bayes_R2(blm2))
betaM1_r2 <- betaM1$pseudo.r.squared
bbetaM1_r2 <- 1 - var(bbetaM1$residuals)/(var(bbetaM1$residuals)+ var(bbetaM1$fitted.values))
bbetaM2_r2A <- bayes_R2(bbetaM2, robust = FALSE)
bbetaM2_r2B <- 1 - var(bbetaM2_residuals$Estimate)/(var(bbetaM2_residuals$Estimate) + var(bbetaM2_fitted$Estimate))

Rsq_df <- data.frame(
  Model = c(
    "MLR",
    "BLR rstanarm",
    "BLR brms",
    "Beta",
    "Bayes Beta rstanarm",
    "Bayes Beta brms A",
    "Bayes Beta brms B"
  ),
  Rsq = c(
    mlm2b_r2,
    blm1_r2,
    blm2_r2,
    betaM1_r2,
    bbetaM1_r2,
    bbetaM2_r2A[1],
    bbetaM2_r2B
  )
)
Rsq_df |> arrange(desc(Rsq))

##### Leave one out R2 ----
loo(blm1)
loo(blm2)
loo(bbetaM1)
loo(bbetaM2)

loo_compare(
  waic(blm1), 
  waic(blm2),
  waic(bbetaM1),
  waic(bbetaM2)
  )

##### Plot Effects -----
# Multiple Linear Regression
mlm2b_plot + 
  labs(title = NULL, 
       subtitle = "Multiple Linear Regression", 
       x = NULL) +
  annotate(geom = "text", x = 1.1, y = 3, hjust = 0, size = rel(3),
           label = paste0("Log-likelihood = ", round(mlm2b_logLik,3), "\n",
                          "Rsq = ", round(mlm2b_r2,3))) +
  
  # Beta Regression
  betaM1_plot + 
  labs(title = NULL, 
       subtitle = "Beta Regression", 
       x = NULL) +
  annotate(geom = "text", x = 1.1, y = 3, hjust = 0, size = rel(3),
           label = paste0("Log-likelihood = ", round(betaM1_logLik,3), "\n",
                          "Rsq = ", round(betaM1_r2,3))) +
  
  # Bayesian Beta Regression with rstanarm
  bbetaM1_plot + 
  labs(title = NULL, 
       subtitle = "Bayesian Beta Regression with rstanarm",
       x = NULL) +
  annotate(geom = "text", x = 1.1, y = 3, hjust = 0, size = rel(3),
           label = paste0("Log-likelihood = ", round(bbetaM1_logLik,3), "\n",
                          "Rsq = ", round(bbetaM1_r2,3))) +
  
  # Bayesian Beta regression with brms
  bbetaM2_plot + 
  labs(title = NULL,
       subtitle = "Bayesian Beta Regression with brms") +
  annotate(geom = "text", x = 1.1, y = 3, hjust = 0, size = rel(3),
           label = paste0("Log-likelihood = ", round(bbetaM2_logLik,3), "\n",
                          "Rsq = ", round(bbetaM2_r2B,3))) +
  plot_layout(ncol = 1, 
              guides = "collect", 
              axes = "collect_y") +
  plot_annotation(
    title = "S-STEM Self-Efficacy Math Construct Score by Grade and School",
    subtitle = "95% Confidence Interval about Mean Math Construct Score",
    theme = theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)),
                  plot.subtitle = element_text(hjust = 0.5, size = rel(1.5)))
  )


