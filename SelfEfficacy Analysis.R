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
# SELF EFFICACY ANALYSIS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load SSTEM Survey Data ----
load("Data/Cleaned S-STEM Survey Data.RData")

# MATH CONSTRUCT ====================================================================================
## Filter Data ----
Mathsurvey <- SSTEMsurvey_data |>
  select(
    1:22,
    str_which(colnames(SSTEMsurvey_data), pattern = "Math")
  ) |>
  mutate(
    Math_Q1 = 6 - Math_Q1,
    Math_Q3 = 6 - Math_Q3,
    Math_Q5 = 6 - Math_Q5
  )

## Check Cronbach alpha of Math Questions
CronbachAlpha(Mathsurvey |> select(str_which(colnames(SSTEMsurvey_data), pattern = "Math")), 
              na.rm = TRUE)

alpha(Mathsurvey |> select(str_which(colnames(SSTEMsurvey_data), pattern = "Math")),
      cumulative = TRUE)

## Calculate Aggregate Score ----
Mathsurvey_data <- Mathsurvey |>
  rowwise() |>
  mutate(
    MathScore = mean(c_across(str_subset(colnames(Mathsurvey), pattern = "Math")), na.rm = TRUE)
  ) |>
  filter(
    complete.cases(MathScore)
  )

## Sample Sizes ----
### By Each Factor ----
table(Mathsurvey_data$SchoolYear, Mathsurvey_data$Semester)
table(Mathsurvey_data$School)
table(Mathsurvey_data$Grade)
table(Mathsurvey_data$Gender)
table(Mathsurvey_data$Race2)

### All combinations ----
Math_SampleSizes_All <- ddply(Mathsurvey_data, .(School, Grade, Gender, Race2), summarize,
                              n = n())
Math_SampleSizes_All

### Interactions ----
Math_SampleSizes_SchoolGrade <- ddply(Mathsurvey_data, .(School, Grade), summarize,
                                      n = n())
Math_SampleSizes_SchoolGender <- ddply(Mathsurvey_data, .(School, Gender), summarize,
                                       n = n())
Math_SampleSizes_SchoolRace <- ddply(Mathsurvey_data, .(School, Race2), summarize,
                                     n = n())
Math_SampleSizes_GradeGender <- ddply(Mathsurvey_data, .(Grade, Gender), summarize,
                                      n = n())
Math_SampleSizes_GradeRace <- ddply(Mathsurvey_data, .(Grade, Race2), summarize,
                                    n = n())
Math_SampleSizes_GenderRace <- ddply(Mathsurvey_data, .(Gender, Race2), summarize,
                                     n = n())

Math_SampleSizes_SchoolGrade
Math_SampleSizes_SchoolGender
Math_SampleSizes_SchoolRace
Math_SampleSizes_GradeGender
Math_SampleSizes_GradeRace
Math_SampleSizes_GenderRace

## Manipulate data ----
Mathsurvey_data <- Mathsurvey_data |>
  mutate(
    MathScoreScaled = (MathScore - 1)/4
  ) |>
  mutate(
    MathScoreScaled2 = ifelse(MathScoreScaled == 0, 0.00001, 
                              ifelse(MathScoreScaled == 1, 0.99999, MathScoreScaled))
  )

Mathsurvey_data2 <- Mathsurvey_data |>
  filter(Gender %in% c("Male", "Female")) |>
  mutate(
    Gender = droplevels(Gender)
  )

## Plot Data ----
### Density ----
parse_fact <- c(
  #"School"#,
  #"Grade"#,
  #"Gender"#,
  "Race2"
)
ggplot(data = Mathsurvey_data) +
  geom_histogram(aes(x = MathScore, after_stat(density), fill = !!sym(parse_fact)
                     ),
                 binwidth = 0.25,
                 position = position_dodge()) +
  geom_density(aes(x = MathScore
  )) +
  facet_wrap(vars(!!sym(parse_fact))) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )


### Survey Distribution Barplots----
SSTEM_Math_df <- data.frame(Mathsurvey_data |> 
                              select(str_subset(colnames(Mathsurvey_data), pattern = "Math_")))
SSTEM_Math_df <- SSTEM_Math_df |>
  mutate(
    across(everything(), 
           ~ factor(.x, levels = c(1,2,3,4,5), ordered = TRUE))
  )

SSTEM_Math_labels <- str_subset(SSTEMsurvey_questions, pattern = "Math - ")
SSTEM_Math_labels <- str_replace(SSTEM_Math_labels, pattern = "Math - ", replacement = "")
colnames(SSTEM_Math_df) <- SSTEM_Math_labels

#### No Grouping ----
SSTEMlikertMath <- likert(SSTEM_Math_df)
plot(SSTEMlikertMath) +
  labs(title = "Math items",
       subtitle = "Ordered by positive response percentage")

#### Grouping ----
groupingColumn <- "Race2"
mathGrouping <- Mathsurvey_data |> pull(groupingColumn)
SSTEMlikertMath <- likert(SSTEM_Math_df, grouping = mathGrouping)
plot(SSTEMlikertMath) +
  labs(title = paste0("Math items parsed by ", groupingColumn),
       subtitle = "Ordered by positive response percentage")

## LINEAR REGRESSION ----
### Model 1 ----
linM1 <- lm(data = Mathsurvey_data2,
            MathScore ~ 
              School 
            + Semester
            + Grade 
            + Gender
            + Race2 
            #+ School:Grade 
            + School:Gender 
            #+ School:Race 
            + Grade:Gender
            #+ Grade:Race
            #+ Gender:Race
            )
summary(linM1)
vif(linM1, type = "predictor")
linM1_step <- step(linM1, direction = "both")

drop1(linM1, test = "Chisq")

t.test(Mathsurvey_data2 |> filter(Gender == "Male") |> pull("MathScore"),
       Mathsurvey_data2 |> filter(Gender == "Female") |> pull("MathScore"))

### Model 2 ----
linM2 <- lm(data = Mathsurvey_data2,
            MathScore ~ 
              School 
            + Semester
            + Grade 
            #+ Gender
            + Race2 
            #+ School:Grade 
            + School:Gender 
            #+ School:Race 
            #+ Grade:Gender
            #+ Grade:Race
            #+ Gender:Race
)
summary(linM2)
vif(linM2)
Anova(linM2, type = 2)

## BETA REGRESSION ----
### Frequentist ----
#### Model 1 ----
set.seed(52)
betaM1 <- betareg(data = Mathsurvey_data2,
                  MathScoreScaled2 ~ 
                    School 
                  + Grade 
                  + Gender
                  + Race2 
                  + School:Grade 
                  + School:Gender 
                  + School:Race 
                  + Grade:Gender
                  + Grade:Race
                  + Gender:Race
                  ,
                  #na.action = na.omit
)
betaM1_sum <- summary(betaM1, type = "pearson", phi = NULL)
betaM1_sum
Anova(betaM1)
vif(betaM1)

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

### Bayesian ----
#### Model 1 ----
set.seed(52)
bbetaM1 <- stan_betareg(data = Mathsurvey_data2,
                        MathScoreScaled2 ~ 
                          School 
                        + Grade 
                        + Gender
                        + Race2 
                        #+ School:Grade 
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

#### Model 2 ----
generalBayes <- generalTestBF(data = Mathsurvey_data2,
                              MathScoreScaled2 ~ 
                                School 
                              + Grade 
                              + Gender
                              + Race2 
                              + School:Grade 
                              + School:Gender 
                              + School:Race2 
                              + Grade:Gender
                              + Grade:Race
                              + Gender:Race
                              )
sort(generalBayes)

bbetaM2 <- brm(data = Mathsurvey_data2,
               MathScoreScaled2 ~ 
                 School 
               + Grade 
               + Gender
               + Race2 
               + School:Grade 
               + School:Gender 
               + School:Race2 
               + Grade:Gender
               + Grade:Race
               + Gender:Race
               , 
               family = Beta(),
               seed = 52,
               save_pars = save_pars(all = TRUE) 
)

bbetaM2_sum <- summary(bbetaM2)
bbetaM2_sum
fixef(bbetaM2)

###### Diagnostic Checks ----
pp_check(bbetaM2, ndraws = 80)
pp_check(bbetaM2, ndraws = 1000,
         type = "dens_overlay_grouped",
         group = "Gender")

bayes_R2(bbetaM2)

bbetaM3 <- brm(data = Mathsurvey_data2,
               MathScoreScaled2 ~ 
               #  School 
               #+ Grade 
               + Gender
               #+ Race2 
               #+ School:Grade 
               #+ School:Gender 
               #+ School:Race2 
               #+ Grade:Gender
               #+ Grade:Race
               #+ Gender:Race
               , 
               family = Beta(),
               seed = 52,
               save_pars = save_pars(all = TRUE) 
)

bbetaM3_sum <- summary(bbetaM3)
bbetaM3_sum
fixef(bbetaM3)

###### Diagnostic Checks ----
pp_check(bbetaM3, ndraws = 80)
pp_check(bbetaM3, ndraws = 1000,
         type = "dens_overlay_grouped",
         group = "Gender")

bayes_R2(bbetaM3)

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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SCIENCE CONSTRUCT ====================================================================================
## Filter Data ----
Sciencesurvey <- SSTEMsurvey_data |>
  select(
    1:22,
    str_which(colnames(SSTEMsurvey_data), pattern = "Science")
  ) |>
  mutate(
    Science_Q8 = 6 - Science_Q8
  )

## Check Cronbach alpha of Science Questions
CronbachAlpha(Sciencesurvey |> select(str_which(colnames(Sciencesurvey), pattern = "Science")), 
              na.rm = TRUE)

## Calculate Aggregate Score ----
Sciencesurvey_data <- Sciencesurvey |>
  rowwise() |>
  mutate(
    ScienceScore = mean(c_across(str_subset(colnames(Sciencesurvey), pattern = "Science")), na.rm = TRUE)
  ) |>
  filter(
    complete.cases(ScienceScore)
  )

## Sample Sizes ----
### By Each Factor ----
table(Sciencesurvey_data$SchoolYear, Sciencesurvey_data$Semester)
table(Sciencesurvey_data$School)
table(Sciencesurvey_data$Grade)
table(Sciencesurvey_data$Gender)
table(Sciencesurvey_data$Race2)

### All combinations ----
Science_SampleSizes_All <- ddply(Sciencesurvey_data, .(School, Grade, Gender, Race2), summarize,
                              n = n(), .drop = FALSE)
Science_SampleSizes_All

### Interactions ----
Science_SampleSizes_SchoolGrade <- ddply(Sciencesurvey_data, .(School, Grade), summarize,
                                      n = n())
Science_SampleSizes_SchoolGender <- ddply(Sciencesurvey_data, .(School, Gender), summarize,
                                       n = n())
Science_SampleSizes_SchoolRace <- ddply(Sciencesurvey_data, .(School, Race2), summarize,
                                     n = n())
Science_SampleSizes_GradeGender <- ddply(Sciencesurvey_data, .(Grade, Gender), summarize,
                                      n = n())
Science_SampleSizes_GradeRace <- ddply(Sciencesurvey_data, .(Grade, Race2), summarize,
                                    n = n())
Science_SampleSizes_GenderRace <- ddply(Sciencesurvey_data, .(Gender, Race2), summarize,
                                     n = n())

Science_SampleSizes_SchoolGrade
Science_SampleSizes_SchoolGender
Science_SampleSizes_SchoolRace
Science_SampleSizes_GradeGender
Science_SampleSizes_GradeRace
Science_SampleSizes_GenderRace

## Manipulate data ----
Sciencesurvey_data <- Sciencesurvey_data |>
  mutate(
    ScienceScoreScaled = (ScienceScore - 1)/4
  ) |>
  mutate(
    ScienceScoreScaled2 = ifelse(ScienceScoreScaled == 0, 0.00001, 
                              ifelse(ScienceScoreScaled == 1, 0.99999, ScienceScoreScaled))
  )

Sciencesurvey_data2 <- Sciencesurvey_data |>
  filter(Gender %in% c("Male", "Female")) |>
  mutate(
    Gender = droplevels(Gender)
  )

## Plot Data ----
### Density ----
parse_fact <- c(
  #"School"#,
  #"Grade"#,
  #"Gender"#,
  "Race2"
)
ggplot(data = Sciencesurvey_data) +
  geom_histogram(aes(x = ScienceScore, after_stat(density), fill = !!sym(parse_fact)
  ),
  binwidth = 0.25,
  position = position_dodge()) +
  geom_density(aes(x = ScienceScore
  )) +
  facet_wrap(vars(!!sym(parse_fact))) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )


### Survey Distribution Barplots----
SSTEM_Science_df <- data.frame(Sciencesurvey_data |> 
                              select(str_subset(colnames(Sciencesurvey_data), pattern = "Science_")))
SSTEM_Science_df <- SSTEM_Science_df |>
  mutate(
    across(everything(), 
           ~ factor(.x, levels = c(1,2,3,4,5), ordered = TRUE))
  )

SSTEM_Science_labels <- str_subset(SSTEMsurvey_questions, pattern = "Science - ")
SSTEM_Science_labels <- str_replace(SSTEM_Science_labels, pattern = "Science - ", replacement = "")
colnames(SSTEM_Science_df) <- SSTEM_Science_labels

#### No Grouping ----
SSTEMlikertScience <- likert(SSTEM_Science_df)
plot(SSTEMlikertScience) +
  labs(title = "Science items",
       subtitle = "Ordered by positive response percentage")

#### Grouping ----
groupingColumn <- "Race2"
mathGrouping <- Sciencesurvey_data |> pull(groupingColumn)
SSTEMlikertScience <- likert(SSTEM_Science_df, grouping = mathGrouping)
plot(SSTEMlikertScience) +
  labs(title = paste0("Science items parsed by ", groupingColumn),
       subtitle = "Ordered by positive response percentage")