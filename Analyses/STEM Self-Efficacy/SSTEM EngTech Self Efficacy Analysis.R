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

## Plotting
library(scales)
library(RColorBrewer)
library(patchwork)

## Tables
library(gt)
library(gtsummary)

## Data Analysis
library(MASS)
library(ordinal)
library(likert)
library(psych)
library(agricolae)
library(lme4)
library(lmtest)
library(car)
library(emmeans)
library(betareg)
library(caret)
library(jtools)
library(blorr)

## Bayesian Data Analysis
library(DescTools)
library(rstanarm)
library(brms)
library(posterior)
library(bayesplot)
library(BayesFactor)
library(tidybayes)

## Load this package last to reduce package conflictions with dplyr
library(tidyverse) 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SELF EFFICACY ANALYSIS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load SSTEM Survey Data ----
load("Data/S-STEM/Cleaned S-STEM Survey Data.RData")

# 1. Calculate Construct Scores ----
## Filter Data ----
SSTEMsurvey_df <- SSTEMsurvey_data |>
  select(
    SchoolYear,
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_data), pattern = "Math"),
    str_which(colnames(SSTEMsurvey_data), pattern = "Science"),
    str_which(colnames(SSTEMsurvey_data), pattern = "EngTech"),
    str_which(colnames(SSTEMsurvey_data), pattern = "Learning")
  ) #|>
# mutate(
#   Math_Q1 = 6 - Math_Q1,
#   Math_Q3 = 6 - Math_Q3,
#   Math_Q5 = 6 - Math_Q5
# )

SampleSize_data <- SSTEMsurvey_df |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2
  )

SampleSizes <- apply(SampleSize_data, 2, table)
SampleSizesPerc <- lapply(SampleSizes, function(x){round(x/nrow(SampleSize_data)*100, 2)})
SampleSizes
SampleSizesPerc

#describe(mySurvey)

my.keys <- list(
  "Math" = c("-Math_Q1",
             "Math_Q2",
             "-Math_Q3",
             "Math_Q4",
             "-Math_Q5",
             "Math_Q6",
             "Math_Q7",
             "Math_Q8"
  ),
  "Science" = c("Science_Q1",
                "Science_Q2",
                "Science_Q3",
                "Science_Q4",
                "Science_Q5",
                "Science_Q6",
                "Science_Q7",
                "-Science_Q8",
                "Science_Q9"
  ),
  "EngTech" = c("EngTech_Q1",
                "EngTech_Q2",
                "EngTech_Q3",
                "EngTech_Q4",
                "EngTech_Q5",
                "EngTech_Q6",
                "EngTech_Q7",
                "EngTech_Q8",
                "EngTech_Q9"
  ),
  "Learning" = c("Learning_Q1",
                 "Learning_Q2",
                 "Learning_Q3",
                 "Learning_Q4",
                 "Learning_Q5",
                 "Learning_Q6",
                 "Learning_Q7",
                 "Learning_Q8",
                 "Learning_Q9",
                 "Learning_Q10",
                 "Learning_Q11"
  )
)

my.scales <- scoreItems(my.keys, SSTEMsurvey_df)
my.scales

my.scores <- my.scales$scores
my.scores

print(my.scales,short=FALSE)
print(my.scales,short=TRUE)

scales.ov <- scoreOverlap(my.keys,SSTEMsurvey_df)
scales.ov


my.scores <- my.scales$scores
names(my.scales)


headTail(round(my.scores,2) )

describe(my.scores)

pairs.panels(my.scores, pch = ".") 

# MATH CONSTRUCT ====================================================================================
## Filter Data ----
Mathsurvey <- SSTEMsurvey_df |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_df), pattern = "Math")
  ) 

## Check Cronbach alpha of Math Questions
MathAlpha <- alpha(Mathsurvey |> select(str_which(colnames(Mathsurvey), pattern = "Math")),
                   cumulative = FALSE, keys = my.keys$Math)
MathAlpha

## Calculate Aggregate Score ----
Mathsurvey_data <- Mathsurvey |>
  mutate(
    Math_Q1 = 6 - Math_Q1,
    Math_Q3 = 6 - Math_Q3,
    Math_Q5 = 6 - Math_Q5
  ) |>
  rowwise() |>
  mutate(
    MeanMathScore = mean(c_across(str_subset(colnames(Mathsurvey), pattern = "Math")), na.rm = TRUE),
    SumMathScore = sum(c_across(str_subset(colnames(Mathsurvey), pattern = "Math")), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    MathScore = my.scores[,1]
  ) |>
  filter(
    complete.cases(MeanMathScore)
  )


hist((Mathsurvey_data$MeanMathScore))
plot(density(Mathsurvey_data$MeanMathScore))

hist((Mathsurvey_data$MathScore))
plot(density(Mathsurvey_data$MathScore))

hist((Mathsurvey_data$SumMathScore))
plot(density(Mathsurvey_data$SumMathScore))

## Sample Sizes ----
### By Each Factor ----
table(Mathsurvey_data$YearSemester)
table(Mathsurvey_data$School)
table(Mathsurvey_data$Grade)
table(Mathsurvey_data$Gender)
table(Mathsurvey_data$Race2)

MathSampleSize_data <- Mathsurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2
  )

MathSampleSizes <- apply(MathSampleSize_data, 2, table)
MathSampleSizesPerc <- lapply(MathSampleSizes, function(x){round(x/nrow(MathSampleSize_data)*100, 2)})
MathSampleSizes
MathSampleSizesPerc

### All combinations ----
Math_SampleSizes_All <- ddply(Mathsurvey_data, .(School, Grade, Gender, Race2), summarise, .drop = FALSE,
                              n = n())
Math_SampleSizes_All

### Interactions ----
Math_SampleSizes_SchoolGrade <- ddply(Mathsurvey_data, .(School, Grade), summarise, .drop = FALSE,
                                      n = n())
Math_SampleSizes_SchoolGender <- ddply(Mathsurvey_data, .(School, Gender), summarise, .drop = FALSE,
                                       n = n())
Math_SampleSizes_SchoolRace <- ddply(Mathsurvey_data, .(School, Race2), summarise, .drop = FALSE,
                                     n = n())
Math_SampleSizes_GradeGender <- ddply(Mathsurvey_data, .(Grade, Gender), summarise, .drop = FALSE,
                                      n = n())
Math_SampleSizes_GradeRace <- ddply(Mathsurvey_data, .(Grade, Race2), summarise, .drop = FALSE,
                                    n = n())
Math_SampleSizes_GenderRace <- ddply(Mathsurvey_data, .(Gender, Race2), summarise, .drop = FALSE,
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
linM1 <- lmer(data = Mathsurvey_data,
              MathScore ~ 
                Gender2
              + Race2
              + Gender2:Race2
              + (1|YearSemester) 
              + (1|YearSemester:School)
              + (1|YearSemester:School:Grade)
)
linM2 <- lmer(data = Mathsurvey_data,
              MathScore ~ 
                Gender2
              + Race2
              + Gender2:Race2
              + (1|YearSemester) 
              #+ (1|YearSemester:School)
              + (1|YearSemester:School:Grade)
)
anova(linM2, linM1)
summary(linM1)
vif(linM1, type = "terms")
vif(linM1, type = "predictor")
step(linM1, direction = "both")
drop1(linM1, test = "Chisq")
Anova(linM1, type = 3, test.statistic = "F")

# Check assumptions
plot(linM1)
shapiro.test(linM1$residuals)
DurbinWatsonTest(linM1)
ncvTest(linM1)

### Model 2 ----
linM2 <- lm(data = Mathsurvey_data,
            MathScore ~ 
              Gender2
)
summary(linM2)
Anova(linM2, type = 2, test.statistic = "F")

plot(linM2)
shapiro.test(linM2$residuals)
DurbinWatsonTest(linM2)
ncvTest(linM2)

emmeans(linM2, specs = "Gender2")

## BETA REGRESSION ----
### Frequentist ----
#### Model 1 ----
set.seed(52)
betaM1 <- betareg(data = Mathsurvey_data,
                  MathScoreScaled2 ~ 
                    School 
                  + Grade 
                  + Gender2
                  + Race2 
                  + Gender2:Race2, 
)
betaM1_sum <- summary(betaM1, type = "pearson", phi = NULL)
betaM1_sum
Anova(betaM1, type = 3)
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
                                Semester
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
Sciencesurvey <- mySurvey |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(mySurvey), pattern = "Science")
  ) 

## Check Cronbach alpha of Science Questions
ScienceAlpha <- alpha(Sciencesurvey |> select(str_which(colnames(Sciencesurvey), pattern = "Science")),
                      cumulative = FALSE, keys = my.keys$Science)
ScienceAlpha

## Calculate Aggregate Score ----
Sciencesurvey_data <- Sciencesurvey |>
  rowwise() |>
  mutate(
    MeanScienceScore = mean(c_across(str_subset(colnames(Sciencesurvey), pattern = "Science")), na.rm = TRUE),
    SumScienceScore = sum(c_across(str_subset(colnames(Sciencesurvey), pattern = "Science")), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    ScienceScore = my.scores[,2]
  ) |>
  filter(
    complete.cases(MeanScienceScore)
  )


hist((Sciencesurvey_data$MeanScienceScore))
plot(density(Sciencesurvey_data$MeanScienceScore))

hist((Sciencesurvey_data$ScienceScore))
plot(density(Sciencesurvey_data$ScienceScore))

hist((Sciencesurvey_data$SumScienceScore))
plot(density(Sciencesurvey_data$SumScienceScore))

## Sample Sizes ----
### By Each Factor ----
table(Sciencesurvey_data$YearSemester)
table(Sciencesurvey_data$School)
table(Sciencesurvey_data$Grade)
table(Sciencesurvey_data$Gender)
table(Sciencesurvey_data$Race2)

ScienceSampleSize_data <- Sciencesurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2
  )

ScienceSampleSizes <- apply(ScienceSampleSize_data, 2, table)
ScienceSampleSizesPerc <- lapply(ScienceSampleSizes, function(x){round(x/nrow(ScienceSampleSize_data)*100, 2)})
ScienceSampleSizes
ScienceSampleSizesPerc

### All combinations ----
Science_SampleSizes_All <- ddply(Sciencesurvey_data, .(School, Grade, Gender, Race2), summarise, .drop = FALSE,
                                 n = n())
Science_SampleSizes_All

### Interactions ----
Science_SampleSizes_SchoolGrade <- ddply(Sciencesurvey_data, .(School, Grade), summarise, .drop = FALSE,
                                         n = n())
Science_SampleSizes_SchoolGender <- ddply(Sciencesurvey_data, .(School, Gender), summarise, .drop = FALSE,
                                          n = n())
Science_SampleSizes_SchoolRace <- ddply(Sciencesurvey_data, .(School, Race2), summarise, .drop = FALSE,
                                        n = n())
Science_SampleSizes_GradeGender <- ddply(Sciencesurvey_data, .(Grade, Gender), summarise, .drop = FALSE,
                                         n = n())
Science_SampleSizes_GradeRace <- ddply(Sciencesurvey_data, .(Grade, Race2), summarise, .drop = FALSE,
                                       n = n())
Science_SampleSizes_GenderRace <- ddply(Sciencesurvey_data, .(Gender, Race2), summarise, .drop = FALSE,
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




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# EngTech CONSTRUCT ====================================================================================
## Filter Data ----
EngTechsurvey <- mySurvey |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(mySurvey), pattern = "EngTech")
  ) 

## Check Cronbach alpha of EngTech Questions
EngTechAlpha <- alpha(EngTechsurvey |> select(str_which(colnames(EngTechsurvey), pattern = "EngTech")),
                      cumulative = FALSE, keys = my.keys$EngTech)
EngTechAlpha

## Calculate Aggregate Score ----
EngTechsurvey_data <- EngTechsurvey |>
  rowwise() |>
  mutate(
    MeanEngTechScore = mean(c_across(str_subset(colnames(EngTechsurvey), pattern = "EngTech")), na.rm = TRUE),
    SumEngTechScore = sum(c_across(str_subset(colnames(EngTechsurvey), pattern = "EngTech")), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    EngTechScore = my.scores[,3]
  ) |>
  filter(
    complete.cases(MeanEngTechScore)
  )


hist((EngTechsurvey_data$MeanEngTechScore))
plot(density(EngTechsurvey_data$MeanEngTechScore))

hist((EngTechsurvey_data$EngTechScore))
plot(density(EngTechsurvey_data$EngTechScore))

hist((EngTechsurvey_data$SumEngTechScore))
plot(density(EngTechsurvey_data$SumEngTechScore))

## Sample Sizes ----
### By Each Factor ----
table(EngTechsurvey_data$YearSemester)
table(EngTechsurvey_data$School)
table(EngTechsurvey_data$Grade)
table(EngTechsurvey_data$Gender)
table(EngTechsurvey_data$Race2)

EngTechSampleSize_data <- EngTechsurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2
  )

EngTechSampleSizes <- apply(EngTechSampleSize_data, 2, table)
EngTechSampleSizesPerc <- lapply(EngTechSampleSizes, function(x){round(x/nrow(EngTechSampleSize_data)*100, 2)})
EngTechSampleSizes
EngTechSampleSizesPerc

### All combinations ----
EngTech_SampleSizes_All <- ddply(EngTechsurvey_data, .(School, Grade, Gender, Race2), summarise, .drop = FALSE,
                                 n = n())
EngTech_SampleSizes_All

### Interactions ----
EngTech_SampleSizes_SchoolGrade <- ddply(EngTechsurvey_data, .(School, Grade), summarise, .drop = FALSE,
                                         n = n())
EngTech_SampleSizes_SchoolGender <- ddply(EngTechsurvey_data, .(School, Gender), summarise, .drop = FALSE,
                                          n = n())
EngTech_SampleSizes_SchoolRace <- ddply(EngTechsurvey_data, .(School, Race2), summarise, .drop = FALSE,
                                        n = n())
EngTech_SampleSizes_GradeGender <- ddply(EngTechsurvey_data, .(Grade, Gender), summarise, .drop = FALSE,
                                         n = n())
EngTech_SampleSizes_GradeRace <- ddply(EngTechsurvey_data, .(Grade, Race2), summarise, .drop = FALSE,
                                       n = n())
EngTech_SampleSizes_GenderRace <- ddply(EngTechsurvey_data, .(Gender, Race2), summarise, .drop = FALSE,
                                        n = n())

EngTech_SampleSizes_SchoolGrade
EngTech_SampleSizes_SchoolGender
EngTech_SampleSizes_SchoolRace
EngTech_SampleSizes_GradeGender
EngTech_SampleSizes_GradeRace
EngTech_SampleSizes_GenderRace

## Manipulate data ----
EngTechsurvey_data <- EngTechsurvey_data |>
  mutate(
    EngTechScoreScaled = (EngTechScore - 1)/4
  ) |>
  mutate(
    EngTechScoreScaled2 = ifelse(EngTechScoreScaled == 0, 0.00001, 
                                 ifelse(EngTechScoreScaled == 1, 0.99999, EngTechScoreScaled))
  )


## Plot Data ----
### Density ----
parse_fact <- c(
  #"School"#,
  #"Grade"#,
  #"Gender"#,
  "Race2"
)
ggplot(data = EngTechsurvey_data) +
  geom_histogram(aes(x = EngTechScore, after_stat(density), fill = !!sym(parse_fact)
  ),
  binwidth = 0.25,
  position = position_dodge()) +
  geom_density(aes(x = EngTechScore
  )) +
  facet_wrap(vars(!!sym(parse_fact))) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
