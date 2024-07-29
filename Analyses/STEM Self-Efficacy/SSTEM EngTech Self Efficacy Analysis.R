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
library(performance)

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
  ) |>
  rowwise() |>
  mutate(
    MeanMathScore = mean(c_across(str_subset(colnames(SSTEMsurvey_data), pattern = "Math")), na.rm = TRUE),
    MeanScienceScore = mean(c_across(str_subset(colnames(SSTEMsurvey_data), pattern = "Science")), na.rm = TRUE),
    MeanEngTechScore = mean(c_across(str_subset(colnames(SSTEMsurvey_data), pattern = "EngTech")), na.rm = TRUE),
    MeanLearningScore = mean(c_across(str_subset(colnames(SSTEMsurvey_data), pattern = "Learning")), na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(
    complete.cases(MeanEngTechScore)
  )

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

# 2. Clean Data ==========
## Filter Data ----
EngTechsurvey <- SSTEMsurvey_df |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_df), pattern = "EngTech"),
    -MeanEngTechScore
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
plot(density(log(EngTechsurvey_data$EngTechScore)))

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
  "YearSemester",
  "School",
  "Grade",
  "Gender2",
  "Race2"
)

t.test(EngTechsurvey_data$EngTechScore)
for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  test <- EngTechsurvey_data |>
    group_by(!!(sym(fact))) |>
    summarise(
      Mean = mean(EngTechScore),
      SE = t.test(EngTechScore, var.equal = FALSE)$stderr,
      LCB = t.test(EngTechScore, var.equal = FALSE)$conf.int[1],
      UCB = t.test(EngTechScore, var.equal = FALSE)$conf.int[2]
    )
  print(test)
}

EngTechsurvey_data |>
  group_by(Gender2, Race2) |>
  summarise(
    Mean = mean(EngTechScore),
    SE = t.test(EngTechScore, var.equal = FALSE)$stderr,
    LCB = t.test(EngTechScore, var.equal = FALSE)$conf.int[1],
    UCB = t.test(EngTechScore, var.equal = FALSE)$conf.int[2]
  )

fact_plots <- list()
for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  fact_plot <- ggplot(data = EngTechsurvey_data) +
    geom_histogram(aes(x = EngTechScore, after_stat(density), fill = !!sym(fact)),
                   #binwidth = 0.25,
                   position = position_dodge()) +
    geom_density(aes(x = EngTechScore)) +
    facet_wrap(vars(!!sym(fact))) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
  fact_plots[[i]] <- fact_plot
}

cowplot::plot_grid(plotlist = fact_plots, ncol = 2)

fact_plots2 <- list()
for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  fact_plot <- ggplot(data = EngTechsurvey_data) +
    geom_point(aes(x = !!sym(fact), EngTechScore, color = !!sym(fact))) +
    geom_boxplot(aes(x = !!sym(fact), EngTechScore, color = !!sym(fact))) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
  fact_plots2[[i]] <- fact_plot
}

cowplot::plot_grid(plotlist = fact_plots2, ncol = 2)


### Survey Distribution Barplots----
SSTEM_EngTech_df <- data.frame(EngTechsurvey_data |> 
                                 select(str_subset(colnames(EngTechsurvey_data), pattern = "EngTech_")))
SSTEM_EngTech_df <- SSTEM_EngTech_df |>
  mutate(
    across(everything(), 
           ~ factor(.x, levels = c(1,2,3,4,5), ordered = TRUE))
  )

SSTEM_EngTech_labels <- str_subset(SSTEMsurvey_questions, pattern = "EngTech - ")
SSTEM_EngTech_labels <- str_replace(SSTEM_EngTech_labels, pattern = "EngTech - ", replacement = "")
colnames(SSTEM_EngTech_df) <- SSTEM_EngTech_labels

#### No Grouping ----
SSTEMlikertEngTech <- likert(SSTEM_EngTech_df)
plot(SSTEMlikertEngTech) +
  labs(title = "EngTech items",
       subtitle = "Ordered by positive response percentage")

#### Grouping ----
groupingColumn <- "Race2"
mathGrouping <- EngTechsurvey_data |> pull(groupingColumn)
SSTEMlikertEngTech <- likert(SSTEM_EngTech_df, grouping = mathGrouping)
plot(SSTEMlikertEngTech) +
  labs(title = paste0("EngTech items parsed by ", groupingColumn),
       subtitle = "Ordered by positive response percentage")

# 3. Linear Regression ----
## Frequentist ----
### No Random Effects ----
#### Model 1 ----
linM1 <- lm(data = EngTechsurvey_data,
            EngTechScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2
)
summary(linM1)
step(linM1, direction = "both")

#### Model 2 ----
linM2 <- lm(data = EngTechsurvey_data,
            EngTechScore ~ School + Gender2 + Race2 + Gender2:Race2
)
summary(linM2)

emmeans(linM2, specs = "School")

linM3 <- lm(data = EngTechsurvey_data,
            EngTechScore ~ School + Race2
)

### Table Report ----
EngTech_reg_sum <- linM3 |>
  tbl_regression(
    conf.level = 0.95,
    intercept = TRUE
  ) |>
  # add_global_p(type = 2, #test.statistic = "LR",
  #              keep = FALSE) |>
  add_n(location = c("level")) |> # add number of obs
  # add_significance_stars(pattern = "{p.value}{stars}",
  #                        thresholds = c(0.001, 0.01, 0.1),
  #                        hide_ci = FALSE,
  #                        hide_p = FALSE) |>
  # add_glance_source_note(
  # ) |>
  # # Format
  # bold_p(t = 0.05) |> 
  italicize_levels() |>
  # Titles
  modify_caption("**Model Summary for EngTech Self-Efficacy**") |>
  modify_header(label = "**Variable**") |>
  modify_column_hide(p.value) |>
  modify_footnote(ci = "CI = Credible Interval") |>
  as_gt() |>
  cols_label(estimate = md("$$\beta$$"))

EngTech_reg_sum
show_header_names(EngTech_reg_sum)

# Check assumptions
# Linearity
par(mfrow = c(2,2))
plot(linM2)
# Normality
shapiro.test(linM2$residuals)
# Independence
DurbinWatsonTest(linM2)
# Constant Variance
ncvTest(linM2)

### With Random Effects ----
linR1 <- lmerTest::lmer(
  EngTechScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + (1|YearSemester),
  data = EngTechsurvey_data
)
lmerTest::step(linR1)

linR2 <- lmerTest::lmer(
  EngTechScore ~ School + Race2 + (1|YearSemester),
  data = EngTechsurvey_data
)
anova(linR2, linM3)
anova()

## Bayesian ----
### General Bayes ----
blinGen <- generalTestBF(
  EngTechScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = EngTechsurvey_data, 
  whichRandom = "YearSemester"
)
sort(blinGen)

blinGen2 <- generalTestBF(
  EngTechScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = EngTechsurvey_data
)
sort(blinGen2)

### Model 1 ----
blinM1NULL <- brm(
  EngTechScore ~ 1,
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)

#### No Random Effects ----
blinM1 <- brm(
  EngTechScore ~ School + Race2 + YearSemester,
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM1)
get_variables(blinM1)
print(blinM1, digits = 4)


blinM1B <- brm(
  EngTechScore ~ School + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM1)
get_variables(blinM1)
print(blinM1, digits = 4)

bayes_factor(blinM1B, blinM1)

blinM1C <- brm(
  EngTechScore ~ School + Gender2 + Race2 + Gender2:Race2,
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM1)
get_variables(blinM1)
print(blinM1, digits = 4)

blinM1D <- brm(
  EngTechScore ~ School + Race2,
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM1)
get_variables(blinM1)
print(blinM1, digits = 4)

bayes_factor(blinM1D, blinM1)
bayes_factor(blinM1D, blinM1B)
bayes_factor(blinM1D, blinM1C)


#### With Random Effects ----
blinM2 <- brm(
  EngTechScore ~ School + Race2 + (1|YearSemester),
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma),
            prior(inv_gamma(0.1, 0.1), class = sd)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM2)
get_variables(blinM2)
print(blinM2, digits = 4)
variance_decomposition(blinM2)
fixef(blinM2)
ranef(blinM2)
posterior_summary(blinM2)

bayes_factor(blinM2, blinM1)
bayes_factor(blinM2, blinM1D)

blinM2B <- brm(
  EngTechScore ~ School + Gender2 + Race2 + Gender2:Race2 + (1|YearSemester),
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma),
            prior(inv_gamma(0.1, 0.1), class = sd)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)

bayes_factor(blinM2, blinM2B)


#### With Random Slopes ----
blinM3 <- brm(
  EngTechScore ~ School + Race2 + (0 + School|YearSemester),
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma),
            prior(inv_gamma(0.1, 0.1), class = sd)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM3)
get_variables(blinM3)
print(blinM3, digits = 4)
variance_decomposition(blinM3)
fixef(blinM3)
ranef(blinM3)
posterior_summary(blinM3)

bayes_factor(blinM3, blinM1D)
bayes_factor(blinM3, blinM2)

blinPostSum <- posterior_summary(blinM2, variable = get_variables(blinM1)[1:4])
blinPostSum <- data.frame(blinPostSum)
blinPostSum
plot(blinM2)




### Final Model ----
load(file = "Analyses/STEM Self-Efficacy/EngTech Data.RData")
finalModel <- blinM1EngTech
finalModel <- brm(
  EngTechScore ~ School + Race2,
  data = EngTechsurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(3,10), class = Intercept),
            prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma)
            ),
  save_pars = save_pars(all = TRUE),
  iter = 5000,
  control = list(adapt_delta = 0.99)
)
print(finalModel, digits = 4)
plot(finalModel)
loo(finalModel)
performance::check_distribution(finalModel)
performance::check_outliers(finalModel)
performance::check_heteroskedasticity(finalModel)
performance_rmse(finalModel)
blinPostSumEngTech <- data.frame(posterior_summary(finalModel, variable = get_variables(finalModel)[1:5]))
blinPostSumEngTech
fixef(finalModel)
ranef(finalModel)

pp_check(finalModel, ndraws = 82, type = "dens_overlay_grouped",
         group = "Race2")

blinPreds <- posterior_predict(finalModel)
blinEPreds <- posterior_epred(finalModel)
blinDraws <- as_draws_df(finalModel)
blinLinPred <- posterior_linpred(finalModel)

MCMC_data <- mcmc_areas_data(finalModel, pars = get_variables(finalModel)[1:5], prob = 0.95)
mcmc_areas(finalModel, prob = 0.95, pars = get_variables(finalModel)[1:5]) +
  theme_bw()


blinEMM_EngTech_School <- emmeans(finalModel, specs = c("School"), epred = TRUE)
blinEMM_EngTech_SchoolSum <- summary(blinEMM_EngTech_School, point.est = "mean", digits = 3) |>
  mutate(
    School = c("West Edgecombe\nMiddle School", "Phillips\nMiddle School")
  ) |>
  rename(
    Mean = emmean,
    LCB = lower.HPD,
    UCB = upper.HPD
  )
blinEMM_EngTech_SchoolSum


blinEMM_EngTech_Race2 <- emmeans(finalModel, specs = c("Race2"), epred = TRUE)
blinEMM_EngTech_Race2Sum <- summary(blinEMM_EngTech_Race2, point.est = "mean", digits = 3) |>
  rename(
    Mean = emmean,
    LCB = lower.HPD,
    UCB = upper.HPD
  )
blinEMM_EngTech_Race2Sum


conditional_effects(finalModel)
finalConds <- make_conditions(finalModel, vars = "School")
condEffectsFinal <- conditional_effects(finalModel, effects = "Race2", conditions = finalConds)
condEffectsFinal

blinM1EngTech <- finalModel
save(EngTechsurvey_data,
     blinM1EngTech,
     blinPostSumEngTech,
     blinEMM_EngTech_SchoolSum,
     blinEMM_EngTech_Race2Sum,
     file = "Analyses/STEM Self-Efficacy/EngTech Data.RData")



EngTech_School_plot <- ggplot(data = blinEMM_EngTech_SchoolSum) +
  geom_errorbarh(aes(xmin = LCB, xmax = UCB, y = School),
                 color = "dodgerblue2", height = 0.5) +
  geom_point(aes(x = Mean, y = School),
             color = "dodgerblue2", size = 3) +
  scale_x_continuous(limits = c(1, 5), 
                     breaks = seq(1,5, by = 1),
                     minor_breaks = seq(1,5, by = 0.25)) +
  scale_y_discrete(name = "School",
                   limits = rev(blinEMM_EngTech_SchoolSum$School)) +
  labs(x = "Mean Engineering and Technology Construct Score") +
  theme_bw()
EngTech_School_plot


EngTech_Race_plot <- ggplot(data = blinEMM_EngTech_Race2Sum) +
  geom_errorbarh(aes(xmin = LCB, xmax = UCB, y = Race2),
                 color = "dodgerblue4", height = 0.5) +
  geom_point(aes(x = Mean, y = Race2),
             color = "dodgerblue4", size = 3) +
  scale_x_continuous(limits = c(1, 5), 
                     breaks = seq(1,5, by = 1),
                     minor_breaks = seq(1,5, by = 0.25)) +
  scale_y_discrete(name = "Race",
                   limits = rev(blinEMM_EngTech_Race2Sum$Race2), 
                   labels = c("Other",
                              "Black_African American" = "Black/African American",
                              "White_Caucasian" = "White/Caucasian",
                              "Hispanic_Latino" = "Hispanic/Latino")) +
  labs(x = "Mean Engineering and Technology Construct Score") +
  theme_bw()
EngTech_Race_plot

EngTech_School_plot / EngTech_Race_plot +
  plot_layout(
    axes = "collect_x",
    heights = c(1,2)
  ) +
  plot_annotation(
    title = "S-STEM Engineering and Technology Construct Score by School and Gender",
    subtitle = "95% Credible Interval about Expected Marginal Mean",
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic")
    )
  ) &
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )



