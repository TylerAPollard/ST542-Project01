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

# 2. Clean Data ==========
## Filter Data ----
Sciencesurvey <- SSTEMsurvey_df |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Gender2,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_df), pattern = "Science")
  ) 

## Check Cronbach alpha of Science Questions
ScienceAlpha <- alpha(Sciencesurvey |> select(str_which(colnames(Sciencesurvey), pattern = "Science")),
                   cumulative = FALSE, keys = my.keys$Science)
ScienceAlpha

## Calculate Aggregate Score ----
Sciencesurvey_data <- Sciencesurvey |>
  mutate(
    Science_Q8 = 6 - Science_Q8,
  ) |>
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
  "YearSemester",
  "School",
  "Grade",
  "Gender2",
  "Race2"
)

t.test(Sciencesurvey_data$ScienceScore)
for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  test <- Sciencesurvey_data |>
    group_by(!!(sym(fact))) |>
    summarise(
      Mean = mean(ScienceScore),
      SE = t.test(ScienceScore, var.equal = FALSE)$stderr,
      LCB = t.test(ScienceScore, var.equal = FALSE)$conf.int[1],
      UCB = t.test(ScienceScore, var.equal = FALSE)$conf.int[2]
    )
  print(test)
}

Sciencesurvey_data |>
  group_by(Gender2, Race2) |>
  summarise(
    Mean = mean(ScienceScore),
    SE = t.test(ScienceScore, var.equal = FALSE)$stderr,
    LCB = t.test(ScienceScore, var.equal = FALSE)$conf.int[1],
    UCB = t.test(ScienceScore, var.equal = FALSE)$conf.int[2]
  )

fact_plots <- list()
for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  fact_plot <- ggplot(data = Sciencesurvey_data) +
    geom_histogram(aes(x = ScienceScore, after_stat(density), fill = !!sym(fact)),
                   binwidth = 0.25,
                   position = position_dodge()) +
    geom_density(aes(x = ScienceScore)) +
    facet_wrap(vars(!!sym(fact))) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
  fact_plots[[i]] <- fact_plot
}

cowplot::plot_grid(plotlist = fact_plots, ncol = 2)


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

# 3. Linear Regression ----
## Frequentist ----
### No Random Effects ----
#### Model 1 ----
linM1 <- lm(data = Sciencesurvey_data,
            ScienceScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2
)
summary(linM1)
step(linM1, direction = "both")

#### Model 2 ----
linM2 <- lm(data = Sciencesurvey_data,
            ScienceScore ~ School + Gender2
)
summary(linM2)

emmeans(linM2, specs = "School")

### Table Report ----
Science_reg_sum <- linM2 |>
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
  modify_caption("**Model Summary for Science Self-Efficacy**") |>
  modify_header(label = "**Variable**") |>
  modify_column_hide(p.value) |>
  modify_footnote(ci = "CI = Credible Interval") |>
  as_gt() |>
  cols_label(estimate = md("$$\beta$$"))

Science_reg_sum
show_header_names(Science_reg_sum)

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
linR1 <- lmer(
  ScienceScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + (1|YearSemester),
  data = Sciencesurvey_data
)
lmerTest::step(linR1)

## Bayesian ----
### General Bayes ----
blinGen <- generalTestBF(
  ScienceScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = Sciencesurvey_data, 
  whichRandom = "YearSemester"
)
sort(blinGen)

blinGen2 <- generalTestBF(
  ScienceScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = Sciencesurvey_data
)
sort(blinGen2)

### Model 1 ----
blinM1NULL <- brm(
  ScienceScore ~ 1,
  data = Sciencesurvey_data,
  family = gaussian(),
  seed = 52,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)

blinM1School <- brm(
  ScienceScore ~ School,
  data = Sciencesurvey_data,
  family = gaussian(),
  seed = 52,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)

blinM1Gender <- brm(
  ScienceScore ~ Gender2,
  data = Sciencesurvey_data,
  family = gaussian(),
  seed = 52,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)


blinM1 <- brm(
  ScienceScore ~ School + Gender2,
  data = Sciencesurvey_data,
  family = gaussian(),
  seed = 52,
  prior = c(prior(normal(0,10), class = b),
            prior(inv_gamma(0.1, 0.1), class = sigma)),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
prior_summary(blinM1)
get_variables(blinM1)
blinPostSum <- posterior_summary(blinM1, variable = get_variables(blinM1)[1:4])
blinPostSum <- data.frame(blinPostSum)
plot(blinM1)
print(blinM1, digits = 4)
summary(blinM1, digits = 4)
performance::check_distribution(blinM1)
performance::check_outliers(blinM1)
performance::check_heteroskedasticity(blinM1)
performance_rmse(blinM1)

pp_check(blinM1, ndraws = 82)

blinPreds <- posterior_predict(blinM1)
blinEPreds <- posterior_epred(blinM1)
blinDraws <- as_draws_df(blinM1)
blinLinPred <- posterior_linpred(blinM1)

MCMC_data <- mcmc_areas_data(blinM1, pars = get_variables(blinM1)[1:3], prob = 0.95)
mcmc_areas(blinM1, pars = get_variables(blinM1)[1:3], prob = 0.95) +
  theme_bw()

summary(linM2)
post_prob(blinM1, blinM1NULL)
post_prob(blinM1, blinM1School)
post_prob(blinM1, blinM1Gender)

mean(blinDraws$b_SchoolPhillipsMiddleSchool > 0)
mean(blinDraws$b_Gender2NotMale > 0)

blinM1 |> 
  emmeans(~ School,
          at = list(Gender2 = "Male"),
          epred = TRUE)

blinM1 |> 
  emmeans(~ School,
          at = list(Gender2 = "Not Male"),
          epred = TRUE)

blinEMM_School <- emmeans(blinM1, specs = c("School"), epred = TRUE)
blinEMM_SchoolSum <- summary(blinEMM_School, point.est = "mean", digits = 3) |>
  mutate(
    School = c("West Edgecombe\nMiddle School", "Phillips\nMiddle School")
  ) |>
  rename(
    Mean = emmean,
    LCB = lower.HPD,
    UCB = upper.HPD
  )

blinM1EMM_School2 <- blinM1 |>
  emmeans(~School) |>
  gather_emmeans_draws() |>
  mean_hdi()
blinM1EMM_School2

blinEMM_Gender2 <- emmeans(blinM1, specs = c("Gender2"), epred = TRUE)
blinEMM_Gender2Sum <- summary(blinEMM_Gender2, point.est = "mean") |>
  rename(
    Mean = emmean,
    LCB = lower.HPD,
    UCB = upper.HPD
  )

blinM1EMM_Gender2 <- blinM1 |>
  emmeans(~Gender2) |>
  gather_emmeans_draws() |>
  mean_hdi()
blinM1EMM_Gender2

conditional_effects(blinM1)

blinM1Science <- blinM1
save(Sciencesurvey_data,
     blinM1Science,
     blinPostSum,
     blinEMM_SchoolSum,
     blinEMM_Gender2Sum,
     file = "Analyses/STEM Self-Efficacy/Science Data.RData")



Science_School_plot <- ggplot(data = blinEMM_SchoolSum) +
  geom_errorbarh(aes(xmin = LCB, xmax = UCB, y = School),
                 color = "dodgerblue2", height = 0.5) +
  geom_point(aes(x = Mean, y = School),
             color = "dodgerblue2", size = 3) +
  scale_x_continuous(limits = c(1, 5), 
                     breaks = seq(1,5, by = 1),
                     minor_breaks = seq(1,5, by = 0.25)) +
  scale_y_discrete(name = "School",
                   limits = rev(blinEMM_SchoolSum$School)) +
  labs(x = "Mean Science Construct Score") +
  theme_bw()
Science_School_plot


Science_Gender_plot <- ggplot(data = blinEMM_Gender2Sum) +
  geom_errorbarh(aes(xmin = LCB, xmax = UCB, y = Gender2),
                 color = "dodgerblue4", height = 0.5) +
  geom_point(aes(x = Mean, y = Gender2),
             color = "dodgerblue4", size = 3) +
  scale_x_continuous(limits = c(1, 5), 
                     breaks = seq(1,5, by = 1),
                     minor_breaks = seq(1,5, by = 0.25)) +
  scale_y_discrete(name = "Gender",
                   limits = rev(blinEMM_Gender2Sum$Gender2)) +
  labs(x = "Mean Science Construct Score") +
  theme_bw()
Science_Gender_plot

Science_School_plot / Science_Gender_plot +
  plot_layout(
    axes = "collect_x"
  ) +
  plot_annotation(
    title = "S-STEM Science Construct Score by School and Gender",
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



