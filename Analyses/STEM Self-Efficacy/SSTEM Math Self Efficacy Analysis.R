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
  "YearSemester",
  "School",
  "Grade",
  "Gender2",
  "Race2"
)

for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  test <- Mathsurvey_data |>
    group_by(!!(sym(fact))) |>
    summarise(
      Mean = mean(MathScore),
      SE = t.test(MathScore, var.equal = FALSE)$stderr,
      LCB = t.test(MathScore, var.equal = FALSE)$conf.int[1],
      UCB = t.test(MathScore, var.equal = FALSE)$conf.int[2]
    )
  print(test)
}

Mathsurvey_data |>
  group_by(Gender2, Race2) |>
  summarise(
    Mean = mean(MathScore),
    SE = t.test(MathScore, var.equal = FALSE)$stderr,
    LCB = t.test(MathScore, var.equal = FALSE)$conf.int[1],
    UCB = t.test(MathScore, var.equal = FALSE)$conf.int[2]
  )

fact_plots <- list()
for(i in 1:length(parse_fact)){
  fact <- parse_fact[i]
  fact_plot <- ggplot(data = Mathsurvey_data) +
    geom_histogram(aes(x = MathScore, after_stat(density), fill = !!sym(fact)),
                   binwidth = 0.25,
                   position = position_dodge()) +
    geom_density(aes(x = MathScore)) +
    facet_wrap(vars(!!sym(fact))) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
  fact_plots[[i]] <- fact_plot
}

cowplot::plot_grid(plotlist = fact_plots, ncol = 2)


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

# 3. Linear Regression ----
## Frequentist ----
### No Random Effects ----
#### Model 1 ----
linM1 <- lm(data = Mathsurvey_data,
            MathScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2
)
summary(linM1)
step(linM1, direction = "both")

#### Model 2 ----
linM2 <- lm(data = Mathsurvey_data,
            MathScore ~ Gender2
)
summary(linM2)

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
  MathScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + (1|YearSemester),
  data = Mathsurvey_data
)
lmerTest::step(linR1)

## Bayesian ----
### General Bayes ----
blinGen <- generalTestBF(
  MathScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = Mathsurvey_data, 
  whichRandom = "YearSemester"
)
sort(blinGen)

blinGen2 <- generalTestBF(
  MathScore ~ School + Grade + Gender2 + Race2 + Gender2:Race2 + YearSemester,
  data = Mathsurvey_data
)
sort(blinGen2)

### NULL Model ----
blinNULL <- brm(
  MathScore ~ 1,
  data = Mathsurvey_data,
  family = gaussian(),
  seed = 52,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)
print(blinNULL, digits = 3)
MathModelSum <- posterior_summary(blinNULL, variable = get_variables(blinNULL)[1:2])
summary(blinNULL, digits = 3)
mcmcArea <- mcmc_areas(blinNULL, pars = "b_Intercept", prob = .95)

blinPreds <- posterior_predict(blinNULL)
quantile(Mathsurvey_data$MathScore, probs = c(0.025, 0.5, 0.975))

blinDraws <- as_draws_df(blinNULL, variable = "Intercept")
summarise_draws(blinDraws, ~quantile(.x, probs = c(0.025, 0.5, 0.975)))

mathPlotdf <- point_interval(blinDraws, .interval = hdi, .point = mean)
mathPlotdf <- mathPlotdf |>
  rename(
    Mean = Intercept,
    LCB = .lower,
    UCB = .upper
  )

# 4. Create Plot ----
mathPlot <- ggplot() + 
  geom_errorbarh(data = mathPlotdf,
                 aes(y = "", xmin = LCB, xmax = UCB),
                 height = 0.25, color = "dodgerblue2") +
  geom_point(data = mathPlotdf,
             aes(y = "", x = Mean),
             size = 3,  color = "dodgerblue2") + 
  scale_x_continuous(limits = c(1,5), 
                     breaks = seq(1,5, by = 1),
                     minor_breaks = seq(1,5, by = 0.25)) +
  labs(
    title = "S-STEM Math Construct Score",
    subtitle = "95% Credible Interval about Mean Score",
    x = "Mean Math Construct Score",
    y = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", size = 14),
    panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
mathPlot

blinNULLMath <- blinNULL
MathModelSum <- data.frame(MathModelSum)
save(Mathsurvey_data,
     blinNULLMath,
     MathModelSum,
     mathPlotdf,
     file = "Analyses/STEM Self-Efficacy/Math Data.RData")



