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

Learningsurvey_df3 <- Learningsurvey_df |>
  mutate(
    across(-LearningScore, ~as.character(.x))
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

hist(log(Learningsurvey_df$LearningScore))
plot(density(log(Learningsurvey_df$LearningScore)))

boxcox_model <- lm(LearningScore ~ 1, data = Learningsurvey_df)
boxcoxLearningScore <- boxCox(boxcox_model)
boxcoxLearningScore_lambda <- boxcoxLearningScore$x[which.max(boxcoxLearningScore$y)]

ddply(Learningsurvey_df, .(School, Gender), summarise,
      mean(LearningScore))
ddply(Learningsurvey_df, .(School), summarise,
      mean(LearningScore))
ddply(Learningsurvey_df, .(Gender), summarise,
      mean(LearningScore))

#### Normal ----
##### General Tests ----
generalBF1 <- generalTestBF(LearningScore ~ . + .^2, 
                            #whichModels = "withmain",
                            data = Learningsurvey_df)
topgenM1 <- head(generalBF1)
topgenM1[1] / topgenM1[5]
topgenM1

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
topgenM2 <- head(generalBF2)
topgenM2[1] / topgenM2[2]

##### Model 1 ----
stanlinM1 <- stan_glm(LearningScore ~ School + Gender,
                      data = Learningsurvey_df,
                      prior_intercept = normal(0,10, autoscale = FALSE), 
                      prior = normal(0,10,autoscale = FALSE),
                      prior_aux = exponential()
                      sparse = FALSE
)

prior_summary(stanlinM1)
posterior_summary(stanlinM1)

print(stanlinM1, digits = 3)
summary(stanlinM1, digits = 3)

fixef(stanlinM1)
mcmc_areas(stanlinM1)

# No Random Effect for Semester Year
linM1prior_coef <- prior("normal(0,10)", class = "b")
linM1prior_Int <- prior(normal(0,10))
linM1prior_sd <- prior("gamma(0.1, 0.1)", class = "sigma")

linM1priors <- c(linM1prior_coef, linM1prior_sd)
linM1 <- brm(bf(LearningScore ~ School + Gender,
                center = FALSE),
             data = Learningsurvey_df,
             family = gaussian(link = "identity"),
             prior = linM1priors,
             save_pars = save_pars(all = TRUE),
             iter = 2000, 
             seed = 52)
prior_summary(linM1)
posterior_summary(linM1)

prior_summary(linM1)
posterior_summary(linM1)


##### Model 2 ----
# Random Effect for Semester Year
linM2prior_coef <- prior("normal(0,10)", class = "b")
linM2prior_Int <- prior("normal(3,2)", class = "b", coef = "Intercept")
linM2prior_sd <- prior("cauchy(0,10)", class = "sd")
linM2prior_sigma <- prior("cauchy(0,10)", class = "sigma")

linM2priors <- c(
  linM2prior_coef, 
  #linM2prior_Int,
  linM2prior_sd,
  linM2prior_sigma
)

linM2 <- brm(bf(LearningScore ~ School + Gender + (1|YearSemester),
                center = FALSE
                ),
             data = Learningsurvey_df,
             family = gaussian(link = "identity"),
             prior = linM2priors,
             save_pars = save_pars(all = TRUE),
             iter = 4000, 
             control = list(adapt_delta = 0.99),
             seed = 52)
prior_summary(linM2)
posterior_summary(linM2)

plot(linM2)
print(linM2, digits = 3)
summary(linM2, digits = 3)

bayes_factor(linM2, linM1)
coef(linM2)
mcmc_areas(linM2, prob = 0.95,
           pars = c(variables(linM2)[1:4])
)


linM2stanA <- stan_lmer(bf(LearningScore ~ School + Gender + (1|YearSemester),
                           center = FALSE),
                        data = Learningsurvey_df,
                        prior = normal(0,5, autoscale = FALSE),
                        prior_intercept = normal(3, 2, autoscale = FALSE),
                        iter = 2000, 
                        seed = 52)
prior_summary(linM2stan)
posterior_summary(linM2stan)

summary(linM2stan, digits = 3)

t <- coef(linM2stan)

mcmc_areas(linM2stan, prob = 0.95,
           pars = rownames(posterior_summary(linM2stan))[1:4])

loo(linM2)

pp_check(linM1, ndraws = 100)
pp_check(linM2, ndraws = 100)

waic(linM1)
waic(linM2)


#### Bayesplot PPCs ----
Y <- Learningsurvey_df$LearningScore
Yrep <- posterior_predict(linM2)

ppcL2 <- apply(Yrep, 1, function(x) quantile(x, 0.025))
ppcU2 <- apply(Yrep, 1, function(x) quantile(x, 0.975))
ppcMedian2 <- apply(Yrep, 1, median)
ppcMean2 <- apply(Yrep, 1, mean)
ppcSD2 <- apply(Yrep, 1, sd)

DppcL2 <- quantile(Y, 0.025)
DppcU2 <- quantile(Y, 0.975)
DppcMedian2 <- median(Y)
DppcMean2 <-mean(Y)
DppcSD2 <- sd(Y)


##### Overall Density ----
ppc_density_plot <- 
  ppc_dens_overlay(Y, Yrep[sample(1:8000, 1000, replace = FALSE), ]) +
  labs(title = "Posterior Predictive Checks of Linear Regression on Training Data",
       subtitle = "Simulated Data Sets Compared to Training Data") +
  theme_bw() +
  legend_none() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1)))
ppc_density_plot

### Residuals
ppc_hist_errors_plot <- ppc_error_hist_grouped(Y, Yrep)

##### Q2.5%  ----
ppc_q2.5_plot2 <- 
  ppc_stat(Y, Yrep, stat = function(y) quantile(y, 0.025), freq = FALSE) +
  labs(title = "2.5% Quantile") +
  theme_bw() +
  legend_none()
ppc_q2.5_plot2

ppcL2dens <- density(ppcL2)
ppcL2dens <- cbind(ppcL2dens$x, ppcL2dens$y)
ppcL2densB <- ppcL2dens[between(ppcL2dens[,1], quantile(ppcL2, 0.025), quantile(ppcL2, 0.975)), ] 
ppc_q2.5_plot2B <- ggplot() +
  # geom_histogram(aes(x = ppcL2,  after_stat(density)),
  #                fill = "#bcdcdc", color = "#99c7c7") +
  geom_ribbon(aes(x = ppcL2densB[,1], ymin = 0, ymax = ppcL2densB[,2]),
              fill = "#bcdcdc") +
  geom_density(aes(x = ppcL2), color = "#99c7c7", linewidth = .75) +
  #geom_vline(aes(xintercept = quantile(ppcL2, 0.975)), color = "#007C7C", linewidth = 2) +
  geom_vline(aes(xintercept = DppcL2), color = "#007C7C", linewidth = 1) +
  # geom_text(aes(label = paste0("p-value = ", round(mean(ppcL2 > DppcL2), 3))),
  #           x = 0.84*max(ppcL2), y = max(ppcL2dens[,2]), size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(title = paste0("2.5% Quantile", "  (p-value = ", round(mean(ppcL2 > DppcL2), 3), ")"),
       x = NULL,
       y = "Posterior Density") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(1)))
ppc_q2.5_plot2B

##### Q97.5%  ----
ppc_q97.5_plot2 <- 
  ppc_stat(Y, Yrep, stat = function(y) quantile(y, 0.975)) +
  labs(title = "97.5% Quantile") +
  theme_bw() +
  legend_none()

ppcU2dens <- density(ppcU2)
ppcU2dens <- cbind(ppcU2dens$x, ppcU2dens$y)
ppcU2densB <- ppcU2dens[between(ppcU2dens[,1], quantile(ppcU2, 0.025), quantile(ppcU2, 0.975)), ] 
ppc_q97.5_plot2B <- ggplot() +
  # geom_histogram(aes(x = ppcU2,  after_stat(density)),
  #                fill = "#bcdcdc", color = "#99c7c7") +
  geom_ribbon(aes(x = ppcU2densB[,1], ymin = 0, ymax = ppcU2densB[,2]),
              fill = "#bcdcdc") +
  geom_density(aes(x = ppcU2), color = "#99c7c7", linewidth = .75) +
  #geom_vline(aes(xintercept = quantile(ppcU2, 0.975)), color = "#007C7C", linewidth = 2) +
  geom_vline(aes(xintercept = DppcU2), color = "#007C7C", linewidth = 1) +
  # geom_text(aes(label = paste0("p-value = ", round(mean(ppcU2 > DppcU2),3))),
  #           x = 0.82*max(ppcU2), y = max(ppcU2dens[,2]), size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(title = paste0("97.5% Quantile", "  (p-value = ", round(mean(ppcU2 > DppcU2), 3), ")"),
       x = NULL,
       y = "Posterior Density") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(1)))
ppc_q97.5_plot2B

##### Median ----
ppc_median_plot2 <- 
  ppc_stat(Y, Yrep, stat = "median") +
  labs(title = "Median") +
  theme_bw() +
  legend_none()

ppcMedian2dens <- density(ppcMedian2)
ppcMedian2dens <- cbind(ppcMedian2dens$x, ppcMedian2dens$y)
ppcMedian2densB <- ppcMedian2dens[between(ppcMedian2dens[,1], quantile(ppcMedian2, 0.025), quantile(ppcMedian2, 0.975)), ] 
ppc_median_plot2B <- ggplot() +
  # geom_histogram(aes(x = ppcMedian2,  after_stat(density)),
  #                fill = "#bcdcdc", color = "#99c7c7") +
  geom_ribbon(aes(x = ppcMedian2densB[,1], ymin = 0, ymax = ppcMedian2densB[,2]),
              fill = "#bcdcdc") +
  geom_density(aes(x = ppcMedian2), color = "#99c7c7", linewidth = .75) +
  #geom_vline(aes(xintercept = quantile(ppcMedian2, 0.975)), color = "#007C7C", linewidth = 2) +
  geom_vline(aes(xintercept = DppcMedian2), color = "#007C7C", linewidth = 1) +
  # geom_text(aes(label = paste0("p-value = ", round(mean(ppcMedian2 > DppcMedian2),3))),
  #           x = 0.72*max(ppcMedian2), y = max(ppcMedian2dens[,2]), size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(title = paste0("Median", "  (p-value = ", round(mean(ppcMedian2 > DppcMedian2),3), ")"),
       x = NULL,
       y = "Posterior Density") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(1)))
ppc_median_plot2B

##### Mean ----
ppc_mean_plot2 <- 
  ppc_stat(Y, Yrep, stat = "mean") +
  labs(title = "Mean") +
  theme_bw() +
  legend_none()

ppcMean2dens <- density(ppcMean2)
ppcMean2dens <- cbind(ppcMean2dens$x, ppcMean2dens$y)
ppcMean2densB <- ppcMean2dens[between(ppcMean2dens[,1], quantile(ppcMean2, 0.025), quantile(ppcMean2, 0.975)), ] 
ppc_mean_plot2B <- ggplot() +
  # geom_histogram(aes(x = ppcMean2,  after_stat(density)),
  #                fill = "#bcdcdc", color = "#99c7c7") +
  geom_ribbon(aes(x = ppcMean2densB[,1], ymin = 0, ymax = ppcMean2densB[,2]),
              fill = "#bcdcdc") +
  geom_density(aes(x = ppcMean2), color = "#99c7c7", linewidth = .75) +
  #geom_vline(aes(xintercept = quantile(ppcMean2, 0.975)), color = "#007C7C", linewidth = 2) +
  geom_vline(aes(xintercept = DppcMean2), color = "#007C7C", linewidth = 1) +
  # geom_text(aes(label = paste0("p-value = ", round(mean(ppcMean2 > DppcMean2),3))),
  #           x = 0.96*max(ppcMean2), y = max(ppcMean2dens[,2]), size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(title = paste0("Mean", "  (p-value = ", round(mean(ppcMedian2 > DppcMedian2),3), ")"),
       x = NULL,
       y = "Posterior Density") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(1)))
ppc_mean_plot2B

##### Standard Deviation ----
ppc_sd_plot2 <- 
  ppc_stat(Y, Yrep, stat = "sd") +
  labs(title = "Standard Deviation") +
  theme_bw() +
  legend_none()

ppcSD2dens <- density(ppcSD2)
ppcSD2dens <- cbind(ppcSD2dens$x, ppcSD2dens$y)
ppcSD2densB <- ppcSD2dens[between(ppcSD2dens[,1], quantile(ppcSD2, 0.025), quantile(ppcSD2, 0.975)), ] 
ppc_sd_plot2B <- ggplot() +
  # geom_histogram(aes(x = ppcSD2,  after_stat(density)),
  #                fill = "#bcdcdc", color = "#99c7c7") +
  geom_ribbon(aes(x = ppcSD2densB[,1], ymin = 0, ymax = ppcSD2densB[,2], linetype = "A"),
              fill = "#bcdcdc") +
  geom_density(aes(x = ppcSD2), color = "#99c7c7", linewidth = .75) +
  #geom_vline(aes(xintercept = quantile(ppcSD2, 0.975)), color = "#007C7C", linewidth = 2) +
  geom_vline(aes(xintercept = DppcSD2, linetype = "B"), color = "#99c7c7", alpha = 0, linewidth = 0.75) +
  geom_vline(aes(xintercept = DppcSD2, linetype = "C"), color = "#007C7C", linewidth = 1) +
  # geom_text(aes(label = paste0("p-value = ", round(mean(ppcSD2 > DppcSD2),3))),
  #           x = 0.965*max(ppcSD2), y = max(ppcSD2dens[,2]), size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(title = paste0("Standard Deviation", "  (p-value = ", round(mean(ppcSD2 > DppcSD2),3), ")"),
       x = NULL,
       y = "Posterior Density") +
  scale_linetype_manual(
    name = element_blank(),
    values = c(1,1,1),
    breaks = c("C", "B", "A"),
    labels = c("Observed", "Posterior", "95% Credible Interval")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        alpha = c(1,1,1),
        shape = c(NA, "--", NA)
      )
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(1)))
ppc_sd_plot2B
ppc_legend2b <- ggpubr::get_legend(ppc_sd_plot2B)

#### Combination Plot ----
ppc_lay2 <- rbind(c(NA,NA, rep(1,8),NA, NA),
                  c(rep(2,4),rep(3,4),rep(4,4)),
                  c(NA, NA, rep(5,4), rep(6,6))
)

ppcComb_plot <- bayesplot_grid(
  plots = list(
    ppc_density_plot,
    ppc_q2.5_plot2B,
    ppc_median_plot2B,
    ppc_q97.5_plot2B,
    ppc_mean_plot2B,
    ppc_sd_plot2B),
  grid_args = list(
    layout_matrix = ppc_lay2
  )
)
ppcComb_plot
