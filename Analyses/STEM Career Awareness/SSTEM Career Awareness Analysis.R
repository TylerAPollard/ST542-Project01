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
  filter(
    complete.cases(Awareness_Q1)
  ) |>
  mutate(
    Awareness_Q1 = ifelse(Awareness_Q1 == 1, "Yes",
                          ifelse(Awareness_Q1 == 2, "No", "Not Sure")),
    Awareness_Q2 = ifelse(Awareness_Q2 == 1, "Yes",
                          ifelse(Awareness_Q2 == 2, "No", "Not Sure")),
    Awareness_Q3 = ifelse(Awareness_Q3 == 1, "Yes",
                          ifelse(Awareness_Q3 == 2, "No", "Not Sure")),
    Awareness_Q4 = ifelse(Awareness_Q4 == 1, "Yes",
                          ifelse(Awareness_Q4 == 2, "No", "Not Sure"))
  ) |>
  mutate(
    Awareness_Q1B = ifelse(Awareness_Q1 == "Yes", 1, 0),
    Awareness_Q2B = ifelse(Awareness_Q2 == "Yes", 1, 0),
    Awareness_Q3B = ifelse(Awareness_Q3 == "Yes", 1, 0),
    Awareness_Q4B = ifelse(Awareness_Q4 == "Yes", 1, 0)
  ) |>
  mutate(
    Awareness_Q1C = factor(Awareness_Q1B),
    Awareness_Q2C = factor(Awareness_Q2B),
    Awareness_Q3C = factor(Awareness_Q3B),
    Awareness_Q4C = factor(Awareness_Q4B)
    # Awareness_Q1C = factor(ifelse(Awareness_Q1 == 1, "Yes", "No")),
    # Awareness_Q2C = factor(ifelse(Awareness_Q2 == 1, "Yes", "No")),
    # Awareness_Q3C = factor(ifelse(Awareness_Q3 == 1, "Yes", "No")),
    # Awareness_Q4C = factor(ifelse(Awareness_Q4 == 1, "Yes", "No"))
  ) |>
  mutate(
    Gender2 = factor(Gender, 
                     levels = c("Male", "Female", "Other"),
                     labels = c("Male", "Not Male", "Not Male"))
  )


## Check Cronbach alpha of all 4 Questions
alpha(CareerAwareness |> select(str_which(colnames(CareerAwareness), pattern = "B")),
      cumulative = FALSE, na.rm = TRUE)

table(CareerAwareness$Gender2, CareerAwareness$Race2)

# Analyze =======================
## SCIENTIST ===================
### Mixed Model ----
awarenessSci_M1R <- glmer(Awareness_Q1C ~
                            School
                          + Grade
                          + Gender2
                          + Race2
                          + Gender2:Race2
                          + (1|YearSemester)
                          ,
                          data = CareerAwareness,
                          family = binomial(link = "logit")
)
print(awarenessSci_M1R)
summary(awarenessSci_M1R)
Anova(awarenessSci_M1R, type = 3)
# Random Effect insignificant

### Fixed Effects ----
awarenessSci_M1 <- glm(Awareness_Q1C ~
                         School
                       + Grade
                       + Gender2
                       + Race2
                       #+ Gender2:Race2
                       ,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
print(awarenessSci_M1)
summary(awarenessSci_M1)
Anova(awarenessSci_M1, type = 2)

step(awarenessSci_M1)

anova(awarenessSci_M1R, awarenessSci_M1)

### FINAL Model ----
awarenessSci_final <- glm(Awareness_Q1C ~
                            #   School
                            # + Grade
                            + Gender2
                          # + Race2
                          #+ Gender2:Race2
                          ,
                          data = CareerAwareness,
                          family = binomial(link = "logit")
)
print(awarenessSci_final)
summary(awarenessSci_final)
Anova(awarenessSci_final, type = 2, test.statistic = "LR")

drop1(awarenessSci_final, test = "LR")
plot(awarenessSci_final)

summ(awarenessSci_final, digits = 4, confint = TRUE)

blr_model_fit_stats(awarenessSci_final)
blr_confusion_matrix(awarenessSci_final)
blr_test_hosmer_lemeshow(awarenessSci_final)

### Predict ----
awarenessSci_preds <- predict(awarenessSci_final, type = "response")
awarenessSci_preds


### EMMs ----
#### Gender ----
awarenessSci_emmeans <- emmeans(awarenessSci_final, specs = "Gender2")
awarenessSci_emmeans_df <- summary(awarenessSci_emmeans, type = "response")
colnames(awarenessSci_emmeans_df)[1] <- "Gender"
# awarenessSci_emmeans_df <- awarenessSci_emmeans_df |>
#   mutate(
#     n = table(CareerAwareness$Gender2),
#     Gender = paste0(Gender, "\n(n = ", n, ")")
#   )


### REPORT ----
#### Table Summary ----
awarenessSci_final_sumTable <- awarenessSci_final |>
  tbl_regression(exponentiate = TRUE,
                 conf.level = 0.95, add_estimate_to_reference_rows = TRUE, ) |>
  # Stats
  add_global_p(type = 2, test.statistic = "LR", ) |> # add global p-value
  add_n(location = c("label","level")) |> # add number of obs
  add_significance_stars(pattern = "{p.value}{stars}",
                         thresholds = c(0.001, 0.01, 0.1),
                         hide_ci = FALSE,
                         hide_p = FALSE) |>
  add_glance_source_note(
    include = c(null.deviance, deviance,
                AIC, BIC, nobs)
  ) |>
  # Format
  bold_p() |> # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.05) |> # now bold q-values under the threshold of 0.10
  #bold_labels() |>
  italicize_levels() |>
  # Titles
  modify_caption("**Model Summary for \n Scientist Career Awareness**") |>
  as_gt()

awarenessSci_final_sumTable
show_header_names(awarenessSci_final_sumTable)

##### Save table ----
gtsave(data = awarenessSci_final_sumTable,
       filename = "Analyses/STEM Career Awareness/STEM Career Awareness Science Table.png")

#### Plot ----
##### Gender ----
hcl.colors(3, "Red-Green")
muted("red", l = 70, c = 70)
muted("green", l = 70, c = 70)

# width = 750, height = 300
###### Vertical ----
awarenessSci_plot_Gender <- ggplot(data = awarenessSci_emmeans_df) +
  #geom_col(aes(x = Gender, y = prob, fill = "Scientists")) +
  geom_col(aes(x = Gender, y = prob, fill = prob)) +#, fill = "#E41A1C") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Gender),
                width = 0.25, linewidth = 1) +
  # geom_text(aes(x = rev(Gender), y = -0.05, label = paste0("n = ", n)),
  #           size = rel(4.5)) +
  #scale_fill_manual(name = NULL, values = c("#E41A1C")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "Gender2",
                   limits = rev(awarenessSci_emmeans_df$Gender)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    title = "DeSIRE Students' Career Awareness of Scientists by Aggregated Gender",
    subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Scientist Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), face = "italic"),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessSci_plot_Gender

### SAVE DATA ----
save(awarenessSci_final,
     awarenessSci_emmeans_df,
     file = "Analyses/STEM Career Awareness/CareerAwarenessScientist.RData")

## ENGINEER ===================
### Mixed Model ----
awarenessEng_M1R <- glmer(Awareness_Q2C ~
                            School
                          + Grade
                          + Gender2
                          + Race2
                          + Gender2:Race2
                          + (1|YearSemester)
                          ,
                          data = CareerAwareness,
                          family = binomial(link = "logit")
)
print(awarenessEng_M1R)
summary(awarenessEng_M1R)
Anova(awarenessEng_M1R, type = 3)
# Random Effect insignificant

drop1(awarenessEng_M1R)

### Fixed Effects ----
#### Full Model ----
awarenessEng_M1 <- glm(Awareness_Q2C ~
                         School
                       + Grade
                       + Gender2
                       + Race2
                       + Gender2:Race2
                       ,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
print(awarenessEng_M1)
summary(awarenessEng_M1)
Anova(awarenessEng_M1, type = 3)

#### Reduced Model ----
awarenessEng_M2 <- glm(Awareness_Q2C ~
                         School
                       + Grade
                       + Gender2
                       + Race2
                       #+ Gender2:Race2
                       ,
                       data = CareerAwareness,
                       family = binomial(link = "logit")
)
print(awarenessEng_M2)
summary(awarenessEng_M2)
Anova(awarenessEng_M2, type = 2, test.statistic = "LR")

step(awarenessEng_M2)
jtools::summ(awarenessEng_M2, digits = 4)

### FINAL Model ----
awarenessEng_final <- glm(Awareness_Q2C ~
                            School
                          + Grade
                          #+ Gender2
                          #+ Race2
                          #+ Gender2:Race2
                          ,
                          data = CareerAwareness,
                          family = binomial(link = "logit")
)
print(awarenessEng_final)
summary(awarenessEng_final)
Anova(awarenessEng_final, type = 2, test.statistic = "LR")

drop1(awarenessEng_final, test = "none")

summ(awarenessEng_final, digits = 4, confint = TRUE)

blr_model_fit_stats(awarenessEng_final)
blr_confusion_matrix(awarenessEng_final)
blr_test_hosmer_lemeshow(awarenessEng_final)

#### Plot Coefficients ----
plot_summs(awarenessEng_final, plot.distributions = TRUE, inner_ci_level = .95)

### Predict ----
awarenessEng_preds <- predict(awarenessEng_final, type = "response")
awarenessEng_preds

### EMMs ----
#### School ----
awarenessEng_emmeans_School <- emmeans(awarenessEng_final, specs = "School")
awarenessEng_emmeans_School_df <- summary(awarenessEng_emmeans_School, type = "response")
awarenessEng_emmeans_School_df <- awarenessEng_emmeans_School_df |>
  mutate(
    School = c("West Edgecombe\nMiddle School", "Phillips\nMiddle School")
  ) #|>
#   mutate(
#     n = table(CareerAwareness$School),
#     School = paste0(School, "\n(n = ", n, ")")
#   )

#### Grade ----
awarenessEng_emmeans_Grade <- emmeans(awarenessEng_final, specs = "Grade")
awarenessEng_emmeans_Grade_df <- summary(awarenessEng_emmeans_Grade, type = "response")
# awarenessEng_emmeans_Grade_df <- awarenessEng_emmeans_Grade_df |>
#   mutate(
#     n = table(CareerAwareness$Grade),
#     Grade = paste0(Grade, "\n(n = ", n, ")")
#   )

### REPORT ----
#### Table Summary ----
awarenessEng_final_sumTable <- awarenessEng_final |>
  tbl_regression(exponentiate = TRUE,
                 conf.level = 0.95, 
                 add_estimate_to_reference_rows = TRUE, ) |>
  # Stats
  add_global_p(type = 2, test.statistic = "LR",
               keep = TRUE) |> 
  add_n(location = c("level")) |> # add number of obs
  add_significance_stars(pattern = "{p.value}{stars}",
                         thresholds = c(0.001, 0.01, 0.1),
                         hide_ci = FALSE,
                         hide_p = FALSE) |>
  add_glance_source_note(
    include = c(null.deviance, deviance,
                AIC, BIC, nobs)
  ) |>
  #add_vif() |>
  # Format
  bold_p(t = 0.05) |> # now bold q-values under the threshold of 0.10
  #modify_footnote(update = "p.value" ~ "**Bold** < 0.05") |>
  #bold_labels() |>
  italicize_levels() |>
  # Titles
  modify_caption("**Model Summary for Engineering Career Awareness**")

awarenessEng_final_sumTable
show_header_names(awarenessEng_final_sumTable)

##### Save table ----
gtsave(data = awarenessEng_final_sumTable |> as_gt(),
       filename = "Analyses/STEM Career Awareness/STEM Career Awareness Engineer Table.png")



#### Plot ----
##### School ----
awarenessEng_plot_School <- ggplot(data = awarenessEng_emmeans_School_df) + 
  #geom_col(aes(x = School, y = prob, fill = "Engineers")) +
  geom_col(aes(x = School, y = prob, fill = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = School),
                width = 0.25, linewidth = 1) +
  #geom_point(aes(y = prob, x = School)) +
  #geom_text(aes(x = School, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#377EB8")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "School",
                   limits = rev(awarenessEng_emmeans_School_df$School)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    title = "DeSIRE Students' Career Awareness of Engineers by School",
    subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Engineer Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), face = "italic"),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessEng_plot_School

###### Horizontal ----
hcl.colors(3, "Red-Green")
muted("red", l = 70, c = 70)
muted("green", l = 70, c = 70)

awarenessEng_plot_SchoolH <- ggplot(data = awarenessEng_emmeans_School_df) + 
  #geom_col(aes(x = School, y = prob, fill = "Engineers")) +
  geom_col(aes(x = School, y = prob, fill = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = School),
                width = 0.25, linewidth = 1) +
  #geom_point(aes(y = prob, x = School)) +
  #geom_text(aes(x = School, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#377EB8")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "School",
                   limits = rev(awarenessEng_emmeans_School_df$School)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Engineers by School",
    #subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Engineering Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), 
                                 face = "bold.italic",
                                 margin = margin(10,0,3,0)),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessEng_plot_SchoolH

##### Grade ----
awarenessEng_plot_Grade <- ggplot(data = awarenessEng_emmeans_Grade_df) +
  # geom_col(aes(x = Grade, y = prob, fill = "Engineers")) +
  geom_col(aes(x = Grade, y = prob), fill = "#377EB8") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Grade),
                width = 0.5, linewidth = 1) +
  #geom_point(aes(y = prob, x = Grade)) +
  geom_text(aes(x = Grade, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#377EB8")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_y_continuous(limits = c(-0.02,1), breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05)) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Engineering by Grade",
    #subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    y = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )
awarenessEng_plot_Grade

###### Horizontal ----
awarenessEng_plot_GradeH <- ggplot(data = awarenessEng_emmeans_Grade_df) + 
  #geom_col(aes(x = Grade, y = prob, fill = "Engineers")) +
  geom_col(aes(x = Grade, y = prob, fill = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Grade),
                width = 0.25, linewidth = 1) +
  #geom_point(aes(y = prob, x = Grade)) +
  #geom_text(aes(x = School, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#377EB8")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "Grade",
                   limits = rev(awarenessEng_emmeans_School_df$Grade)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Engineers by Grade",
    #subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Engineer Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), 
                                 face = "bold.italic",
                                 margin = margin(10,0,3,0)),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessEng_plot_GradeH

#### Combined ----
awarenessEng_plotH <- 
  awarenessEng_plot_SchoolH +
  awarenessEng_plot_GradeH +
  plot_layout(
    ncol = 1,
    heights = c(2,3),
    guides = "collect",
    axis_titles = "collect_x", 
  ) +
  plot_annotation(
    title = "DeSIRE Students' Career Awareness of Engineering by School and Grade",
    subtitle = "95% Confidence Interval about Expected Marginal Probability of Awareness",
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_text(size = rel(1.3), 
                                #hjust = 0.5,
                                #margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                                ),
      plot.subtitle = element_text(size = rel(1.1), face = "italic",
                                   #hjust = 0.5,
                                   #margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                                   )
    )
  ) &
  theme(
    axis.title.x = element_text(size = rel(1.1)),
    #axis.title.y = element_blank(),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessEng_plotH

### SAVE DATA ----
save(awarenessEng_final,
     awarenessEng_emmeans_School_df,
     awarenessEng_emmeans_Grade_df,
     file = "Analyses/STEM Career Awareness/CareerAwarenessEngineering.RData")


## MATHEMATICIANS ===================
### Mixed Model ----
awarenessMath_M1R <- glmer(Awareness_Q3C ~
                             School
                           + Grade
                           + Gender2
                           + Race2
                           + Gender2:Race2
                           + (1|YearSemester)
                           ,
                           data = CareerAwareness,
                           family = binomial(link = "logit")
)
print(awarenessMath_M1R)
summary(awarenessMath_M1R)
Anova(awarenessMath_M1R, type = 3)
vif(awarenessMath_M1R)
# Random Effect insignificant

drop1(awarenessMath_M1R)
plot(awarenessMath_M1R)

### Fixed Effects ----
#### Full Model ----
awarenessMath_M1 <- glm(Awareness_Q3C ~
                          School
                        + Grade
                        + Gender2
                        + Race2
                        + Gender2:Race2
                        ,
                        data = CareerAwareness,
                        family = binomial(link = "logit")
)
print(awarenessMath_M1)
summary(awarenessMath_M1)
Anova(awarenessMath_M1, type = 3)
vif(awarenessMath_M1)

anova(awarenessMath_M1R,awarenessMath_M1)

#### Reduced Model ----
awarenessMath_M2 <- glm(Awareness_Q3C ~
                          School
                        + Grade
                        + Gender2
                        + Race2
                        #+ Gender2:Race2
                        ,
                        data = CareerAwareness,
                        family = binomial(link = "logit")
)
print(awarenessMath_M2)
summary(awarenessMath_M2)
Anova(awarenessMath_M2, type = 2)

step(awarenessMath_M2)

jtools::summ(awarenessMath_M2, digits = 4)
drop1(awarenessMath_M2, test = "LRT")

### FINAL Model ----
ddply(CareerAwareness, .(School, Grade, Gender2, Race2), summarize,
      mean(Awareness_Q3B))

awarenessMath_final <- glm(Awareness_Q3C ~
                             School
                           #+ Grade
                           + Gender2
                           #+ Race2
                           #+ Gender2:Race2
                           ,
                           data = CareerAwareness,
                           family = binomial(link = "logit")
)
print(awarenessMath_final)
summary(awarenessMath_final)
Anova(awarenessMath_final, type = 2, test.statistic = "LR")

drop1(awarenessMath_final, test = "LRT")
summ(awarenessMath_final, digits = 4, confint = TRUE)

varImp(awarenessMath_final) |> arrange(Overall)

plot(awarenessMath_final)
Conf(awarenessMath_final)

summ(awarenessMath_final, digits = 4, confint = TRUE)

blr_model_fit_stats(awarenessMath_final)
blr_confusion_matrix(awarenessMath_final)
blr_test_hosmer_lemeshow(awarenessMath_final)

#### Plot Coefficients ----
plot_summs(awarenessMath_final, plot.distributions = TRUE, inner_ci_level = .95)

### Predict ----
awarenessMath_preds <- predict(awarenessMath_final, type = "response")
awarenessMath_preds

### EMMs ----
#### School ----
awarenessMath_emmeans_School <- emmeans(awarenessMath_final, specs = "School")
awarenessMath_emmeans_School_df <- summary(awarenessMath_emmeans_School, type = "response")
awarenessMath_emmeans_School_df <- awarenessMath_emmeans_School_df |>
  mutate(
    School = c("West Edgecombe\nMiddle School", "Phillips\nMiddle School")
  ) 

#### Gender ----
awarenessMath_emmeans_Gender <- emmeans(awarenessMath_final, specs = "Gender2")
awarenessMath_emmeans_Gender_df <- summary(awarenessMath_emmeans_Gender, type = "response")
#colnames(awarenessMath_emmeans_Gender_df)[1] <- "Gender"
awarenessMath_emmeans_Gender_df <- awarenessMath_emmeans_Gender_df |>
  mutate(
    n = table(CareerAwareness$Gender2)
  )

### REPORT ----
#### Table Summary ----
awarenessMath_final_sumTable <- awarenessMath_final |>
  tbl_regression(exponentiate = TRUE,
                 conf.level = 0.95, 
                 add_estimate_to_reference_rows = TRUE, ) |>
  # Stats
  add_global_p(type = 2, test.statistic = "LR",
               keep = TRUE) |> 
  add_n(location = c("level")) |> # add number of obs
  add_significance_stars(pattern = "{p.value}{stars}",
                         thresholds = c(0.001, 0.01, 0.1),
                         hide_ci = FALSE,
                         hide_p = FALSE) |>
  add_glance_source_note(
    include = c(null.deviance, deviance,
                AIC, BIC, nobs)
  ) |>
  #add_vif() |>
  # Format
  bold_p(t = 0.05) |> # now bold q-values under the threshold of 0.10
  #modify_footnote(update = "p.value" ~ "**Bold** < 0.05") |>
  #bold_labels() |>
  italicize_levels() |>
  # Titles
  modify_caption("**Model Summary for Mathematicians Career Awareness**")

awarenessMath_final_sumTable
show_header_names(awarenessMath_final_sumTable)

##### Save table ----
gtsave(data = awarenessMath_final_sumTable |> as_gt(),
       filename = "Analyses/STEM Career Awareness/STEM Career Awareness Math Table.png")



#### Plot ----
##### School ----
awarenessMath_plot_School <- ggplot(data = awarenessMath_emmeans_School_df) + 
  geom_col(aes(x = School, y = prob), fill = "#4DAF4A") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = School),
                width = 0.5, linewidth = 1) +
  #geom_point(aes(y = prob, x = School)) +
  geom_text(aes(x = School, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#4DAF4A")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(labels = c("West\nEdgecombe",
                              "Phillips")) +
  scale_y_continuous(limits = c(-0.02,1), breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05)) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Mathematicians by School",
    #subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    y = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )
awarenessMath_plot_School

###### Horizontal ----
hcl.colors(3, "Red-Green")
muted("red", l = 70, c = 70)
muted("green", l = 70, c = 70)

awarenessMath_plot_SchoolH <- ggplot(data = awarenessMath_emmeans_School_df) + 
  #geom_col(aes(x = School, y = prob, fill = "Mathineers")) +
  geom_col(aes(x = School, y = prob, fill = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = School),
                width = 0.25, linewidth = 1) +
  #geom_point(aes(y = prob, x = School)) +
  #geom_text(aes(x = School, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#377EB8")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "School",
                   limits = rev(awarenessMath_emmeans_School_df$School)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Mathineers by School",
    #subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Mathematicians Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), 
                                 face = "bold.italic",
                                 margin = margin(10,0,3,0)),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessMath_plot_SchoolH

##### Gender ----
awarenessMath_plot_Gender <- ggplot(data = awarenessMath_emmeans_Gender_df) +
  #geom_col(aes(x = Gender, y = prob, fill = "Mathematicians")) +
  geom_col(aes(x = Gender, y = prob), fill = "#4DAF4A") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Gender),
                width = 0.5, linewidth = 1) +
  #geom_point(aes(y = prob, x = Gender)) +
  geom_text(aes(x = Gender, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#4DAF4A")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(labels = c("Male",
                              "Not\nMale")) +
  scale_y_continuous(limits = c(-0.02,1), breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05)) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Mathineering by Gender",
    #subtitle = "95% Confidence Interval about Point Estimate Probability of Awareness",
    y = "Probability of Career Awareness"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )
awarenessMath_plot_Gender

###### Horizontal ----
awarenessMath_plot_GenderH <- ggplot(data = awarenessMath_emmeans_Gender_df) + 
  #geom_col(aes(x = Gender, y = prob, fill = "Mathineers")) +
  geom_col(aes(x = Gender2, y = prob, fill = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = Gender2),
                width = 0.25, linewidth = 1) +
  #geom_point(aes(y = prob, x = Gender)) +
  #geom_text(aes(x = School, y = -0.02, label = paste0("n = ", n))) +
  #scale_fill_manual(name = NULL, values = c("#377EB8")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "Gender2",
                   limits = rev(awarenessMath_emmeans_Gender_df$Gender2)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    #title = "DeSIRE Students' Career Awareness of Mathineers by Gender",
    #subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Mathematicians Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), 
                                 face = "bold.italic",
                                 margin = margin(10,0,3,0)),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessMath_plot_GenderH

#### Combined ----
awarenessMath_plotH <- 
  awarenessMath_plot_SchoolH +
  awarenessMath_plot_GenderH +
  plot_layout(
    ncol = 1,
    #heights = c(2,3),
    guides = "collect",
    axis_titles = "collect_x", 
  ) +
  plot_annotation(
    title = "DeSIRE Students' Career Awareness of Mathematicians by School and Gender",
    subtitle = "95% Confidence Interval about Expected Marginal Probability of Awareness",
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_text(size = rel(1.3), 
                                #hjust = 0.5,
                                #margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                                ),
      plot.subtitle = element_text(size = rel(1.1), face = "italic",
                                   #hjust = 0.5,
                                   #margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                                   )
    )
  ) &
  theme(
    axis.title.x = element_text(size = rel(1.1)),
    #axis.title.y = element_blank(),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessMath_plotH

### SAVE DATA ----
save(awarenessMath_final,
     awarenessMath_emmeans_School_df,
     awarenessMath_emmeans_Gender_df,
     file = "Analyses/STEM Career Awareness/CareerAwarenessMath.RData")

## TECHNOLOGISTS ===================
### Mixed Model ----
awarenessTech_M1R <- glmer(Awareness_Q4C ~
                             School
                           + Grade
                           + Gender2
                           + Race2
                           + Gender2:Race2
                           + (1|YearSemester)
                           ,
                           data = CareerAwareness,
                           family = binomial(link = "logit")
)
print(awarenessTech_M1R)
summary(awarenessTech_M1R)
Anova(awarenessTech_M1R, type = 3)
vif(awarenessTech_M1R)
# Random Effect insignificant

drop1(awarenessTech_M1R)

### Fixed Effects ----
#### Full Model ----
awarenessTech_M1 <- glm(Awareness_Q4C ~
                          School
                        + Grade
                        + Gender2
                        + Race2
                        #+ Gender2:Race2
                        ,
                        data = CareerAwareness,
                        family = binomial(link = "logit")
)
print(awarenessTech_M1)
summary(awarenessTech_M1)
Anova(awarenessTech_M1, type = 3)
vif(awarenessTech_M1)

anova(awarenessTech_M1R,awarenessTech_M1)

step(awarenessTech_M1)
varImp(awarenessTech_M1) |> arrange(Overall)

#### Reduced Model ----
awarenessTech_M2 <- glm(Awareness_Q4C ~
                          School
                        #+ Grade
                        #+ Gender2
                        #+ Race2
                        #+ Gender2:Race2
                        ,
                        data = CareerAwareness,
                        family = binomial(link = "logit")
)
print(awarenessTech_M2)
summary(awarenessTech_M2)
Anova(awarenessTech_M2, type = 2, test.statistic = "LR")


jtools::summ(awarenessTech_M2, digits = 4, confint = TRUE)
drop1(awarenessTech_M2, test = "LRT")

### FINAL Model ----
awarenessTech_final <- glm(Awareness_Q4C ~
                             School
                           #+ Grade
                           #+ Gender2
                           #+ Race2
                           #+ Gender2:Race2
                           ,
                           data = CareerAwareness,
                           family = binomial(link = "logit")
)
print(awarenessTech_final)
summary(awarenessTech_final)
Anova(awarenessTech_final, type = 2, test.statistic = "LR")

summ(awarenessTech_final, digits = 4, confint = TRUE)

blr_model_fit_stats(awarenessTech_final)
blr_confusion_matrix(awarenessTech_final)
blr_test_hosmer_lemeshow(awarenessTech_final)

#### Plot Coefficients ----
plot_summs(awarenessTech_final, plot.distributions = TRUE, inner_ci_level = .95)

### Predict ----
awarenessTech_preds <- predict(awarenessTech_final, type = "response")
awarenessTech_preds

### EMMs ----
#### School ----
awarenessTech_emmeans_School <- emmeans(awarenessTech_final, specs = "School")
awarenessTech_emmeans_School_df <- summary(awarenessTech_emmeans_School, type = "response")
awarenessTech_emmeans_School_df <- awarenessTech_emmeans_School_df |>
  mutate(
    School = c("West Edgecombe\nMiddle School", "Phillips\nMiddle School")
  )

### REPORT ----
#### Table Summary ----
awarenessTech_final_sumTable <- awarenessTech_final |>
  tbl_regression(exponentiate = TRUE,
                 conf.level = 0.95, 
                 add_estimate_to_reference_rows = TRUE
                 ) |>
  # Stats
  add_global_p(type = 2, test.statistic = "LR", ) |> # add global p-value
  add_n(location = c("level")) |> # add number of obs
  add_significance_stars(pattern = "{p.value}{stars}",
                         thresholds = c(0.001, 0.01, 0.1),
                         hide_ci = FALSE,
                         hide_p = FALSE) |>
  add_glance_source_note(
    include = c(null.deviance, deviance,
                AIC, BIC, nobs)
  ) |>
  # Format
  bold_p() |> # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.05) |> # now bold q-values under the threshold of 0.10
  #bold_labels() |>
  italicize_levels() |>
  # Titles
  modify_caption("**Model Summary for Technologist Career Awareness**") |>
  as_gt()

awarenessTech_final_sumTable
show_header_names(awarenessTech_final_sumTable)

##### Save table ----
gtsave(data = awarenessTech_final_sumTable,
       filename = "Analyses/STEM Career Awareness/STEM Career Awareness Tech Table.png")

#### Plot ----
##### Gender ----
hcl.colors(3, "Red-Green")
muted("red", l = 70, c = 70)
muted("green", l = 70, c = 70)

# width = 750, height = 300
###### Vertical ----
awarenessTech_plot_School <- ggplot(data = awarenessTech_emmeans_School_df) +
  #geom_col(aes(x = School, y = prob, fill = "Techentists")) +
  geom_col(aes(x = School, y = prob, fill = prob)) +#, fill = "#E41A1C") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, x = School),
                width = 0.25, linewidth = 1) +
  # geom_text(aes(x = rev(School), y = -0.05, label = paste0("n = ", n)),
  #           size = rel(4.5)) +
  #scale_fill_manual(name = NULL, values = c("#E41A1C")) +
  # c("#E41A1C" "#377EB8" "#4DAF4A" "#984EA3")
  scale_x_discrete(name = "School",
                   limits = rev(awarenessTech_emmeans_School_df$School)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1, 0.1), minor_breaks = seq(0,1, 0.05),
                     #expand = expansion(add = c(0.04,0.04))
  ) +
  scale_fill_gradient2(name = NULL, limits = c(0,1), #guide = "colourbar",
                       low = "#F28F8F", mid = "#E2E2E2", high = "#64BF64",
                       midpoint = 0.5) +
  labs(
    title = "DeSIRE Students' Career Awareness of Technologists by School",
    subtitle = "95% Confidence Interval about Expected Probability of Career Awareness",
    y = "Probability of Technologist Career Awareness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "panel",
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1.1), face = "italic"),
    axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1), hjust = 0.5),
    axis.text.x = element_text(size = rel(1.1))
  )
awarenessTech_plot_School

### SAVE DATA ----
save(awarenessTech_final,
     awarenessTech_emmeans_School_df,
     file = "Analyses/STEM Career Awareness/CareerAwarenessTech.RData")


# COMBINE ALL PLOTS ----
## Vertical ----
awareness_combined_plot <- 
  ### Scientists ----
(awarenessSci_plot_Gender + 
   labs(
     title = "Scientists",
     subtitle = "Gender"
   ) +
   theme(
     plot.title = element_text(face = "bold", 
                               hjust = 0.5, 
                               margin = margin(0,0,5,0, unit = "pt")),
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt"))
   )
) + 
  #plot_spacer() +
  ### Engineers ----
(awarenessEng_plot_School +
   labs(
     #title = "Engineers",
     subtitle = "School"
   ) +
   theme(
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt")),
     #axis.title.y = element_blank()
   )
 +
   awarenessEng_plot_Grade +
   labs(
     subtitle = "Grade"
   ) +
   theme(
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt")),
     axis.text.y = element_blank(),
     axis.ticks.y = element_blank()
   )
) +
  #plot_spacer() +
  ### Mathematicians ----
(awarenessMath_plot_School +
   labs(
     subtitle = "School"
   ) +
   theme(
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt"))
   )
) + 
  (awarenessMath_plot_Gender +
     labs(
       subtitle = "Gender"
     ) +
     theme(
       plot.title.position = "panel",
       plot.subtitle = element_text(face = "bold.italic",
                                    margin = margin(0,0,3,0, unit = "pt")),
       axis.text.y = element_blank(),
       axis.ticks.y = element_blank()
     )
  ) + 
  #plot_spacer() +
  ### Technologists ----
(awarenessTech_plot_School +
   labs(
     title = "Technologists",
     subtitle = "School"
   ) +
   theme(
     plot.title = element_text(face = "bold", 
                               hjust = 1, 
                               margin = margin()),
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt"))
   )
) +
  ### Layout ----
plot_layout(
  nrow = 1, 
  widths = c(2,
             #0.25,
             2,
             3,
             #0.25,
             2,
             2,
             #0.25,
             2
  ),
  #guides = "collect",
  #axes = "collect_y",
  axis_titles = "collect_y"
) +
  ### Annotation ----
plot_annotation(
  title = "DeSIRE Students' Career Awareness of STEM Professions",
  #Scientists, Engineers, Mathematicians, and Technologists",
  subtitle = "95% Confidence Interval about Point Estimate Probability of Career Awareness",
  theme = theme(
    #legend.position = "right"
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14, face = "italic"),
    axis.title.y = element_text(size = 14,
                                margin = margin(0,0,0,0, unit = "pt")
    ),
  )
) &
  theme(
    # axis.title.y = element_text(size = 14,
    #                             margin = margin(0,0,0,0, unit = "pt")
    # ),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )
awareness_combined_plot






## Horizontal ----
awareness_combined_plotH <- 
  ### Scientists ----
(awarenessSci_plot_GenderH + 
   labs(
     title = "Scientists",
     subtitle = "Gender"
   ) +
   theme(
     plot.margin = unit(c(10,5,0,0), "pt"),
     plot.title = element_text(face = "bold", 
                               hjust = 0.5, 
                               margin = margin()
     ),
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt")
     ),
     # plot.background = element_rect(
     #   fill = NA,
     #   color = "black",
     #   linewidth = 1
     # ),
     axis.title.y = element_blank()
   )
) + 
  #plot_spacer() +
  ### Engineers ----
(awarenessEng_plot_SchoolH +
   labs(
     title = "Engineers",
     subtitle = "School"
   ) +
   theme(
     plot.margin = unit(c(25,5,0,0), "pt"),
     plot.title = element_text(face = "bold", 
                               hjust = 0.5,
                               margin = margin()
     ),
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt")
     ),
     axis.title.y = element_blank(),
     axis.text.x = element_blank(),
     axis.ticks.x = element_blank()
   )
) +
  (awarenessEng_plot_GradeH +
     labs(
       subtitle = "Grade"
     ) +
     theme(
       plot.margin = unit(c(10,0,0,0), "pt"),
       #plot.title = element_text(face = "bold"),
       plot.title.position = "panel",
       plot.subtitle = element_text(face = "bold.italic",
                                    margin = margin(0,0,3,0, unit = "pt")),
       axis.title.y = element_blank()
     )
  ) +
  #plot_spacer() +
  ### Mathematicians ----
(awarenessMath_plot_SchoolH +
   labs(
     title = "Mathematicians",
     subtitle = "School"
   ) +
   theme(
     plot.margin = unit(c(25,5,0,0), "pt"),
     plot.title = element_text(face = "bold", 
                               hjust = 0.5,
                               margin = margin()
     ),
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt")),
     axis.title.y = element_blank(),
     axis.text.x = element_blank(),
     axis.ticks.x = element_blank()
   )
) + 
  (awarenessMath_plot_GenderH +
     labs(
       subtitle = "Gender"
     ) +
     theme(
       plot.margin = unit(c(10,0,0,0), "pt"),
       #plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(face = "bold.italic",
                                    margin = margin(0,0,3,0, unit = "pt")),
       plot.title.position = "panel",
       axis.title.y = element_blank()
     )
  ) + 
  #plot_spacer() +
  ### Technologists ----
(awarenessTech_plot_SchoolH +
   labs(
     title = "Technologists",
     subtitle = "School"
   ) +
   theme(
     plot.margin = unit(c(25,5,0,0), "pt"),
     plot.title = element_text(face = "bold", 
                               hjust = 0.5,
                               margin = margin()
     ),
     plot.title.position = "panel",
     plot.subtitle = element_text(face = "bold.italic",
                                  margin = margin(0,0,3,0, unit = "pt")),
     axis.title.y = element_blank()
   )
) +
  ### Layout ----
plot_layout(
  ncol = 1, 
  heights = c(2,
              #0.25,
              2,
              3,
              #0.25,
              2,
              2,
              #0.25,
              2
  ),
  #guides = "collect",
  #axes = "collect_x",
  axis_titles = "collect_x"
) +
  ### Annotation ----
plot_annotation(
  title = "DeSIRE Students' Career Awareness of STEM Professions",
  #Scientists, Engineers, Mathematicians, and Technologists",
  subtitle = "95% Confidence Interval about Point Estimate Probability of Career Awareness",
  theme = theme(
    #legend.position = "right"
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14, face = "italic"),
    axis.title.y = element_blank()
  )
) &
  theme(
    axis.title.x = element_text(size = 14,
                                margin = margin(25,0,0,0, unit = "pt")
    ),
    axis.text.y = element_text(size = 10)
  )
awareness_combined_plotH



# SAVE ALL DATA ----
save(CareerAwareness,
     awarenessSci_final,
     awarenessSci_emmeans_df,
     awarenessEng_final,
     awarenessEng_emmeans_School_df,
     awarenessEng_emmeans_Grade_df,
     awarenessMath_final,
     awarenessMath_emmeans_School_df,
     awarenessMath_emmeans_Gender_df,
     awarenessTech_final,
     awarenessTech_emmeans_School_df,
     file = "Analyses/STEM Career Awareness/AllCareerAwareness.RData"
     )



