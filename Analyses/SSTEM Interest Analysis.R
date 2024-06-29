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
Interest <- SSTEMsurvey_data |>
  select(
    Semester,
    YearSemester,
    School,
    Grade,
    Gender,
    Race,
    Race2,
    str_which(colnames(SSTEMsurvey_data), pattern = "Interest_")
  ) |>
  filter(
    Gender %in% c("Male","Female")
  ) |>
  mutate(
    Gender = droplevels(Gender)
  )

hist(Interest$Interest_Q1)
hist(Interest$Interest_Q2)
hist(Interest$Interest_Q3)
hist(Interest$Interest_Q4)
hist(Interest$Interest_Q5)
hist(Interest$Interest_Q6)
hist(Interest$Interest_Q7)
hist(Interest$Interest_Q8)
hist(Interest$Interest_Q9)
hist(Interest$Interest_Q10)
hist(Interest$Interest_Q11)
hist(Interest$Interest_Q12)

shapiro.test(Interest$Interest_Q1)
shapiro.test(Interest$Interest_Q2)
shapiro.test(Interest$Interest_Q3)
shapiro.test(Interest$Interest_Q4)
shapiro.test(Interest$Interest_Q5)
shapiro.test(Interest$Interest_Q6)
shapiro.test(Interest$Interest_Q7)
shapiro.test(Interest$Interest_Q8)
shapiro.test(Interest$Interest_Q9)
shapiro.test(Interest$Interest_Q10)
shapiro.test(Interest$Interest_Q11)
shapiro.test(Interest$Interest_Q12)

# Question 1 =============================================
Q1kruskYearSemester <- kruskal(y = Interest$Interest_Q1, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q1kruskSchool <- kruskal(y = Interest$Interest_Q1, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q1kruskGrade <- kruskal(y = Interest$Interest_Q1, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q1kruskGender <- kruskal(y = Interest$Interest_Q1, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT2
Q1kruskRace <- kruskal(y = Interest$Interest_Q1, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT

# Question 2 =============================================
Q2kruskYearSemester <- kruskal(y = Interest$Interest_Q2, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q2, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q2, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q2, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q2, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 3 =============================================
kruskal(y = Interest$Interest_Q3, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q3, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q3, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q3, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q3, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 4 =============================================
Q4kruskYearSemester <- kruskal(y = Interest$Interest_Q4, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT2
Q4kruskSchool <- kruskal(y = Interest$Interest_Q4, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q4, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) 
kruskal(y = Interest$Interest_Q4, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q4, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 5 =============================================
kruskal(y = Interest$Interest_Q5, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q5kruskSchool <- kruskal(y = Interest$Interest_Q5, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q5, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q5, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q5, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 6 =============================================
kruskal(y = Interest$Interest_Q6, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q6, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q6, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q6, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q6, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 7 =============================================
kruskal(y = Interest$Interest_Q7, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q7, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q7, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q7, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q7kruskRace2 <- kruskal(y = Interest$Interest_Q7, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT2

# Question 8 =============================================
kruskal(y = Interest$Interest_Q8, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q8, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q8, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q8kruskGender <- kruskal(y = Interest$Interest_Q8, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q8, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 9 =============================================
kruskal(y = Interest$Interest_Q9, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q9, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q9, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q9kruskGender <- kruskal(y = Interest$Interest_Q9, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q9, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 10 =============================================
Q10kruskYearSemester <- kruskal(y = Interest$Interest_Q10, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q10, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q10, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q10kruskGender <- kruskal(y = Interest$Interest_Q10, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
kruskal(y = Interest$Interest_Q10, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)

# Question 11 =============================================
kruskal(y = Interest$Interest_Q11, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q11, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q11, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q11kruskGender <- kruskal(y = Interest$Interest_Q11, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
Q11kruskRace2 <- kruskal(y = Interest$Interest_Q11, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT

# Question 12 =============================================
Q12kruskYearSemester <- kruskal(y = Interest$Interest_Q12, 
        trt = Interest$YearSemester, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT2
kruskal(y = Interest$Interest_Q12, 
        trt = Interest$School, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
kruskal(y = Interest$Interest_Q12, 
        trt = Interest$Grade, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE)
Q12kruskGender <- kruskal(y = Interest$Interest_Q12, 
        trt = Interest$Gender, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT
Q12kruskRace2 <- kruskal(y = Interest$Interest_Q12, 
        trt = Interest$Race2, 
        p.adj = "bonferroni",
        group = TRUE,
        console = TRUE) #### SIGNIFICANT2



# Plot ----
interestAreas <- c(
  "Physics",             # Q28_1
  "Environmental Work",   # Q28_2
  "Biology and Zoology",     # Q28_3
  "Veterinary Work",      # Q28_4
  "Mathematics",         # Q28_5
  "Medicine",            # Q28_6
  "Earth Science",        # Q28_7
  "Computer Science",     # Q28_8
  "Medical Science",      # Q28_9
  "Chemistry",           # Q28_10
  "Energy",              # Q28_11
  "Engineering"          # Q28_12
)
Interest_plot_df <- data.frame(Interest)
Interest_plot_df <- Interest_plot_df |>
  select(str_which(colnames(Interest_plot_df), pattern = "Interest_")) |>
  mutate(
    across(everything(), ~factor(.x, levels = c(1,2,3,4), labels = c("Not at all Interested",
                                                                     "Not So Interested",
                                                                     "Interested",
                                                                     "Very Interested")))
  )

colnames(Interest_plot_df) <- interestAreas
Interest_likert_overall <- likert(Interest_plot_df)

plot(Interest_likert_overall, text.size = 3) +
  labs(title = "DeSIRE Students' Interest in STEM Subject Areas") +
  theme(
    plot.title.position = "plot",
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


Qorder <- Interest_likert_overall$results |>
  rowwise() |>
  mutate(
    Positive = sum(c(Interested, `Very Interested`))
  )
Qnames <- Qorder |>
  arrange(desc(Positive)) |>
  pull(Item)
Qnames
#[1] "Interest_Q12" "Interest_Q10" "Interest_Q8"  "Interest_Q2"  "Interest_Q7"  "Interest_Q9"  
#[7] "Interest_Q4"  "Interest_Q3" "Interest_Q6"  "Interest_Q11" "Interest_Q5"  "Interest_Q1" 

## Q12 ----
Interest_plot_df2 <- Interest_plot_df |>
  filter(
    complete.cases(Physics)
  )
likertQ12 <- likert(Interest_plot_df2 |> 
                      select(Physics), 
                    grouping = Interest |> 
                      filter(complete.cases(Interest_Q12)) |>
                      pull(Gender)
                    )
likertQ12_plot_Gender <- plot(likertQ12, group.order = c("Male", "Female", "Other"))+
  #labs(title = "DeSIRE Students' Interest in Physics by Gender") +
  theme(
    plot.title.position = "plot",
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

likertQ12_GenderGroups <- Q12kruskGender$groups |>
  mutate(Gender = factor(rownames(Q12kruskGender$groups), levels = rev(rownames(Q12kruskGender$groups))))
likertQ12_plot_GenderGroups <- ggplot(data = likertQ12_GenderGroups) +
  geom_text(aes(x = "", y = Gender, label = groups)) +
  labs(
    title = "Group"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
likertQ12_plot_GenderGroups

likertQ12_plot_Gender + likertQ12_plot_GenderGroups +
  plot_annotation(
    title = "DeSIRE Students' Interest in Physics by Gender with Pairwise Grouping",
    subtitle = "95% Confidence Simultaneous Multiple Comparison Grouping using Bonferroni adjustment"
  ) +
  plot_layout(nrow = 1,
              widths = c(9,1),
              guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

## Ordinal Regression ----
Interest_plot_df <- Interest_plot_df |>
  select(str_which(colnames(Interest_plot_df), pattern = "Interest_")) |>
  mutate(
    across(everything(), ~factor(.x, levels = c(1,2,3,4), labels = c("Not at all Interested",
                                                                     "Not So Interested",
                                                                     "Interested",
                                                                     "Very Interested")))
  )

