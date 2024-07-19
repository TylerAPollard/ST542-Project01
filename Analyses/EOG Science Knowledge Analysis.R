#### EOG Science Disciplinary Knowledge Analysis 
## Tyler Pollard
## 14 July 2024

## There are three sheets in the file: EOG data for students we have tracked as
### DeSIRE students, EOG data for students that are non-DeSIRE students and have
### not participated in DeSIRE, and a codebook.

## The codebook explains all the variables in the data sheets, however we are
### unclear what some of the variables are for. Please note, all the variables 
### that are unknown currently do not play into the analysis we are aiming to
### do, so you can ignore those for now.

## Students have been assigned a "student number" as to not associate specific
### names with their EOG score. As you'll see, there are a number of students
### that only have either 5th or 8th grade EOG scores. We anticipate that some
### of these students could have left the district which is why they have only
### one score, or could have also not taken an EOG in 5th grade given it being 
### a COVID year. 

## I have included a "dosage" variable in the data that shows how many school 
### years the student has participated in DeSIRE. In the "DeSIRE Students" 
### sheet you'll notice that some students have a dosage of "0". Currently, 
### our table shows that these students were tracked as being a part of the 
### DeSIRE program, but are unconfirmed which school years they have 
### participated in. From an analysis standpoint, it might make more sense to
### include these students as "non-DeSIRE students", however wanted to keep
### their data in the "DeSIRE students" sheet to internally track them as we
### confirm their participation.

## The "School" variable has also been masked from their actual school code,
### but note that those labeled as "999000" and "999001" are elementary schools
### and those labeled as "999002" and "999004" are middle schools.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

# 1. Read EOG Data ----
## DesIRE ----
EOG_DeSIRE_data <- read_excel("Data/EOG/DeSIRE Student EOG Data .xlsx", 
                              sheet = 1)
EOG_DeSIRE_df <- EOG_DeSIRE_data |>
  mutate(
    DeSIRE = "Yes"
  )

## non-DeSIRE ----
EOG_nonDeSIRE_data <- read_excel("Data/EOG/DeSIRE Student EOG Data .xlsx", 
                              sheet = 2)
EOG_nonDeSIRE_df <- EOG_nonDeSIRE_data |>
  mutate(
    DeSIRE = "No"
  )

## Codebook ----
EOG_codebook <- EOG_nonDeSIRE_data <- read_excel("Data/EOG/DeSIRE Student EOG Data .xlsx", 
                                                 sheet = 3)

## Combine Data ----
EOG_data <- bind_rows(
  EOG_DeSIRE_df,
  EOG_nonDeSIRE_df
)

# 2. Clean Data ----
## Full Data ----
EOG_df1 <- EOG_data |>
  mutate(
    School = factor(School),
    Year = factor(Year, ordered = TRUE),
    Grade = factor(grade),
    Ethnicity = factor(ethnic),
    Gender = factor(sex),
    Score = pc_sc_score,
    Level = factor(pc_sc_level, 
                   levels = c("Not Proficient",
                              "Level 3",
                              "Level 4",
                              "Level 5"
                              ),
                   ordered = TRUE),
    Dosage = dosage,
    DeSIRE = factor(DeSIRE)
  ) |>
  select(
    "Student Number",
    "School",
    "Year", 
    "Grade",
    "Gender",
    "Ethnicity",
    "DeSIRE",
    "Dosage",
    "Score",
    "Level"
  ) 

### Sample Size ----
EOG_SampleSizes1 <- sapply(EOG_df1, table)
EOG_SampleSizes1

## Remove missing scores ----
EOG_df2 <- EOG_df1 |>
  filter(
    complete.cases(Score)
  )

which(EOG_df2$Score > 400)

### Sample Size ----
EOG_SampleSizes2 <- sapply(EOG_df2, table)
EOG_SampleSizes2

## Remove scores outside of range ----
EOG_df3 <- EOG_df2 |>
  filter(
    between(Score, 217, 279)
  )

### Sample Size ----
EOG_SampleSizes3 <- sapply(EOG_df3, table)
EOG_SampleSizes3

# Determine number of matched paired students
table(EOG_SampleSizes3$`Student Number`)
which(EOG_SampleSizes3$`Student Number` > 2)










# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# RESEARCH QUESTION 1 =======
## Diciplinary Knowledge ----
### Analyze ----
discKnow_aov <- lm(Score ~ School*Student,
                   data = discKnow_data)
discKnow_aov <- lmer(EOG ~ Student + (1|School),
                     data = discKnow_data)
discKnow_aov <- lm(EOG ~ Student + School,
                   data = discKnow_data)
discKnow_aov <- aov(EOG ~ Student + School,
                    data = discKnow_data)
discKnow_aov
summary(discKnow_aov)
Anova(discKnow_aov, type = 2)
coef(discKnow_aov)

t.test(discKnow_data |> 
         filter(School == "West Edgecombe Middle School") |>
         pull(EOG),
       discKnow_data |> 
         filter(School == "Phillips Middle School") |>
         pull(EOG), var.equal = TRUE)

ddply(discKnow_data, .(School, Student), summarise,
      mean(EOG))
ddply(discKnow_data, .(School), summarise,
      mean(EOG))
ddply(discKnow_data, .(Student), summarise,
      mean(EOG))

plot(discKnow_aov, 1)
shapiro.test(discKnow_aov$residuals)
durbinWatsonTest(discKnow_aov)
ncvTest(discKnow_aov)

Student_emmeans <- emmeans(discKnow_aov, specs = "Student")
Student_emmeans <- data.frame(Student_emmeans)
Student_emmeans <- Student_emmeans |>
  mutate(
    N = ddply(discKnow_data, .(Student), summarise, N = n())$N
  )
School_emmeans <- emmeans(discKnow_aov, specs = "Student", by = "School")
School_emmeans <- data.frame(School_emmeans)
School_emmeans <- School_emmeans |>
  mutate(
    N = ddply(discKnow_data, .(School, Student), summarise, N = n())$N
  )


Student_plot <- ggplot(data = Student_emmeans) +
  geom_errorbarh(aes(y = Student, xmin = lower.CL, xmax = upper.CL)) +
  geom_point(aes(y = Student, x = emmean)) +
  geom_text(aes(x = -4, y = Student, label = paste0("N = ", N)), hjust = 0) +
  scale_x_continuous(limits = c(-5,30), 
                     breaks = seq(0,30,by=5), minor_breaks = seq(0,30,by=2.5),
                     expand = expansion(add = c(0,1))) +
  facet_wrap(vars("Combined")) +
  labs(#title = "End-of-Grade Science Score of DeSIRE vs Non-DeSIRE Students in Phillips Middle School and West Edgecombe Middle School",
    x = "EOG Science Score") +
  theme_bw()
Student_plot

School_plot <- ggplot(data = School_emmeans) +
  geom_errorbarh(aes(y = Student, xmin = lower.CL, xmax = upper.CL, color = Student),
                 show.legend = FALSE) +
  geom_point(aes(y = Student, x = emmean, color = Student),
             show.legend = FALSE) +
  geom_text(aes(x = -4, y = Student, label = paste0("N = ", N)), hjust = 0) +
  scale_x_continuous(limits = c(-5,30), 
                     breaks = seq(0,30,by=5), minor_breaks = seq(0,30,by=2.5),
                     expand = expansion(add = c(0,1))) +
  facet_wrap(vars(School), nrow = 2) +
  labs(#title = "End-of-Grade Science Score of DeSIRE vs Non-DeSIRE\nStudents in Phillips Middle School and West Edgecombe Middle School",
    x = "EOG Science Score") +
  theme_bw()
School_plot

Student_plot + School_plot +
  plot_annotation(
    title = "End-of-Grade Science Score of DeSIRE vs Non-DeSIRE Students in\nPhillips Middle School and West Edgecombe Middle School",
    subtitle = "95% Confidence Intervals about Mean Difference in 8th and 5th Grade EOG Science Score"
  ) +
  plot_layout(ncol = 1, heights = c(1,2),
              guides = "collect",
              axis =  "collect") &
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank()) 











