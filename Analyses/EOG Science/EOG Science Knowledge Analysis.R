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

## Plotting
library(scales)
library(RColorBrewer)
library(patchwork)

## Tables
library(gt)
library(gtsummary)
library(tidybayes)

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
library(performance)

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
    #`Student Number` = factor(`Student Number`),
    School = factor(School),
    Year = factor(Year, ordered = TRUE),
    grade = factor(grade),
    ethnic = factor(ethnic),
    sex = factor(sex),
    Score = pc_sc_score,
    Level = factor(pc_sc_level, 
                   levels = c("Not Proficient",
                              "Level 3",
                              "Level 4",
                              "Level 5"
                   ),
                   ordered = TRUE),
    dosage2 = factor(dosage, ordered = TRUE),
    DeSIRE = factor(DeSIRE),
    DeSIRE2 = factor(ifelse(dosage == 0, "No", "Yes"))
  ) |>
  select(
    "Student Number",
    "School",
    "Year", 
    "grade",
    "sex",
    "ethnic",
    "DeSIRE",
    "DeSIRE2",
    "dosage",
    "dosage2",
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
which(EOG_df3$`Student Number` %in% c(272, 486))
EOG_df3 |> slice(which(EOG_df3$`Student Number` %in% c(272, 486)))

## Assign this different multiple as new student ----
max(EOG_df3$`Student Number`)
EOG_df4 <- EOG_df3
EOG_df4$`Student Number`[406] <- max(EOG_df4$`Student Number`) + 1
EOG_df4$`Student Number`[578] <- max(EOG_df4$`Student Number`) + 1

### Sample Size ----
EOG_SampleSizes4 <- sapply(EOG_df4, table)
EOG_SampleSizes4

EOG_df4 |> filter(dosage == 3)

table(EOG_df4$School, EOG_df4$DeSIRE2)
table(EOG_df4$School, EOG_df4$grade)
table(EOG_df4$grade, EOG_df4$DeSIRE2)

## Change 5th grade to 0 dosage ----
EOG_df5 <- EOG_df4 |>
  filter(
    !(School %in% c("999001", "999002") & DeSIRE2 == "Yes")
  ) |>
  mutate(
    dosageB = ifelse(grade == 5, 0, dosage),
    dosage2B = factor(dosageB)
  )

### Sample Size ----
EOG_SampleSizes5 <- sapply(EOG_df5, table)
EOG_SampleSizes5

EOG_df5 |> filter(dosage == 3)

table(EOG_df5$School, EOG_df5$DeSIRE2)
table(EOG_df5$School, EOG_df5$grade)
table(EOG_df5$grade, EOG_df5$DeSIRE2)


# 3. Examine Matched Structure ----
StudentSchool <- matrix(table(EOG_df5$`Student Number`, EOG_df5$School),ncol = 4)
StudentSchool <- data.frame(StudentSchool)
colnames(StudentSchool) <- levels(EOG_df5$School)
StudentSchool
Student_pairs <- rowSums(StudentSchool)
StudentSchool_pairs <- StudentSchool |> 
  filter(Student_pairs == 2)

sum(StudentSchool_pairs$`999000` == StudentSchool_pairs$`999001`)
sum(StudentSchool_pairs$`999000` == StudentSchool_pairs$`999002`)
sum(StudentSchool_pairs$`999000` == StudentSchool_pairs$`999004`)

sum(StudentSchool_pairs$`999001` == StudentSchool_pairs$`999002`)
sum(StudentSchool_pairs$`999001` == StudentSchool_pairs$`999004`)

sum(StudentSchool_pairs$`999002` == StudentSchool_pairs$`999004`)

sum((StudentSchool_pairs$`999000` == StudentSchool_pairs$`999004`) &
      (StudentSchool_pairs$`999000` == StudentSchool_pairs$`999004`))


# 4. Filter data to matched design ----
student_pairs <- which(duplicated(EOG_df5$`Student Number`))
student_pairs2 <- ddply(EOG_df5, .(`Student Number`, sex, ethnic), summarize,
                        n = n())
student_pairs
student_pairs2
student_pairs3 <- student_pairs2 |>
  filter(n == 2) |>
  pull(`Student Number`)
student_pairs3

EOG_df6 <- EOG_df5 |> filter(`Student Number` %in% student_pairs3)
EOG_df6_NOT <- EOG_df5 |> filter(!(`Student Number` %in% student_pairs3))

str(EOG_df6)

## Sample Size ----
EOG_SampleSizes6 <- sapply(EOG_df6, table)
EOG_SampleSizes6

EOG_SampleSizes6_NOT <- sapply(EOG_df6_NOT, table)
EOG_SampleSizes6_NOT

## Make wider
EOG_df7 <- EOG_df6 
# mutate(
#   Year = as.character(Year),
#   School = as.character(School),
#   Level = as.character(Level)
# )
str(EOG_df7)

EOG7_dps <- which(duplicated(EOG_df7 |> select(`Student Number`, grade)))
EOG_df7 <- EOG_df7 |> slice(-c(EOG7_dps, EOG7_dps-1))

EOGnonpaired_df <- EOG_df7
EOGpaired_df <- EOG_df7 |>
  select(-dosageB, -dosage2B) |>
  pivot_wider(names_from = grade,
              values_from = c(Year, School, Score, Level))

str(EOGpaired_df)

# 5. Likert Plot ----
EOGpair_likert <- likert(
  items = data.frame(EOGpaired_df |> select(Level_5, Level_8)),
  grouping = EOGpaired_df |> pull(dosage2)
)
EOGpair_likert

EOGpair_likert2 <- likert(
  items = data.frame(EOGpaired_df |> select(Level_8)),
  grouping = EOGpaired_df |> pull(Level_5)
)
EOGpair_likert2

plot(EOGpair_likert2, centered = FALSE, ordered = FALSE) +
  scale_y_continuous(transform = "reverse")

# 6. Paired Ordinal Probit Regerssion on Paired data ----
## Continuous Dosage ----
brmpair1 <- brm(Level_8 ~ sex + ethnic + dosage + Level_5,
                data = EOGpaired_df, 
                family = cumulative(link = "probit"), 
                save_pars = save_pars(all = TRUE),
                seed = 52, 
                control = list(adapt_delta = 0.95)
)

brmpair1

conditional_effects(brmpair1, categorical = TRUE)
condPairs1 <- conditional_effects(brmpair1, categorical = TRUE, effects = "dosage")
condPairs1_df <- condPairs1$`dosage:cats__`
condPairs1

brmpair1B <- brm(Level_8 ~ sex + ethnic + dosage + Level_5 + dosage:Level_5,
                 data = EOGpaired_df, 
                 family = cumulative(link = "probit"), 
                 save_pars = save_pars(all = TRUE),
                 seed = 52, 
                 control = list(adapt_delta = 0.95)
)

print(brmpair1B, digits = 4)
condPairs1B <- conditional_effects(brmpair1B, categorical = TRUE, effects = "dosage")
condPairs1B_df <- condPairs1$`dosage:cats__`
condPairs1B

bayes_factor(brmpair1B, brmpair1)

brmpair1C <- brm(Level_8 ~ sex + ethnic + dosage + Level_5 + dosage:Level_5 + School_8,
                 data = EOGpaired_df, 
                 family = cumulative(link = "probit"), 
                 save_pars = save_pars(all = TRUE),
                 seed = 52, 
                 control = list(adapt_delta = 0.95)
)

brmpair1C
condPairs1C <- conditional_effects(brmpair1C, categorical = TRUE, effects = "dosage")
condPairs1C_df <- condPairs1C$`dosage:cats__`
condPairs1C

brmpair1D <- brm(Level_8 ~ ethnic + dosage + Level_5 + dosage:Level_5,
                 data = EOGpaired_df, 
                 family = cumulative(link = "probit"), 
                 save_pars = save_pars(all = TRUE),
                 seed = 52, 
                 control = list(adapt_delta = 0.95)
)

brmpair1D
condPairs1D <- conditional_effects(brmpair1D, categorical = TRUE, effects = "dosage")
condPairs1D_df <- condPairs1C$`dosage:cats__`
condPairs1D


## Categorical Dosage ----
brmpair2 <- brm(Level_8 ~ sex + ethnic + dosage2 + Level_5,
                data = EOGpaired_df, 
                family = cumulative(link = "probit"), 
                save_pars = save_pars(all = TRUE),
                seed = 52, 
                control = list(adapt_delta = 0.95)
)

brmpair2
conditional_effects(brmpair2, categorical = TRUE)
conditional_effects(brmpair2, categorical = TRUE, effects = "dosage2")

bayes_factor(brmpair2, brmpair1)


brmpair2B <- brm(Level_8 ~ sex + ethnic + dosage2 + Level_5 + dosage2:Level_5,
                 data = EOGpaired_df, 
                 family = cumulative(link = "probit"), 
                 save_pars = save_pars(all = TRUE),
                 seed = 52, 
                 control = list(adapt_delta = 0.95)
)

brmpair2B
conditional_effects(brmpair2B, categorical = TRUE)
conditional_effects(brmpair2B, categorical = TRUE, effects = "dosage2")

bayes_factor(brmpair2B, brmpair2)

brmpair2C <- brm(Level_8 ~ sex + ethnic + dosage2 + Level_5 + dosage2:Level_5 + School_8,
                 data = EOGpaired_df, 
                 family = cumulative(link = "probit"), 
                 save_pars = save_pars(all = TRUE),
                 seed = 52, 
                 iter = 5000,
                 chains = 4,
                 control = list(adapt_delta = 0.95)
)

brmpair2C
bayes_factor(brmpair2C, brmpair2)
bayes_factor(brmpair2C, brmpair2B)


conditional_effects(brmpair2C, categorical = TRUE)
conditional_effects(brmpair2C, categorical = TRUE, effects = "dosage2")


conditions2C <- make_conditions(EOGpaired_df, vars = c("Level_5"))
conditions2C <- conditions2C |>
  filter(Level_5 != "Level 5") |>
  mutate(Level_5 = droplevels(Level_5))
conditional_effects(brmpair2C, categorical = TRUE, effects = "dosage2",
                    conditions = conditions2C,
                    prob = 0.95)


brmpairC_preds <- posterior_predict(brmpair2C)
brmpairC_preds


conditions2C2 <- make_conditions(EOGpaired_df, vars = c("Level_5", "School_8"))
conditions2C2 <- conditions2C2 |>
  filter(Level_5 != "Level 5") |>
  mutate(Level_5 = droplevels(Level_5)) |>
  filter(School_8 %in% c("999002", "999004"))|>
  mutate(School_8 = droplevels(School_8))



condpairs2C <- conditional_effects(brmpair2C, categorical = TRUE, effects = "dosage2",
                                   conditions = conditions2C2,
                                   prob = 0.95)
condpairs2C
condpairs2C_df <- condpairs2C$`dosage2:cats__`



brmpair2D <- brm(Level_8 ~ ethnic + dosage2 + Level_5 + dosage2:Level_5,
                 data = EOGpaired_df, 
                 family = cumulative(link = "probit"), 
                 save_pars = save_pars(all = TRUE),
                 seed = 52, 
                 control = list(adapt_delta = 0.95)
)

brmpair2D
bayes_factor(brmpair2D, brmpair2)
bayes_factor(brmpair2D, brmpair2B)
bayes_factor(brmpair2D, brmpair2C)
bayes_factor(brmpair1D, brmpair2D)


conditional_effects(brmpair2D, categorical = TRUE)
conditional_effects(brmpair2D, effects = "dosage2:Level_5")


conditions2D <- make_conditions(EOGpaired_df, vars = c("Level_5"))
conditions2D <- conditions2D |>
  filter(Level_5 != "Level 5") |>
  mutate(Level_5 = droplevels(Level_5))
conditional_effects(brmpair2D, categorical = TRUE, effects = "dosage2",
                    conditions = conditions2D,
                    prob = 0.95)


brmpairC_preds <- posterior_predict(brmpair2D)
brmpairC_preds


conditions2D2 <- make_conditions(EOGpaired_df, vars = c("Level_5", "School_8"))
conditions2D2 <- conditions2D2 |>
  filter(Level_5 != "Level 5") |>
  mutate(Level_5 = droplevels(Level_5)) |>
  filter(School_8 %in% c("999002", "999004"))|>
  mutate(School_8 = droplevels(School_8))



condpairs2D <- conditional_effects(brmpair2D, categorical = TRUE, effects = "dosage2",
                                   conditions = conditions2D2,
                                   prob = 0.95)
condpairs2D
condpairs2D_df <- condpairs2D$`dosage2:cats__`


# 7. Unpaired Ordinal Probit Regression Model ----
EOG_df <- EOG_df5 |>
  mutate(
    StudentNumber = factor(`Student Number`)
  ) |>
  select(StudentNumber, everything(), -"Student Number")

str(EOG_df)
describe(EOG_df)
describeBy(EOG_df, group = c("DeSIRE2"))
describeBy(EOG_df, group = c("grade", "DeSIRE2"))

t <- table(EOG_df$School, EOG_df$Level)
t
rowSums(t)

round(t/nrow(EOG_df),4)

table(EOG_df$School, EOG_df$DeSIRE2)
table(EOG_df$School, EOG_df$grade)
table(EOG_df$grade, EOG_df$DeSIRE2)

t2 <- apply(t, 2, function(x){x/rowSums(t)})

t2

ggplot(data = EOG_df) +
  geom_bar(aes(x = Level, y = after_stat(count/sum(count)), fill = grade),
           position = position_dodge()) +
  scale_y_continuous(labels = percent, name = "Percent of All Students", n.breaks = 8) +
  labs(x = "EOG Science Level") +
  facet_wrap(vars(DeSIRE2))

ggplot(data = EOG_df) +
  geom_bar(aes(x = DeSIRE2, y = after_stat(count/sum(count)), fill = grade),
           position = position_dodge()) +
  scale_y_continuous(labels = percent, name = "Percent of All Students", n.breaks = 8) +
  labs(x = "EOG Science Level") +
  facet_wrap(vars(Level), nrow = 1)

ggplot(data = EOG_df) +
  geom_bar(aes(x = Level, y = after_stat(count/sum(count)), fill = DeSIRE2),
           position = position_dodge()) +
  facet_wrap(vars(School))


brmNULL <- brm(Level ~ 1 + (1|StudentNumber),
               data = EOG_df, 
               family = cumulative(link = "probit"), 
               save_pars = save_pars(all = TRUE),
               seed = 52
)
performance::variance_decomposition(brmNULL)
print(brmNULL)
summary(brmNULL)
prior_summary(brmNULL)
postsumNull <- posterior_summary(brmNULL)

VarCorr(brmNULL)


brm1 <- brm(Level ~ grade + sex + ethnic + dosage + (1|gr(School, by = grade)) + (1|StudentNumber),
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm1)
summary(brm1)
prior_summary(brm1)
posterior_summary(brm1)
plot(brm1)

bayes_factor(brm1, brmNULL)

conditional_effects(brm1, categorical = TRUE, effects = "dosage")

pp_check(brm4, ndraws = 100, type = "xyz")
pp_check(brm4, ndraws = 100, type = "bars")
pp_check(brm5, ndraws = 100, type = "bars")
pp_check(brm5, ndraws = 100, type = "bars_grouped", group = "dosage")
brm5Cond <- conditional_effects(brm5, effects = "dosage", 
                                categorical = TRUE,
                                conditions = data.frame("grade" = c("5", "8")))
brm5Cond_df <- brm5Cond$`dosage:cats__`
brm5Cond

dosageEMM <- emmeans(brm5, specs = "dosage")
dosageEMMsum <- summary(dosageEMM, type = "response")
dosageEMMsum

gradeEMM <- emmeans(brm5, specs = "grade")
gradeEMMsum <- summary(gradeEMM, type = "response")
gradeEMMsum

preds <- post_prob(brm5)

pairs(brm5)

brm6 <- brm(Level ~ grade + sex + ethnic + dosage + (1|gr(School, by = grade)) + (1|StudentNumber),
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm6)
summary(brm6)
prior_summary(brm6)
posterior_summary(brm6)

bayes_factor(brm5, brm6)

conditional_effects(brm6, categorical = TRUE)


brm7 <- brm(Level ~ grade + (1|gr(School, by = grade)) + sex + ethnic + dosage2,
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm7)
summary(brm7)
prior_summary(brm7)
posterior_summary(brm7)
plot(brm7)

bayes_factor(brm5, brm7)

pp_check(brm4, ndraws = 100, type = "xyz")
pp_check(brm4, ndraws = 100, type = "bars")
pp_check(brm7, ndraws = 100, type = "bars")
ppc7 <- pp_check(brm7, ndraws = 100, type = "bars_grouped", group = "dosage2")
ppc_data <- ppc7$data

pp_check(brm7, ndraws = 100, type = "bars_grouped", 
         group = c("grade", "dosage2"))
conditional_effects(brm7, categorical = TRUE)

pp7_preds <- posterior_predict(brm7)


conditional_effects(brm7, categorical = TRUE, effects = "dosage2")
brm7Cond <- conditional_effects(brm7, effects = "dosage2", 
                                #categorical = TRUE,
                                conditions = data.frame("grade" = c("8")))
brm7Cond_df <- brm7Cond$`dosage:cats__`
brm7Cond

dosageEMM <- emmeans(brm7, specs = "dosage")
dosageEMMsum <- summary(dosageEMM, type = "response")
dosageEMMsum

gradeEMM <- emmeans(brm7, specs = "grade")
gradeEMMsum <- summary(gradeEMM, type = "response")
gradeEMMsum


brm8 <- brm(Level ~ grade + sex + ethnic + dosage + (1|StudentNumber),
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)

print(brm8)
bayes_factor(brm5, brm8)

conditional_effects(brm8, categorical = TRUE)
conditional_effects(brm8, categorical = TRUE, effects = "dosage")

##### This Plot
brm8_plot <- conditional_effects(brm8, categorical = TRUE, effects = "dosage",
                                 conditions = data.frame(grade = "8"))
brm8_plotdf <- brm8_plot$`dosage:cats__`

brm9<- brm(Level ~ grade + sex + ethnic + dosage2 + (1|School) + (1|StudentNumber),
           data = EOG_df, 
           family = cumulative(link = "probit"), 
           save_pars = save_pars(all = TRUE),
           seed = 52, 
           control = list(adapt_delta = 0.95)
)

print(brm9)
bayes_factor(brm5, brm9)
conditional_effects(brm5, categorical = TRUE, effects = "dosage",
                    conditions = data.frame(grade = "8"))
conditional_effects(brm9, categorical = TRUE, effects = "dosage2",
                    conditions = data.frame(grade = "8"))


table(EOG_df$dosage2, EOG_df$Level)

brm10 <- brm(Level ~ grade + sex + ethnic + dosage2 + (1|StudentNumber),
             data = EOG_df, 
             family = cumulative(link = "probit"), 
             save_pars = save_pars(all = TRUE),
             seed = 52, 
             control = list(adapt_delta = 0.95)
)

print(brm10)
bayes_factor(brm8, brm10)
t <- conditional_effects(brm10, categorical = TRUE, effects = "dosage2",
                         conditions = data.frame(grade = "8"))
t2 <- t$`dosage2:cats__`
t

et <- emmeans(brm10, specs = "dosage2")
et_sum <- summary(et, type = "response")


preds <- posterior_predict(brm10)


# 8. FINAL MODEL ========================================================
## Final Model ----
# brmpairFinal <- brm(Level_8 ~ ethnic + dosage2 + Level_5 + dosage2:Level_5,
#                     data = EOGpaired_df, 
#                     family = cumulative(link = "probit"), 
#                     save_pars = save_pars(all = TRUE),
#                     seed = 52, 
#                     iter = 1000,
#                     control = list(adapt_delta = 0.95),
#                     backend = "cmdstanr"
# )
# 
# save(brmpairFinal, 
#      EOGnonpaired_df,
#      EOGpaired_df,
#      file = "Analyses/EOG Science/PairedOrdinalFit.RData")

## Load Data and Model ----
load(file = "Analyses/EOG Science/PairedOrdinalFit.RData")

brmpairFinal
loo(brmpairFinal)
waic(brmpairFinal)
bayes_R2(brmpairFinal)
performance(brmpairFinal)

tidy(brmpairFinal)

pp_check(brmpairFinal, ndraws = 100, type = "bars")

## Create Output Plot ----
conditional_effects(brmpairFinal, categorical = TRUE)
condPlotCont <- conditional_effects(brmpairFinal, effects = "dosage2:Level_5")
condPlotCont_df <- condPlotCont$`dosage2:Level_5`
condPlotCont

conditionsFinal <- make_conditions(EOGpaired_df, vars = c("Level_5"))
conditionsFinal <- conditionsFinal |>
  filter(Level_5 != "Level 5") |>
  mutate(Level_5 = droplevels(Level_5))
condPlotFac <- conditional_effects(brmpairFinal, categorical = TRUE, 
                                   effects = "dosage2",
                                   conditions = conditionsFinal, 
                                   method = "posterior_epred",
                                   re_formula = NA,
                                   prob = 0.95)
condPlotFac
condPlotFac_df <- condPlotFac$`dosage2:cats__`
condPlotFac_df
condPlotFac_df2 <- condPlotFac_df |>
  select(
    dosage2, Level_5, effect2__, estimate__, se__, lower__, upper__
  ) |>
  group_by(dosage2, Level_5, effect2__) |>
  summarise(
    Probability = mean(estimate__),
    Lower = mean(lower__),
    Upper = mean(upper__)
  ) |>
  rename(
    "Dosage" = dosage2,
    "Grade 5 Level" = Level_5,
    "Grade 8 Level" = effect2__
  )
condPlotFac_df2
writexl::write_xlsx(condPlotFac_df2, "Analyses/EOG Science/Conditional Probs for EOG Science.xlsx")

muted("red", l = 70, c = 70)
"#F28F8F"
muted("gold", l = 70, c = 70)
"#C6A93A"
muted("green", l = 70, c = 70)
"#64BF64"
muted("blue", l = 50, c = 50)
"#7171AD"

brewer.pal(9, "Reds")
"#FFF5F0" "#FEE0D2" "#FCBBA1" "#FC9272" "#FB6A4A" "#EF3B2C" "#CB181D" "#A50F15" "#67000D"

brewer.pal(9, "Oranges")
"#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#A63603" "#7F2704"

brewer.pal(4, "Greens")
"#EDF8E9" "#BAE4B3" "#74C476" "#238B45"

brewer.pal(4, "Blues")
"#EFF3FF" "#BDD7E7" "#6BAED6" "#2171B5"

ggplot(data = condPlotFac_df2) +
  # geom_col(aes(x = Dosage, y = Probability, fill = Grade8Level),
  #          position = position_dodge()) +
  # geom_errorbar(aes(x = Dosage, ymin = Lower, ymax = Upper,
  #                   group = `Grade 8 Level`, color = `Grade 8 Level`),
  #               position = position_dodge(width = 0.5),
  #               width = 0.5, alpha = 0.5) +
  geom_point(aes(x = Dosage, y = Probability, color = `Grade 8 Level`),
             position = position_dodge(width = 0.5),
             size = 2) +
  geom_line(aes(x = Dosage, y = Probability, group = `Grade 8 Level`, color = `Grade 8 Level`),
            position = position_dodge(width = 0.5), linewidth = 1) +
  # geom_ribbon(aes(x = Dosage, ymin = Lower, ymax = Upper, 
  #                 group = Grade8Level, fill = Grade8Level),
  #             alpha = 0.3) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(values = c("#F28F8F", "gold2", "#74C476","#6BAED6")) +
  labs(
    title = "Probability of Score Level on 8th Grade EOG given 5th Grade Level and Dosage",
    subtitle = "95% Credible Interval about Expected Probability",
    y = "Probability of 8th Grade Level",
    x = "Number of Years in DeSIRE Program"
  ) +
  facet_wrap(vars(`Grade 5 Level`), nrow = 1, labeller = label_both) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", size = 14),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


ggplot(data = condPlotFac_df2) +
  # geom_col(aes(x = Dosage, y = Probability, fill = Grade8Level),
  #          position = position_dodge()) +
  # geom_errorbar(aes(x = Dosage, ymin = Lower, ymax = Upper,
  #                   group = `Grade 8 Level`, color = `Grade 8 Level`),
  #               position = position_dodge(width = 0.5),
  #               width = 0.5, alpha = 0.5) +
  geom_col(aes(x = Probability, y = `Grade 8 Level`, fill = Dosage),
             position = position_dodge(),
             size = 2) +
  # geom_line(aes(x = Probability, y = `Grade 8 Level`, group = Dosage, color = Dosage),
  #           position = position_dodge(width = 0.5), linewidth = 1) +
  # geom_ribbon(aes(x = Dosage, ymin = Lower, ymax = Upper, 
  #                 group = Grade8Level, fill = Grade8Level),
  #             alpha = 0.3) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_fill_manual(values = c("#74C476", "gold2", "#F28F8F"),
                    breaks = c("2", "1", "0"))+
  labs(
    title = "Probability of Score Level on 8th Grade EOG given 5th Grade Level and Dosage",
    subtitle = "95% Credible Interval about Expected Probability",
    y = "8th Grade Level",
    x = "Probability of Scoring in 8th Grade Level"
  ) +
  facet_wrap(vars(`Grade 5 Level`), nrow = 1, labeller = label_both) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", size = 14),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


ggplot(data = condPlotFac_df2) +
  # geom_col(aes(x = Dosage, y = Probability, fill = Grade8Level),
  #          position = position_dodge()) +
  # geom_errorbar(aes(x = Dosage, ymin = Lower, ymax = Upper,
  #                   group = `Grade 8 Level`, color = `Grade 8 Level`),
  #               position = position_dodge(width = 0.5),
  #               width = 0.5, alpha = 0.5) +
  geom_col(aes(x = Probability, y = `Grade 8 Level`, fill = `Grade 5 Level`),
           position = position_dodge(),
           size = 2) +
  # geom_line(aes(x = Probability, y = `Grade 8 Level`, group = Dosage, color = Dosage),
  #           position = position_dodge(width = 0.5), linewidth = 1) +
  # geom_ribbon(aes(x = Dosage, ymin = Lower, ymax = Upper, 
  #                 group = Grade8Level, fill = Grade8Level),
  #             alpha = 0.3) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_fill_manual(values = c("#74C476", "gold2", "#F28F8F"),
                    breaks = c("Level 4", "Level 3", "Not Proficient"))+
  labs(
    title = "Probability of Score Level on 8th Grade EOG given 5th Grade Level and Dosage",
    subtitle = "95% Credible Interval about Expected Probability",
    y = "8th Grade Level",
    x = "Probability of Scoring in 8th Grade Level"
  ) +
  facet_wrap(vars(Dosage), nrow = 1, labeller = label_both) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", size = 14),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )




emmeans(brmpairFinal, specs = c("dosage2", "Level_5"), type = "response")

newdataobs <- expand.grid(
  dosage2 = c("0","1","2"),
  Level_5 = c('Not Proficient', 'Level 3', 'Level 4'),
  ethnic = unique(EOGpaired_df$ethnic)
)
brmFinal_epreds <- posterior_epred(brmpairFinal, newdata = newdataobs)
tidy_epred <- brmpairFinal |>
  epred_draws(newdata = newdataobs)

tidy_epred2 <- tidy_epred |>
  group_by(dosage2, Level_5, .category) |>
  summarise(
    Prob = mean(.epred),
    LCB = quantile(.epred, 0.025),
    UCB = quantile(.epred, 0.975)
  )
tidy_epred2

tidy_epred3 <- tidy_epred |>
  group_by(dosage2, Level_5, .category) |>
  point_interval(.epred, .interval = qi) |>
  rename(
    "Dosage" = dosage2,
    "Grade5Level" = Level_5,
    "Grade8Level" = .category,
    "Probability" = .epred,
    "LCB" = .lower,
    "UCB" = .upper
  )

tidy_epred3




## Predictions ----
finalPreds <- posterior_predict(brmpairFinal)
finalPreds2 <- t(finalPreds)



finalPreds3 <- cbind(
  EOGpaired_df |> select(ethnic, dosage2, Level_5),
  finalPreds2
)

finalProbsDosage <- finalPreds3 |>
  select(-ethnic,-dosage2, -Level_5) |>
  summarise(n = ~count(everything(.x)))


count1 <- apply(finalPreds2, 1, function(x){sum(x == 1)})
count2 <- apply(finalPreds2, 1, function(x){sum(x == 2)})
count3 <- apply(finalPreds2, 1, function(x){sum(x == 3)})
count4 <- apply(finalPreds2, 1, function(x){sum(x == 4)})


finalPreds3 <- EOGpaired_df |> select(ethnic, dosage2, Level_5) |>
  mutate(
    Count1 = count1,
    Count2 = count2,
    Count3 = count3,
    Count4 = count4
  )

finalPreds4 <- ddply(finalPreds3, .(dosage2, Level_5), summarise,
                     Count1 = sum(Count1),
                     Count2 = sum(Count2),
                     Count3 = sum(Count3),
                     Count4 = sum(Count4))

sum(finalPreds3$Count1)/(102*10000)
sum(finalPreds3$Count2)/(102*10000)
sum(finalPreds3$Count3)/(102*10000)
sum(finalPreds3$Count4)/(102*10000)

library(tidybayes)
library(modelr)

EOGpaired_df$dosage2 <- droplevels(EOGpaired_df$dosage2)
EOGpaired_df$Level_5 <- droplevels(EOGpaired_df$Level_5)

EOG_plot <- EOGpaired_df %>%
  data_grid(dosage2, ethnic, Level_5) %>%
  add_epred_draws(brmpairFinal, category = "Level_8") %>%
  ggplot(aes(x = .epred, y = Level_8, color = dosage2)) +
  #coord_cartesian(expand = FALSE) +
  facet_grid(. ~ Level_5, switch = "x") +
  theme_bw() +
  #theme(strip.background = element_blank(), strip.placement = "outside") +
  labs(
    title = "Probability of Score Level on 8th Grade EOG given 5th Grade Level and Dosage",
    subtitle = "95% Credible Interval about Expected Probability",
    y = "Probability of 8th Grade Level",
    x = "Number of Years in DeSIRE Program"
  )

EOG_plot +
  stat_summary(fun = median, geom = "bar",
               aes(fill = dosage2),
               position = position_dodge(width = .95)) 
  # stat_pointinterval(
  #   aes(group = dosage2),
  #   position = position_dodge(), .width = 0.95, 
  #   color = "black", 
  #   )







