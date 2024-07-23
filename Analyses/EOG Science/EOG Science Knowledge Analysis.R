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

# 3. Examine Matched Structure ----
StudentSchool <- matrix(table(EOG_df4$`Student Number`, EOG_df4$School),ncol = 4)
StudentSchool <- data.frame(StudentSchool)
colnames(StudentSchool) <- levels(EOG_df4$School)
pairs <- rowSums(StudentSchool)
StudentSchool_pairs <- StudentSchool |> 
  filter(pairs == 2)

sum(StudentSchool_pairs$`999000` == StudentSchool_pairs$`999001`)
sum(StudentSchool_pairs$`999000` == StudentSchool_pairs$`999002`)
sum(StudentSchool_pairs$`999000` == StudentSchool_pairs$`999004`)

sum(StudentSchool_pairs$`999001` == StudentSchool_pairs$`999002`)
sum(StudentSchool_pairs$`999001` == StudentSchool_pairs$`999004`)

sum(StudentSchool_pairs$`999002` == StudentSchool_pairs$`999004`)

sum((StudentSchool_pairs$`999000` == StudentSchool_pairs$`999004`) &
      (StudentSchool_pairs$`999000` == StudentSchool_pairs$`999004`))

# 4. Filter data to matched design
student_pairs <- which(duplicated(EOG_df4$`Student Number`))
student_index <- sort(c(student_pairs, student_pairs-1))
student_index2 <- row.names(EOG_df4) %in% as.character(student_index)
EOG_df5 <- EOG_df4 |> filter(student_index2)
EOG_df5_NOT <- EOG_df4 |> filter(!student_index2)

## Sample Size ----
EOG_SampleSizes5 <- sapply(EOG_df5, table)
EOG_SampleSizes5


EOG_SampleSizes5_NOT <- sapply(EOG_df5_NOT, table)
EOG_SampleSizes5_NOT

EOG_df4 |> filter(dosage == 3)

table(EOG_df4$School, EOG_df4$DeSIRE2)
table(EOG_df4$School, EOG_df4$grade)
table(EOG_df4$grade, EOG_df4$DeSIRE2)

EOG_df4 |>
  slice(
    which(EOG_df4$School %in% c("999001", "999002") & EOG_df4$DeSIRE == "Yes")
)

EOG_df6 <- EOG_df4 |>
  filter(
    !(School %in% c("999001", "999002") & DeSIRE == "Yes" & DeSIRE2 == "Yes")
  )

# 4. Ordinal Probit Regression Model ----
EOG_df <- EOG_df6 |>
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


lm1 <- lm(Level ~ grade + DeSIRE2,
          data = EOG_df)
summary(lm1)


polrNULL <- polr(Level ~ 1|StudentNumber,
                 data = EOG_df,
                 method = "probit",
                 Hess = TRUE)
summary(polrNULL)

polr1 <- polr(Level ~ grade + sex + ethnic + DeSIRE2 + (DeSIRE2|dosage2),
              data = EOG_df, 
              method = "probit",
              Hess = TRUE)
print(polr1, digits = 3)
summary(polr1, digits = 3)
polr1_profile <- profile(polr1)

stepAIC(polr1)

anova(polrNULL, polr1)


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

brm1 <- brm(Level ~ grade + sex + ethnic + sex:ethnic + DeSIRE2,
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

brm2 <- brm(Level ~ grade + sex + ethnic + sex:ethnic + dosage2,
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm2)
summary(brm2)
prior_summary(brm2)
posterior_summary(brm2)

conditional_effects(brm2)

bayes


brm3 <- brm(Level ~ grade + sex + ethnic + sex:ethnic + dosage,
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm3)
summary(brm3)
prior_summary(brm3)
posterior_summary(brm3)

bayes_factor(brm4, brm3)

brm4 <- brm(Level ~ grade + (1|gr(School, by = grade)) + sex + ethnic + sex:ethnic + dosage,
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm4)
summary(brm4)
prior_summary(brm4)
posterior_summary(brm4)
variance_decomposition(brm4)

bayes_factor(brm4, brm3)


brm4 <- brm(Level ~ grade + (1|gr(School, by = grade)) + sex + ethnic + sex:ethnic + dosage,
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm4)
summary(brm4)
prior_summary(brm4)
posterior_summary(brm4)

pp_check(brm3, ndraws = 100)
pp_check(brm4, ndraws = 100)


brm5 <- brm(Level ~ grade + (1|gr(School, by = grade)) + sex + ethnic + dosage,
            data = EOG_df, 
            family = cumulative(link = "probit"), 
            save_pars = save_pars(all = TRUE),
            seed = 52, 
            control = list(adapt_delta = 0.95)
)
print(brm5)
summary(brm5)
prior_summary(brm5)
posterior_summary(brm5)
plot(brm5)

bayes_factor(brm4, brm5)

conditional_effects(brm5, categorical = TRUE, conditions = data.frame(grade = "8"))

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
conditional_effects(brm8, categorical = TRUE, conditions = data.frame(grade = "8"))

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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# OLD CODE =======
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











