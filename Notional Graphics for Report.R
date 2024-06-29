#### Notional Graphics for Consulting Report

# Load Libraries ----
library(data.table) # Read csv in tibble format
library(readxl) # Read xlsx 

## Data Manipulation
library(stringr) # Manipulate strings
library(plyr) # Produce summary tables/data.frames

## Data Analysis
library(likert)
library(psych)
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

# Load S-STEM survey data ----
load("Data/Cleaned S-STEM Survey Data.RData")


# RESEARCH QUESTION 1 =======
## Diciplinary Knowledge ----
discKnow_data <- data.frame(
  School = c(rep("West Edgecombe Middle School", 115), 
             rep("Phillips Middle School", 97)),
  Student = c(rep("DeSIRE",45), 
              rep("non DeSIRE",70),
              rep("DeSIRE",36), 
              rep("non DeSIRE",61)),
  EOG = NA
)

## Score from 217-279
set.seed(52)
for(i in 1:nrow(discKnow_data)){
  if(discKnow_data$School[i] == "West Edgecombe Middle School"){
    temp <- rnorm(1, mean = 245, sd = 5)
    if(discKnow_data$Student[i] == "DeSIRE"){
      temp <- temp + rnorm(1, mean = 7, sd = 3)
    }else if(discKnow_data$Student[i] == "non DeSIRE"){
      
    }
  }else if(discKnow_data$School[i] == "Phillips Middle School"){
    temp <- rnorm(1, mean = 260, sd = 10)
    if(discKnow_data$Student[i] == "DeSIRE"){
      temp <- temp + rnorm(1, mean = 7, sd = 3)
    }else if(discKnow_data$Student[i] == "non DeSIRE"){
      
    }
  }
  discKnow_data$EOG[i] <- round(temp, 0) - 240
}

discKnow_data <- discKnow_data |>
  mutate(
    School = factor(School),
    Student = factor(Student)
  )

### Analyze ----
discKnow_aov <- lm(EOG ~ School*Student,
                   data = discKnow_data)
discKnow_aov <- lmer(EOG ~ Student + (1|School),
                     data = discKnow_data)
discKnow_aov <- lm(EOG ~ Student + School,
                   data = discKnow_data)
summary(discKnow_aov)
Anova(discKnow_aov, type = 3)
anova(discKnow_aov)

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


## Career Awareness =========================================
#careerAwarenessMath
#careerAwarenessEng
#careerAwarenessTech
#careerAwarenessScience 

### Data ----
careerAwaren










