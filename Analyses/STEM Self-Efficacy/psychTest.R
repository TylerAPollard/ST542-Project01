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
library(psychTools)
library(nlme)
library(multilevel)
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
# SELF EFFICACY ANALYSIS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load SSTEM Survey Data ----
load("Data/S-STEM/Cleaned S-STEM Survey Data.RData")

# MATH CONSTRUCT ====================================================================================
## Filter Data ----
mySurvey <- SSTEMsurvey_data |>
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

describe(mySurvey)

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

my.scales <- scoreItems(my.keys, mySurvey)
my.scales

my.scores <- my.scales$scores
my.scores

print(my.scales,short=FALSE)
print(my.scales,short=TRUE)

scales.ov <- scoreOverlap(my.keys,mySurvey)
scales.ov


my.scores <- my.scales$scores
headTail(round(my.scores,2) )

describe(my.scores)

pairs.panels(my.scores, pch = ".")


mySurvey2 <- mySurvey |>
  select(
    "YearSemester",
    "School",
    "Grade", 
    "Gender2",
    "Race2",
    c(str_which(colnames(mySurvey), pattern = "Math")),
    c(str_which(colnames(mySurvey), pattern = "Science")),
    c(str_which(colnames(mySurvey), pattern = "EngTech")),
    c(str_which(colnames(mySurvey), pattern = "Learning"))
  ) |>
  mutate(
    MathScore = my.scores[,1],
    ScienceScore = my.scores[,2],
    EngTechScore = my.scores[,3],
    LearningScore = my.scores[,4]
  )

MathData <- mySurvey2 |>
  select(
    "YearSemester",
    "School",
    "Grade", 
    "Gender2",
    "Race2",
    "MathScore"
  )

describeBy(MathData, group = c("School"))
describeBy(MathScore ~ YearSemester + School + Grade + Gender2 + Race2, data = MathData)

groupStatsMath <- statsBy(data = MathData ,#|> select("YearSemester", "School", "Grade","Gender2", "Race2"),
                          group = c(
                            #"YearSemester",
                            #"School",
                            #"Grade"
                            "Gender2",
                            "Race2"
                          ),
                          cors = TRUE)
print(groupStatsMath, short = FALSE)

groupStatsMath$F


ggplot(data = MathData, aes(x = Gender2, y = MathScore, color = YearSemester)) +
  geom_point(position = position_jitter(width = 0.05)) +
  geom_smooth(aes(group = YearSemester), method = "lm") +
  facet_wrap(vars(Race2)) +
  scale_y_continuous(limits = c(1,5)) +
  theme_bw()
  


# Factor Analysis
fa <- faBy(sb,nfactors=1)
print(fa)

Thurstone
lmCor(y = 5:9,x=1:4,data=Thurstone)

lmCor(y = c(str_which(colnames(mySurvey), pattern = "Math")),
      x = c("YearSemester",
            "School",
            "Grade", 
            "Gender2",
            "Race2"),
      data=mySurvey
)

library(haven)
mlmdata <- read_dta("https://stats.idre.ucla.edu/stat/examples/imm/imm10.dta")




set.seed(2020)

# Load original data
bounce_data <- read_csv("../data/bounce_rates_original.csv", col_types = cols() )

# Compute mean and stdev for each gorup
suff_stats <- bounce_data %>% 
  group_by(county) %>% 
  summarise(mu_bounce=mean(bounce_time), sd_bounce = sd(bounce_time),
            mu_age = mean(age), sd_age = sd(age))

# Initialize df
sim_df <- tibble(bounce_time = double(),
                 age = integer(),
                 county = character())

# Randomize counties, set max group size, set group samp props in relation to max grp size
county_subset <- sample(unique(bounce_data$county))
grp_size=150
props <- c(0.05, 0.15, 0.25, 0.5, 0.6, 0.75, 0.8, 1)

# Generate simulated data
for (i in 1:nrow(suff_stats)){
  
  n = grp_size*props[i]
  
  sim_times <- rnorm(n, suff_stats$mu_bounce[i], 
                     suff_stats$sd_bounce[i])
  
  sim_ages <- rnorm(n, suff_stats$mu_age[i],
                    suff_stats$sd_age[i])
  
  
  tmp_county = tibble(bounce_time = sim_times,
                      age = round(sim_ages),
                      county = county_subset[i])
  
  sim_df <- bind_rows(sim_df, tmp_county)
  
}



