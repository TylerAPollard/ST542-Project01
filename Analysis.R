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
# RESEARCH QUESTION 1 =======================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





