# CHI-Medical-errors
Install R Studio. R Studio version >= 1.1.463 is needed. R version 3.6.2


-- Table 1: "Baseline characteristics of the population" is generated from Baseline Table 1.R

-- Table 2: "Medicines dosage deviation statistics" is generated from Medication tables.R

-- Table 3: "Significant risks for longer length of stay according to gestation group" is generated from Final_table.R

-- Supplementary Figure S1(a,b,c) and Supplementary Figure S5(a,b,c,d,e): "Bubble plot for intake of nutrition according to gestation group" is generated from Bubble plot ALL.R

-- Supplementary Figure S2(a) and S2(b): "Medication deviation across gestation group" is generated from Medication bar chart.R

-- Supplementary Figure S3 : "Clinical diagnosis distribution across gestational age groups" is generated from Morbidity stacks.R

-- Supplementary S4(a,b,c,d): "Patient frequency vs LOS for various gestation categories" is generated from Supplementary 4.R

-- Supplementary Table S2(a,b,c): "Medicines deviation statistics" is generated from Medication tables 2a,b supp 3a,b.R

Steps to successfully run individual scripts:
Install and import the required packages - tibble, lsmeans, dplyr, relimp, rstudioapi, readxl, multcomp, qpcR, xquartz, ggplot2, hrbrthemes, viridis, tidyverse

#### Scripts ####
1. Download the Training files of Site 1 and Site 2 for both Neofax and Nutrition (These files are created from JAVA code - EsphaganErrors.java and NeofaxErrors.java)
2. Define the path in Baseline Table 1.R, Final_table.R and in other R files.
3. Run the script and respective Tables and Figures will be generated.
# CHI-LOS
# CHI-LOS
