
#                                           
#  COVID-19 Cases by State Study, comparing Democratic and Republican States                  
#  Class project for MTU 5701 Fall 2020     
#  by Lorenzo Gordon                        
#     Owen Applequist                       
#     Kasia Krueger                         
#                                           
 
########################################
#                                      #
# Analysis by Region                   #
#                                      #
########################################


# Install tidyverse library 
#install.packages("tidyverse")
library(tidyverse)

# Install qqpubr library
#install.packages("ggpubr")
library(ggpubr)

# Import Excel spredsheet 
library(readxl)
CovidByState <- read_excel(file.choose())

# Strip state code from State variable and add StateCode variable to dataset
StateCode = str_split(CovidByState$State, " - ", simplify=TRUE)[,2]
CovidByState <- cbind(CovidByState, StateCode)

# Calculate state population as a percentage of U.S. population and 
# add StatePopulationPercent to dataset
USpopulation = sum(CovidByState$Population)
StatePopulationPercent = CovidByState$Population/USpopulation
CovidByState <- cbind(CovidByState, StatePopulationPercent)

# Calculate cases per thousand population for each state and 
# add CasesPerThousand to dataset
CasesPerThousand = (CovidByState$`COVID Cases`/Population)*1000
CovidByState <- cbind(CovidByState, CasesPerThousand)

# Calculate deaths per thousand population for each state and 
# add DeathsPerThousand to dataset
DeathsPerThousand = (CovidByState$`COVID Deaths`/Population)*1000
CovidByState <- cbind(CovidByState, DeathsPerThousand)

# Total Cases 
TotalCases = sum(CovidByState$"COVID Cases")

# Total Deaths
TotalDeaths = sum(CovidByState$"COVID Deaths")

# Calculate expected cases by state based on their population and
# add ExpectedCases to dataset
ExpectedCases = CovidByState$StatePopulationPercent*TotalCases
CovidByState <- cbind(CovidByState, ExpectedCases)

# Calculate expected deaths by state based on their population and
# add ExpectedDeaths to dataset
ExpectedDeaths = CovidByState$StatePopulationPercent*TotalDeaths
CovidByState <- cbind(CovidByState, ExpectedCases)

# Calculate (cases - Expected cases) as % of expected cases by state and
# add CasesDeviation to dataset
CasesDeviation = (CovidByState$'COVID Cases' - ExpectedCases)/ExpectedCases
CovidByState <- cbind(CovidByState, CasesDeviation)

# Calculate (deaths - Expected deaths) as % of expected deaths by state and
# add DeathsDeviation to dataset
DeathsDeviation = (CovidByState$'COVID Deaths' - ExpectedDeaths)/ExpectedDeaths
CovidByState <- cbind(CovidByState, DeathsDeviation)

# Summary of CovidByState
summary(CovidByState)

# Print list of states by region
westSt = CovidByState$StateCode[Region == 'West']
southSt = CovidByState$StateCode[Region == 'South']
midwestSt = CovidByState$StateCode[Region == 'Midwest']
northeastSt = CovidByState$StateCode[Region == 'Northeast']
westStList = paste(westSt, collapse = " ")
southStList = paste(southSt, collapse = " ")
midwestStList = paste(midwestSt, collapse = " ")
northeastStList = paste(northeastSt, collapse = " ")

Regions = c("West", "South", "Midwest", "Northeast")
States = c(westStList, southStList, midwestStList, northeastStList)

report = cbind(Regions, States)
t = ggtexttable(report,  rows = NULL,theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "States by Region", face = "plain")

# Histogram of CasesPerThousand total and by Region
h = hist(CasesPerThousand,
     main="COVID-19 Cases per Thousand by State",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$CasesPerThousand[Region == 'West'],
     main="COVID-19 Cases per Thousand by Western States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$CasesPerThousand[Region == 'South'],
     main="COVID-19 Cases per Thousand by Southern States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$CasesPerThousand[Region == 'Midwest'],
     main="COVID-19 Cases per Thousand by Midwestern States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$CasesPerThousand[Region == 'Northeast'],
         main="COVID-19 Cases per Thousand by Northeastern States",
         col="blue",
         xlab="Cases per Thousand Population",
         ylab="Number of States",
         ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Histogram of DeathsPerThousand total and by region
h = hist(DeathsPerThousand,
     main="COVID-19 Deaths per Thousand by State",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$DeathsPerThousand[Region == 'West'],
     main="COVID-19 Deaths per Thousand by Western States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$DeathsPerThousand[Region == 'South'],
     main="COVID-19 Deaths per Thousand by Southern States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$DeathsPerThousand[Region == 'Midwest'],
     main="COVID-19 Deaths per Thousand by Midwestern States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h = hist(CovidByState$DeathsPerThousand[Region == 'Northeast'],
         main="COVID-19 Deaths per Thousand by Northeastern States",
         col="blue",
         xlab="Deaths per Thousand Population",
         ylab="Number of States",
         ylim=range(0:15)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Total Cases by region
TotalCasesWest = sum(CovidByState$"COVID Cases"[Region == 'West'])
TotalCasesSouth = sum(CovidByState$"COVID Cases"[Region == 'South'])
TotalCasesMidwest = sum(CovidByState$"COVID Cases"[Region == 'Midwest'])
TotalCasesNortheast = sum(CovidByState$"COVID Cases"[Region == 'Northeast'])

# Total Deaths by region
TotalDeathsWest = sum(CovidByState$"COVID Deaths"[Region == 'West'])
TotalDeathsSouth = sum(CovidByState$"COVID Deaths"[Region == 'South'])
TotalDeathsMidwest = sum(CovidByState$"COVID Deaths"[Region == 'Midwest'])
TotalDeathsNortheast = sum(CovidByState$"COVID Deaths"[Region == 'Northeast'])

# Population by region
PopulationWest = sum(CovidByState$Population[Region == 'West'])
PopulationSouth = sum(CovidByState$Population[Region == 'South'])
PopulationMidwest = sum(CovidByState$Population[Region == 'Midwest'])
PopulationNortheast = sum(CovidByState$Population[Region == 'Northeast'])

# COVID infection rate per 1000 by reion
CovidRateWest = (TotalCasesWest/PopulationWest)*1000
CovidRateSouth = (TotalCasesSouth/PopulationSouth)*1000
CovidRateMidwest = (TotalCasesMidwest/PopulationMidwest)*1000
CovidRateNortheast = (TotalCasesNortheast/PopulationNortheast)*1000

# COVID death rate per 1000 by region
CovidDeathRateWest= (TotalDeathsWest/PopulationWest)*1000
CovidDeathRateSouth = (TotalDeathsSouth/PopulationSouth)*1000
CovidDeathRateMidwest = (TotalDeathsMidwest/PopulationMidwest)*1000
CovidDeathRateNortheast = (TotalDeathsNortheast/PopulationNortheast)*1000

# Display rates by region
CovidRateWest
CovidRateSouth 
CovidRateMidwest 
CovidRateNortheast
CovidDeathRateWest 
CovidDeathRateSouth 
CovidDeathRateMidwest 
CovidDeathRateNortheast

# boxplots of casesPerThousand by region
boxplot(CasesDeviation ~ CovidByState$Region, 
        main = "Cases Deviation by Region", 
        xlab = "Percent Deviation from Expected Cases",
        ylab="",
        col = "blue"
)

# boxplots of deathsPerThousand by region
boxplot(DeathsDeviation ~ CovidByState$Region, 
        main = "Deaths Deviation by Region", 
        xlab = "Percent Deviation from Expected Deaths",
        ylab="",
        col = "blue"
)

# Barchart total cases per thousand by region
CasesPerThousand = c(CovidRateWest, CovidRateSouth, CovidRateMidwest,
                     CovidRateNortheast)
Regions = c("West", "South", "Midwest", "Northeast")
barplot(CasesPerThousand, names=Regions, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Cases per Thousand Population by Region",
        ylim = c(0,35))

# Barchart total deaths per thousand by region
DeathsPerThousand = c(CovidDeathRateWest, CovidDeathRateSouth, 
      CovidDeathRateMidwest, CovidDeathRateNortheast)
barplot(DeathsPerThousand, names=Regions, xlab="Party", ylab="Deaths per Thousand",
        col="blue", main="COVID-19 Deaths per Thousand Population by Region")

# Mean and S.D. of cases deviation from expected cases
sum(CasesDeviation)
mean(CasesDeviation)
sd(CasesDeviation)

# Mean and S.D. of cases deviation for West
MeanWest = round(mean(CovidByState$CasesDeviation[Region == "West"]), 
    digits = 4)
SdWest = round(sd(CovidByState$CasesDeviation[Region == "West"]), 
    digits = 4)

# Mean and S.D. of cases deviation for South
MeanSouth = round(mean(CovidByState$CasesDeviation[Region == "South"]), 
                 digits = 4)
SdSouth = round(sd(CovidByState$CasesDeviation[Region == "South"]), 
               digits = 4)

# Mean and S.D. of cases deviation for Midwest
MeanMidwest = round(mean(CovidByState$CasesDeviation[Region == "Midwest"]), 
                 digits = 4)
SdMidwest = round(sd(CovidByState$CasesDeviation[Region == "Midwest"]), 
               digits = 4)

# Mean and S.D. of cases deviation for Northeast
MeanNortheast = round(mean(CovidByState$CasesDeviation[Region == "Northeast"]), 
                 digits = 4)
SdNortheast = round(sd(CovidByState$CasesDeviation[Region == "Northeast"]), 
               digits = 4)

# Print mean and s.d. for regions
Mean = c(MeanWest, MeanSouth, MeanMidwest, MeanNortheast)
SD = c(SdWest, SdSouth, SdMidwest, SdNortheast)

report = cbind(Regions, Mean, SD)
t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:5), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Cases Deviation Mean & SD", face = "plain")

# Mean and S.D. of death deviation from expected cases
sum(DeathsDeviation)
mean(DeathsDeviation)
sd(DeathsDeviation)

# Mean and S.D. of death deviation for West
MeanDeathsWest = round(mean(CovidByState$DeathsDeviation[Region == "West"]),
      digits = 4)
SdDeathsWest = round(sd(CovidByState$DeathsDeviation[Region == "West"]), 
      digits = 4)

# Mean and S.D. of death deviation for South
MeanDeathsSouth = round(mean(CovidByState$DeathsDeviation[Region == "South"]),
                       digits = 4)
SdDeathsSouth = round(sd(CovidByState$DeathsDeviation[Region == "South"]), 
                     digits = 4)

# Mean and S.D. of death deviation for Midwest
MeanDeathsMidwest = round(mean(CovidByState$DeathsDeviation[Region == "Midwest"]),
                       digits = 4)
SdDeathsMidwest = round(sd(CovidByState$DeathsDeviation[Region == "Midwest"]), 
                     digits = 4)

# Mean and S.D. of death deviation for Northeast
MeanDeathsNortheast = round(mean(CovidByState$DeathsDeviation[Region == "Northeast"]),
                       digits = 4)
SdDeathsNortheast = round(sd(CovidByState$DeathsDeviation[Region == "Northeast"]), 
                     digits = 4)

# Print mean and s.d. for region deaths
Mean = c(MeanDeathsWest, MeanDeathsSouth, MeanDeathsMidwest, MeanDeathsNortheast)
SD = c(SdDeathsWest, SdDeathsSouth, SdDeathsMidwest, SdDeathsNortheast)

report = cbind(Regions, Mean, SD)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:5), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Deaths Deviation Mean & SD", face = "plain")

aggregate(DeathsDeviation~Region, FUN=mean)
aggregate(DeathsDeviation~Region, FUN=sd)
aggregate(CasesDeviation~Region, FUN=mean)
aggregate(CasesDeviation~Region, FUN=sd)

# Checks for normality of CaseDeviations

# Q-Q plot of CaseDeviation
ggqqplot(CasesDeviation, color="blue", title = "Q-Q Plot of Cases Deviation")

# Check for bell shaped density plot
ggdensity(CasesDeviation, 
          main = "Density plot of COVID Case Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(CasesDeviation)

# Check for bell shaped density plot
ggdensity(DeathsDeviation, 
          main = "Density plot of COVID Deaths Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(DeathsDeviation)

#
# One-Way ANOVA for cases deviation
#

# R ANOVA function (the eazy way)
y = CovidByState$CasesDeviation
region.aov = aov(y~Region, data=CovidByState)
anovaT = anova(region.aov)

# Display ANOVA table
columns = c('Source', 'df',	'SS',	'MS',	'F',	'P-Value')

row1 = c('Between Groups', anovaT$Df[1], 
         round(anovaT$'Sum Sq'[1], digits = 4), 
         round(anovaT$'Mean Sq'[1], digits = 4), 
         round(anovaT$'F value'[1], digits = 4), 
         round(anovaT$'Pr(>F)'[1], digits = 4))
row2 = c('Within Groups', anovaT$Df[2], 
         round(anovaT$'Sum Sq'[2], digits = 4), 
         round(anovaT$'Mean Sq'[2], digits = 4), 
         '', '')
row3 = c('Total', round(sum(anovaT$Df), digits = 4), 
         round(sum(anovaT$'Sum Sq'), digits = 4), 
         '', '', '')
report = rbind(columns, row1, row2, row3)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:6), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "One-Way ANOVA of Cases Deviation by Region", face = "plain")

#
# One-Way ANOVA for deaths deviation
#

# R ANOVA function (the eazy way)
y = CovidByState$DeathsDeviation
region.aov = aov(y~Region, data=CovidByState)
anovaT = anova(region.aov)

# Display ANOVA table
columns = c('Source', 'df',	'SS',	'MS',	'F',	'P-Value')

row1 = c('Between Groups', anovaT$Df[1], 
         round(anovaT$'Sum Sq'[1], digits = 4), 
         round(anovaT$'Mean Sq'[1], digits = 4), 
         round(anovaT$'F value'[1], digits = 4), 
         round(anovaT$'Pr(>F)'[1], digits = 4))
row2 = c('Within Groups', anovaT$Df[2], 
         round(anovaT$'Sum Sq'[2], digits = 4), 
         round(anovaT$'Mean Sq'[2], digits = 4), 
         '', '')
row3 = c('Total', round(sum(anovaT$Df), digits = 4), 
         round(sum(anovaT$'Sum Sq'), digits = 4), 
         '', '', '')
report = rbind(columns, row1, row2, row3)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:6), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "One-Way ANOVA of Deaths Deviation by Region", face = "plain")

# Why are case rates different then death rates
plot(CasesDeviation, 
     DeathsDeviation,
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, All States")
abline(lm(DeathsDeviation ~ CasesDeviation))

plot(CasesDeviation[Region == "West"], 
     DeathsDeviation[Region == "West"],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, West Region")
abline(lm(DeathsDeviation[Region == "West"] ~ 
    CasesDeviation[Region == "West"]))

plot(CasesDeviation[Region == "South"], 
     DeathsDeviation[Region == "South"],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, South Region")
abline(lm(DeathsDeviation[Region == "South"] ~ 
    CasesDeviation[Region == "South"]))

plot(CasesDeviation[Region == "Midwest"], 
     DeathsDeviation[Region == "Midwest"],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Midwest Region")
abline(lm(DeathsDeviation[Region == "Midwest"] ~ 
    CasesDeviation[Region == "Midwest"]))

plot(CasesDeviation[Region == "Northeast"], 
     DeathsDeviation[Region == "Northeast"],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Northeast Region")
abline(lm(DeathsDeviation[Region == "Northeast"] ~ 
            CasesDeviation[Region == "Northeast"]))

