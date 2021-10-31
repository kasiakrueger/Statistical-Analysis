
#                                           
#  COVID-19 Cases by State Study, comparing Democratic and Republican States                  
#  Class project for MTU 5701 Fall 2020     
#  by Lorenzo Gordon                        
#     Owen Applequist                       
#     Kasia Krueger                         
#                                           
 
########################################
#                                      #
# Analysis by Population Density       #
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

# Calculate state area as a percentage of U.S. area and 
# add StateAreaPercent CovidByState
USArea = sum(`State Land Area (sq mi)`)
StateAreaPercent = `State Land Area (sq mi)`/USpopulation
CovidByState <- cbind(CovidByState, StateAreaPercent)

# Calculate population density per state and
# add PopulationDensity to CovidByState
PopulationDensity = Population/`State Land Area (sq mi)`
CovidByState <- cbind(CovidByState, PopulationDensity)

# Total Cases 
TotalCases = sum(CovidByState$"COVID Cases")

# Total Deaths
TotalDeaths = sum(CovidByState$"COVID Deaths")

# Calculate expected cases by state based on their population and
# add ExpectedCases to dataset
ExpectedCases = StatePopulationPercent*TotalCases
CovidByState <- cbind(CovidByState, ExpectedCases)

# Calculate expected deaths by state based on their population and
# add ExpectedDeaths to dataset
ExpectedDeaths = StatePopulationPercent*TotalDeaths
CovidByState <- cbind(CovidByState, ExpectedCases)

# Calculate cases per thousand population for each state and 
# add CasesPerThousand to dataset
CasesPerThousand = (`COVID Cases`/Population)*1000
CovidByState <- cbind(CovidByState, CasesPerThousand)

# Calculate deaths per thousand population for each state and 
# add DeathsPerThousand to dataset
DeathsPerThousand = (`COVID Deaths`/Population)*1000
CovidByState <- cbind(CovidByState, DeathsPerThousand)

# Split states into three groups by population density
PopulationDensitySorted = sort(PopulationDensity)
PopulationDensityLevel = PopulationDensitySorted
for (i in 1:51) {
  if (PopulationDensity[i] < PopulationDensitySorted[18]) {
    PopulationDensityLevel[i] = 'Low'
  } else if (PopulationDensity[i] > PopulationDensitySorted[34]) {
    PopulationDensityLevel[i] = 'High'
  } else {
    PopulationDensityLevel[i] = 'Medium'
  }
}
CovidByState <- cbind(CovidByState, PopulationDensityLevel)

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

# Print list of states by density
lowSt = CovidByState$StateCode[PopulationDensityLevel == 'Low']
medSt = CovidByState$StateCode[PopulationDensityLevel == 'Medium']
highSt = CovidByState$StateCode[PopulationDensityLevel == 'High']
lowStList = paste(lowSt, collapse = " ")
medStList = paste(medSt, collapse = " ")
highStList = paste(highSt, collapse = " ")

Levels = c("Low", "Medium", "High")
States = c(lowStList, medStList, highStList)

report = cbind(Levels, States)
t = ggtexttable(report,  rows = NULL,theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "States by Population Density", face = "plain")

# Histogram of CasesPerThousand total and by density
mx = mean(CasesPerThousand)
h = hist(CasesPerThousand,
     main="COVID-19 Cases per 1,000 by State",
     col="blue",
     xlab="Cases per 1,000 by State",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CovidByState$CasesPerThousand[PopulationDensityLevel == 'Low'])
h = hist(CovidByState$CasesPerThousand[PopulationDensityLevel == 'Low'],
     main="COVID-19 Cases per Thousand by Low Density States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CovidByState$CasesPerThousand[PopulationDensityLevel == 'Medium'])
h = hist(CovidByState$CasesPerThousand[PopulationDensityLevel == 'Medium'],
     main="COVID-19 Cases per Thousand by Medium Density States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CovidByState$CasesPerThousand[PopulationDensityLevel == 'High'])
h = hist(CovidByState$CasesPerThousand[PopulationDensityLevel == 'High'],
     main="COVID-19 Cases per Thousand by High Density States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Histogram of DeathsPerThousand total and by population density
mx = mean(DeathsPerThousand)
h = hist(DeathsPerThousand,
     main="COVID-19 Deaths per Thousand by State",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CovidByState$DeathsPerThousand[PopulationDensityLevel == 'Low'])
h = hist(CovidByState$DeathsPerThousand[PopulationDensityLevel == 'Low'],
     main="COVID-19 Deaths per Thousand by Low Density States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CovidByState$DeathsPerThousand[PopulationDensityLevel == 'Medium'])
h = hist(CovidByState$DeathsPerThousand[PopulationDensityLevel == 'Medium'],
     main="COVID-19 Deaths per Thousand by Medium Density States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CovidByState$DeathsPerThousand[PopulationDensityLevel == 'High'])
h = hist(CovidByState$DeathsPerThousand[PopulationDensityLevel == 'High'],
     main="COVID-19 Deaths per Thousand by High Density States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Total Cases by density
TotalCasesLow = sum(CovidByState$"COVID Cases"[PopulationDensityLevel == 'Low'])
TotalCasesMed = sum(CovidByState$"COVID Cases"[PopulationDensityLevel == 'Medium'])
TotalCasesHigh = sum(CovidByState$"COVID Cases"[PopulationDensityLevel == 'High'])

# Total Deaths by density
TotalDeathsLow = sum(CovidByState$"COVID Deaths"[PopulationDensityLevel == 'Low'])
TotalDeathsMed = sum(CovidByState$"COVID Deaths"[PopulationDensityLevel == 'Medium'])
TotalDeathsHigh = sum(CovidByState$"COVID Deaths"[PopulationDensityLevel == 'High'])

# Population by density
PopulationLow = sum(CovidByState$Population[PopulationDensityLevel == 'Low'])
PopulationMed = sum(CovidByState$Population[PopulationDensityLevel == 'Medium'])
PopulationHigh = sum(CovidByState$Population[PopulationDensityLevel == 'High'])

# COVID infection rate per 1000 by density
CovidRateLow = (TotalCasesLow/PopulationLow)*1000
CovidRateMed = (TotalCasesMed/PopulationMed)*1000
CovidRateHigh = (TotalCasesHigh/PopulationHigh)*1000

# COVID death rate per 1000 by density
CovidDeathRateLow = (TotalDeathsLow/PopulationLow)*1000
CovidDeathRateMed = (TotalDeathsMed/PopulationMed)*1000
CovidDeathRateHigh = (TotalDeathsHigh/PopulationHigh)*1000

# Display rates by density
CovidRateLow
CovidRateMed 
CovidRateHigh 
CovidDeathRateLow 
CovidDeathRateMed 
CovidDeathRateHigh 

# boxplots of casesPerThousand for Low, Medium, High density
boxplot(CasesDeviation ~ CovidByState$PopulationDensityLevel, 
        main = "Cases Deviation by Population Density", 
        xlab = "Percent Deviation from Expected Cases",
        ylab="",
        col = "blue"
)

# boxplots of deathsPerThousand for Low, Medium, High density
boxplot(DeathsDeviation ~ CovidByState$PopulationDensityLevel, 
        main = "Deaths Deviation by Population Density", 
        xlab = "Percent Deviation from Expected Deaths",
        ylab="",
        col = "blue"
)

# Barchart total cases per thousand by density
CasesPerThousandDensity = c(CovidRateLow, CovidRateMed, CovidRateHigh)
Levels = c("Low", "Medium", "High")
barplot(CasesPerThousandDensity, names=Levels, xlab="Density", ylab="Cases per Thousand",
        col="blue", 
        main="COVID-19 Cases per Thousand Population by Density",
        ylim = c(0,35))

# Barchart total deaths per thousand by density
DeathsPerThousandDensity = c(CovidDeathRateLow, CovidDeathRateMed, 
      CovidDeathRateHigh)
barplot(DeathsPerThousandDensity, names=Levels, xlab="Density", 
        ylab="Deaths per Thousand",
        ylim=0:1,
        col="blue", 
        main="COVID-19 Deaths per Thousand Population by Density")

# Mean and S.D. of cases deviation from expected cases
sum(CasesDeviation)
mean(CasesDeviation)
sd(CasesDeviation)

# Mean and S.D. of cases deviation for low density
MeanLow = round(mean(CovidByState$CasesDeviation[PopulationDensityLevel == 'Low']), digits = 4)
SdLow = round(sd(CovidByState$CasesDeviation[PopulationDensityLevel == 'Low']), digits = 4)

# Mean and S.D. of cases deviation for medium density
MeanMed = round(mean(CovidByState$CasesDeviation[PopulationDensityLevel == 'Medium']), digits = 4)
SdMed = round(sd(CovidByState$CasesDeviation[PopulationDensityLevel == 'Medium']), digits = 4)

# Mean and S.D. of cases deviation for high density
MeanHigh = round(mean(CovidByState$CasesDeviation[PopulationDensityLevel == 'High']), digits = 4)
SdHigh = round(sd(CovidByState$CasesDeviation[PopulationDensityLevel == 'High']), digits = 4)

# Print mean and s.d. by density
Mean = c(MeanLow, MeanMed, MeanHigh)
SD = c(SdLow, SdMed, SdHigh)

report = cbind(Levels, Mean, SD)
t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Cases Deviation Mean & SD", face = "plain")

# Mean and S.D. of death deviation from expected cases
sum(DeathsDeviation)
mean(DeathsDeviation)
sd(DeathsDeviation)

# Mean and S.D. of death deviation for low density 
MeanDeathLow = round(mean(CovidByState$DeathsDeviation[PopulationDensityLevel == 'Low']),
      digits = 4)
SdDeathLow = round(sd(CovidByState$DeathsDeviation[PopulationDensityLevel == 'Low']),
      digits = 4)

# Mean and S.D. of death deviation for medium density
MeanDeathMed = round(mean(CovidByState$DeathsDeviation[PopulationDensityLevel == 'Medium']),
                     digits = 4)
SdDeathMed = round(sd(CovidByState$DeathsDeviation[PopulationDensityLevel == 'Medium']),
                   digits = 4)

# Mean and S.D. of death deviation for high density
MeanDeathHigh = round(mean(CovidByState$DeathsDeviation[PopulationDensityLevel == 'High']),
                     digits = 4)
SdDeathHigh = round(sd(CovidByState$DeathsDeviation[PopulationDensityLevel == 'High']),
                   digits = 4)

# Print mean and s.d. for deaths by density
Mean = c(MeanDeathLow, MeanDeathMed, MeanDeathHigh)
SD = c(SdDeathLow, SdDeathMed, SdDeathHigh)

report = cbind(Levels, Mean, SD)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Deaths Deviation Mean & SD", face = "plain")

aggregate(DeathsDeviation~PopulationDensityLevel, FUN=mean)
aggregate(DeathsDeviation~PopulationDensityLevel, FUN=sd)
aggregate(CasesDeviation~PopulationDensityLevel, FUN=mean)
aggregate(CasesDeviation~PopulationDensityLevel, FUN=sd)

# Checks for normality of CaseDeviations

# Q-Q plot of CaseDeviation
ggqqplot(CasesDeviation, color="blue", title = "Q-Q Plot of Cases Deviation")

# Check for bell shaped density plot
ggdensity(CasesDeviation, 
          main = "Density plot of COVID Case Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(CasesDeviation)

#
# One-Way ANOVA for cases deviation by density
#

# R ANOVA function (the eazy way)
y = CovidByState$CasesDeviation
density.aov = aov(y~PopulationDensityLevel, data=CovidByState)
anovaT = anova(density.aov)

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
  tab_add_title(text = "One-Way ANOVA of Cases Deviation by Density", face = "plain")

# R^2 = 1 -SSE/SST 
Rsqr = 1-anovaT$'Sum Sq'[2]/(anovaT$'Sum Sq'[1] + anovaT$'Sum Sq'[2])
Rsqr

# 100*Rsqr% of the variability in CaseDeviation is explained by density level

#
# One-Way ANOVA for deaths deviation
#

# R ANOVA function (the eazy way)
y = CovidByState$DeathsDeviation
density.aov = aov(y~PopulationDensityLevel, data=CovidByState)
anovaT = anovaT = anova(density.aov)

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
  tab_add_title(text = "One-Way ANOVA of Deaths Deviation by Density", face = "plain")

# R^2 = 1 -SSE/SST 
Rsqr = 1-anovaT$'Sum Sq'[2]/(anovaT$'Sum Sq'[1] + anovaT$'Sum Sq'[2])
Rsqr

# Fisher's Least Significant Difference
# produces a matrix of p-values
pairwise.t.test(y, PopulationDensityLevel, p.adjust.method='none')

# Bonferroni correction
pairwise.t.test(y, PopulationDensityLevel, p.adjust.method='bonferroni')

# Turey HSD
TukeyHSD(density.aov)

#
# Assessing Homoscedasticity
#

# Levene's test
# can not reject H0 that varianves are equal
density.res = residuals(density.aov)
anova(aov(density.res^2~PopulationDensityLevel))

# Plot residuals aginst fitted values
density.fit = fitted.values(density.aov)
plot(x=density.fit, y=density.res,
     main = "ANOVA Fitted Values vs Resuials")

# Standized residuals
density.stdres = density.res/(anovaT$'Mean Sq'[2]^.5)
plot(x=density.fit, y=density.stdres, 
    main = "ANOVA Fitted Values vs Standard-Resuials")

# Why are case rates different then death rates
plot(CasesDeviation, 
     DeathsDeviation,
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, All States")
abline(lm(DeathsDeviation ~ CasesDeviation))

plot(CasesDeviation[PopulationDensityLevel == 'Low'], 
     DeathsDeviation[PopulationDensityLevel == 'Low'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Low States")
abline(lm(DeathsDeviation[PopulationDensityLevel == 'Low'] ~ 
    CasesDeviation[PopulationDensityLevel == 'Low']))

plot(CasesDeviation[PopulationDensityLevel == 'Medium'], 
     DeathsDeviation[PopulationDensityLevel == 'Medium'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Medium States")
abline(lm(DeathsDeviation[PopulationDensityLevel == 'Medium'] ~ 
            CasesDeviation[PopulationDensityLevel == 'Medium']))

plot(CasesDeviation[PopulationDensityLevel == 'High'], 
     DeathsDeviation[PopulationDensityLevel == 'High'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, High States")
abline(lm(DeathsDeviation[PopulationDensityLevel == 'High'] ~ 
            CasesDeviation[PopulationDensityLevel == 'High']))

