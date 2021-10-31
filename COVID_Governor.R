
#                                           
#  COVID-19 Cases by State Study, comparing Democratic and Republican States                  
#  Class project for MTU 5701 Fall 2020     
#  by Lorenzo Gordon                        
#     Owen Applequist                       
#     Kasia Krueger                         
#                                           
 
########################################
#                                      #
# Analysis by State Governor           #
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

# Print list of states by Governor's party
DemocratSt = CovidByState$StateCode[CovidByState$"Governor's Party" == 'Democrat']
RepublicanSt = CovidByState$StateCode[CovidByState$"Governor's Party" == 'Republican']
DemocratStList = paste(DemocratSt, collapse = " ")
RepublicanStList = paste(RepublicanSt, collapse = " ")

Party = c("Democrat", "Republican")
States = c(DemocratStList, RepublicanStList)

report = cbind(Party, States)
t = ggtexttable(report,  rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:3), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "States by Governor's Party", face = "plain")

# Histogram of CasesPerThousand total and by Governor's Party
mx = mean(CasesPerThousand)
h = hist(CasesPerThousand,
     main="COVID-19 Cases per Thousand by State",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CasesPerThousand[CovidByState$"Governor's Party" == 'Democrat'])
h = hist(CasesPerThousand[CovidByState$"Governor's Party" == 'Democrat'],
     main="COVID-19 Cases per Thousand by Democratic Governor",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CasesPerThousand[CovidByState$"Governor's Party" == 'Republican'])
h = hist(CovidByState$CasesPerThousand[CovidByState$"Governor's Party" == 'Republican'],
     main="COVID-19 Cases per Thousand by Republican Governor",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Histogram of DeathsPerThousand total and by Governor's party
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

mx = mean(DeathsPerThousand[CovidByState$"Governor's Party" == 'Democrat'])
h = hist(CovidByState$DeathsPerThousand[CovidByState$"Governor's Party" == 'Democrat'],
     main="COVID-19 Deaths per Thousand by Democratic Governor",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(DeathsPerThousand[CovidByState$"Governor's Party" == 'Republican'])
h = hist(CovidByState$DeathsPerThousand[CovidByState$"Governor's Party" == 'Republican'],
     main="COVID-19 Deaths per Thousand by Republican Governor",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Total Cases by Governor
TotalCasesDem = sum(CovidByState$"COVID Cases"[CovidByState$"Governor's Party" == 'Democrat'])
TotalCasesRep = sum(CovidByState$"COVID Cases"[CovidByState$"Governor's Party" == 'Republican'])

# Total Deaths by Governor
TotalDeathsDem = sum(CovidByState$"COVID Deaths"[CovidByState$"Governor's Party" == 'Democrat'])
TotalDeathsRep = sum(CovidByState$"COVID Deaths"[CovidByState$"Governor's Party" == 'Republican'])

# Population by Governor
PopulationDem = sum(CovidByState$Population[CovidByState$"Governor's Party" == 'Democrat'])
PopulationRep = sum(CovidByState$Population[CovidByState$"Governor's Party"== 'Republican'])

# COVID infection rate per 1000 by Governor
CovidRateDem = (TotalCasesDem/PopulationDem)*1000
CovidRateRep = (TotalCasesRep/PopulationRep)*1000

# COVID death rate per 1000 by Governor
CovidDeathRateDem = (TotalDeathsDem/PopulationDem)*1000
CovidDeathRateRep = (TotalDeathsRep/PopulationRep)*1000

# Display rates by Governor
CovidRateDem 
CovidRateRep 
CovidDeathRateDem 
CovidDeathRateRep 

# boxplots of casesPerThousand for Democrat, Republican, Governor
boxplot(CasesDeviation ~ CovidByState$"Governor's Party", 
        main = "Cases Deviation by Governor's Party", 
        xlab = "Percent Deviation from Expected Cases",
        ylab="",
        col = "blue"
)

# boxplots of deathsPerThousand for Democrat, Republican Governor
boxplot(DeathsDeviation ~ CovidByState$"Governor's Party", 
        main = "Deaths Deviation by Governor's Party", 
        xlab = "Percent Deviation from Expected Deaths",
        ylab="",
        col = "blue"
)

# Barchart total cases per thousand by Governor's Party
CasesPerThousand = c(CovidRateDem, CovidRateRep)
Parties = c("Democrat", "Republican")
barplot(CasesPerThousand, names=Parties, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Cases per Thousand Population by Party",
        ylim = c(0,35))

# Barchart total deaths per thousand by Governor's Party
DeathsPerThousand = c(CovidDeathRateDem, CovidDeathRateRep)
barplot(DeathsPerThousand, names=Parties, xlab="Party", ylab="Deaths per Thousand",
        col="blue", main="COVID-19 Deaths per Thousand Population by Party")

# Mean and S.D. of cases deviation from expected cases
sum(CasesDeviation)
mean(CasesDeviation)
sd(CasesDeviation)

# Mean and S.D. of cases deviation for Democratic states
MeanDem = round(mean(CovidByState$CasesDeviation[CovidByState$"Governor's Party" == 
      'Democrat']), digits = 4)
SdDem= round(sd(CovidByState$CasesDeviation[CovidByState$"Governor's Party" == 
      'Democrat']), digits = 4)

# Mean and S.D. of cases deviation for Republican states
MeanRep = round(mean(CovidByState$CasesDeviation[CovidByState$"Governor's Party" == 
      'Republican']), digits = 4)
SdRep = round(sd(CovidByState$CasesDeviation[CovidByState$"Governor's Party" == 
      'Republican']), digits = 4)

# Print mean and s.d.
Mean = c(MeanDem, MeanRep)
SD = c(SdDem, SdRep)

report = cbind(Party, Mean, SD)
t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:3), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Cases Deviation Mean & SD", face = "plain")

# Mean and S.D. of death deviation from expected cases
sum(DeathsDeviation)
mean(DeathsDeviation)
sd(DeathsDeviation)

# Mean and S.D. of death deviation for Democratic states
MeanDeathsDem = round(mean(CovidByState$DeathsDeviation[CovidByState$"Governor's Party" == 
      'Democrat']), digits = 4)
SdDeathsDem = round(sd(CovidByState$DeathsDeviation[CovidByState$"Governor's Party" == 
      'Democrat']), digits = 4)

# Mean and S.D. of death deviation for Republican states
MeanDeathsRep = round(mean(CovidByState$DeathsDeviation[CovidByState$"Governor's Party" == 
      'Republican']), digits = 4)
SdDeathsRep = round(sd(CovidByState$DeathsDeviation[CovidByState$"Governor's Party" == 
      'Republican']), digits = 4)

# Print mean and s.d. for Governor deaths
Mean = c(MeanDeathsDem, MeanDeathsRep)
SD = c(SdDeathsDem, SdDeathsRep)

report = cbind(Party, Mean, SD)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:3), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Deaths Deviation Mean & SD", face = "plain")

aggregate(DeathsDeviation~CovidByState$"Governor's Party", FUN=mean)
aggregate(DeathsDeviation~CovidByState$"Governor's Party", FUN=sd)
aggregate(CasesDeviation~CovidByState$"Governor's Party", FUN=mean)
aggregate(CasesDeviation~CovidByState$"Governor's Party", FUN=sd)

# Checks for normality of CaseDeviations

# Q-Q plot of CaseDeviation
ggqqplot(CasesDeviation, color="blue", title = "Q-Q Plot of Cases Deviation")

# Check for bell shaped density plot
ggdensity(CasesDeviation, 
          main = "Density plot of COVID Case Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(CasesDeviation)
 
# Two sample independent t-test of cases deviation between Democrat and republican
tResults = t.test(CovidByState$CasesDeviation[CovidByState$"Governor's Party" == 'Democrat'], 
      CovidByState$CasesDeviation[CovidByState$"Governor's Party" == 'Republican'],
      alternative = c("two.sided", "less", "greater"),
      mu = 0, paired = FALSE, var.equal = FALSE,
      conf.level = 0.95)

# Print table of t-test results
str(tResults)
Statistic = c("Method", "Conf Level", "t", "df", "p-value", "Conf Int Low", 
              "Conf Int High", "Mean Dem", "Mean Rep") 

Value = tResults$method
Value = c(Value, "0.95") # problem fetching this from the results
Value = c(Value, round(tResults$statistic, digits = 3))
Value = c(Value, round(tResults$parameter, digits = 2))
Value = c(Value, round(tResults$p.value, digits = 3))
Value = c(Value, round(tResults$conf.int[1], digits = 3))
Value = c(Value, round(tResults$conf.int[2], digits = 3))
Value = c(Value, round(tResults$estimate[1], digits = 3))
Value = c(Value, round(tResults$estimate[2], digits = 3))
Value
report = cbind(Statistic, Value)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:10), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "t-test COVID Cases, Rep vs Dem", face = "plain")

# Checks for normality of DeathsDeviations

# Q-Q plot of CaseDeviation
ggqqplot(DeathsDeviation, color="blue", title = "Q-Q Plot of Deaths Deviation")

# Check for bell shaped density plot
ggdensity(DeathsDeviation, 
          main = "Density plot of COVID Deaths Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(DeathsDeviation)

# Two sample independent t-test of deaths deviation between Democrat and republican
tResults = t.test(CovidByState$DeathsDeviation[CovidByState$"Governor's Party" == 'Democrat'], 
                  CovidByState$DeathsDeviation[CovidByState$"Governor's Party" == 'Republican'],
                  alternative = c("two.sided", "less", "greater"),
                  mu = 0, paired = FALSE, var.equal = FALSE,
                  conf.level = 0.95)

# Print table of t-test results
str(tResults)
Statistic = c("Method", "Conf Level", "t", "df", "p-value", "Conf Int Low", 
              "Conf Int High", "Mean Dem", "Mean Rep") 

Value = tResults$method
Value = c(Value, "0.95") # problem fetching this from the results
Value = c(Value, round(tResults$statistic, digits = 3))
Value = c(Value, round(tResults$parameter, digits = 2))
Value = c(Value, round(tResults$p.value, digits = 3))
Value = c(Value, round(tResults$conf.int[1], digits = 3))
Value = c(Value, round(tResults$conf.int[2], digits = 3))
Value = c(Value, round(tResults$estimate[1], digits = 3))
Value = c(Value, round(tResults$estimate[2], digits = 3))
Value
report = cbind(Statistic, Value)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:10), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "t-test COVID Deaths, Rep vs Dem", face = "plain")

# Print list of states by Governor's Party
DemocratSt = CovidByState$StateCode[CovidByState$"Governor's Party" == 'Democrat']
RepublicanSt = CovidByState$StateCode[CovidByState$"Governor's Party" == 'Republican']
DemocratStList = paste(DemocratSt, collapse = " ")
RepublicanStList = paste(RepublicanStList, collapse = " ")

States = c(DemocratStList, RepublicanStList)

report = cbind(Party, States)
t = ggtexttable(report,  rows = NULL,theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:3), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "States by Governor's Party", face = "plain")

# Why are case rates different then death rates
plot(CasesDeviation, 
     DeathsDeviation,
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, All States")
abline(lm(DeathsDeviation ~ CasesDeviation))

plot(CasesDeviation[CovidByState$"Governor's Party" == 'Democrat'], 
     DeathsDeviation[CovidByState$"Governor's Party" == 'Democrat'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Democratic")
abline(lm(DeathsDeviation[CovidByState$"Governor's Party" == 'Democrat'] ~ 
     CasesDeviation[CovidByState$"Governor's Party" == 'Democrat']))

plot(CasesDeviation[CovidByState$"Governor's Party" == 'Republican'], 
     DeathsDeviation[CovidByState$"Governor's Party" == 'Republican'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Republican")
abline(lm(DeathsDeviation[CovidByState$"Governor's Party" == 'Republican'] ~ 
    CasesDeviation[CovidByState$"Governor's Party" == 'Republican']))

