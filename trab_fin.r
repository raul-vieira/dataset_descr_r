# importing database
library(readr)
db <- read_csv('MUP_IHP_RY23_P03_V10_DY21_PRVSVC.CSV')

# selecting interesting variables
library(dplyr)
df <- db %>% select(Provider_Name, Provider_City, Provider_State, 
                    Diagnosis_Definition, Diagnosis_Description, 
                    Total_Discharges, Avg_Covered_Charges, Avg_Total_Payments,
                    Avg_Medicare_Payments)
# database overview
summary(df)
str(df)
# counting States, Cities and Diagnosis
nlevels(as.factor(df$Provider_State))
nlevels(as.factor(df$Provider_City))
nlevels(as.factor(df$Diagnosis_Definition))

# which is the most expensive service
which.max(df$Avg_Covered_Charges)
df$Diagnosis_Description[18746]
# LIVER TRANSPLANT WITH MCC OR INTESTINAL TRANSPLANT is the most expensive service registered

# defining function to remove outliers
trim <- function(x){
  x[(x > quantile(x, probs = c(0.25), na.rm=TRUE)-1.5*IQR(x, na.rm=TRUE)) & (x < quantile(x, probs = c(0.75), na.rm=TRUE)+1.5*IQR(x,na.rm=TRUE))]
}

# histograms of numerical variables
hist(df$Total_Discharges, xlab = 'Payments ($)', main = 'Average Medicare Payments')
hist(trim(df$Total_Discharges), xlab = 'Payments ($)', main = 'Average Medicare Payments w/o Outliers')

hist(df$Avg_Covered_Charges, xlab = 'Charges ($)', main = 'Average Covered Charges')
hist(trim(df$Avg_Covered_Charges), xlab = 'Charges ($)', main = 'Average Covered Charges w/o Outliers')

hist(df$Avg_Total_Payments, xlab = 'Payments ($)', main = 'Average Total Payments')
hist(trim(df$Avg_Total_Payments), xlab = 'Payments ($)', main = 'Average Total Payments w/o Outliers')

hist(df$Avg_Medicare_Payments, xlab = 'Payments ($)', main = 'Average Medicare Payments')
hist(trim(df$Avg_Medicare_Payments), xlab = 'Payments ($)', main = 'Average Medicare Payments w/o Outliers')
# with them we can see the most frequent values for discharges, charges, total and medicare payments

# creating vectors for each US region 
northeast <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
south <- c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV")
midwest <- c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI")
west <- c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")

# creating new dataframes with data from each US region
df_northeast <- df %>% filter(Provider_State %in% northeast)
df_south <- df %>% filter(Provider_State %in% south)
df_midwest <- df %>% filter(Provider_State %in% midwest)
df_west <- df %>% filter(Provider_State %in% west)

# verifying total discharges for each US region
northeast_disch <- sum(df_northeast$Total_Discharges)
south_disch <- sum(df_south$Total_Discharges)
midwest_disch <- sum(df_midwest$Total_Discharges)
west_disch <- sum(df_west$Total_Discharges)

# plotting total discharges per region
library(ggplot2)
disch_region <- data.frame(Region = c("Northeast", "South", "Midwest", "West"), Discharges = c(1075369, 1542689, 1182383, 839602))
ggplot(disch_region, aes(x = Region, y = Discharges)) +  geom_bar(stat = "identity", fill='darkgreen') + labs(x = "Region", y = "Discharges", title = "Discharges per Region")
# South has more number of discharges than the others

# verifying number of providers per region
providers_northeast <- nlevels(as.factor(df_northeast$Provider_Name))
providers_south <- nlevels(as.factor(df_south$Provider_Name))
providers_midwest <- nlevels(as.factor(df_midwest$Provider_Name))
providers_west <- nlevels(as.factor(df_west$Provider_Name))

# plotting number of providers per region
providers_region <- data.frame(Region = c("Northeast", "South", "Midwest", "West"), Providers = c(providers_northeast,providers_south,providers_midwest,providers_west))
ggplot(providers_region, aes(x=Region, y=Providers)) + geom_bar(stat = 'identity', fill = '#20B2AA')
# south region has more providers, northeast has less

# plotting discharges related to medicare payments
install.packages('tidyverse')
library(tidyverse)
ggplot(data = df_northeast) + geom_point(mapping = aes(x=Total_Discharges, y=Avg_Medicare_Payments, color=Provider_State))
ggplot(data = df_south) + geom_point(mapping = aes(x=Total_Discharges, y=Avg_Medicare_Payments, color=Provider_State))
ggplot(data = df_midwest) + geom_point(mapping = aes(x=Total_Discharges, y=Avg_Medicare_Payments, color=Provider_State))
ggplot(data = df_west) + geom_point(mapping = aes(x=Total_Discharges, y=Avg_Medicare_Payments, color=Provider_State))
# for higher costs of providers services, less discharges are registered. Cheaper treatments are more common.
# REMOVE OUTLIERS LATER TO A BETTER VISUALIZATION

#plotting covered charges related to medicare payments
ggplot(data = df) + geom_point(mapping = aes(x=Avg_Covered_Charges, y=Avg_Medicare_Payments), color = '#40E0D0') + geom_smooth(mapping = aes(x=Avg_Covered_Charges, y=Avg_Medicare_Payments))
cor(df$Avg_Covered_Charges, df$Avg_Medicare_Payments) # high correlation: +0.81
# high positive correlation, means that for higher charges, higher medicare payments

#plotting total payments related to medicare payments
ggplot(data = df) + geom_point(mapping = aes(x=Avg_Total_Payments, y=Avg_Medicare_Payments), color = '#BA55D3') + geom_smooth(mapping = aes(x=Avg_Total_Payments, y=Avg_Medicare_Payments))
cor(df$Avg_Total_Payments, df$Avg_Medicare_Payments) # very high correlation: +0.98

# creating column 'Region' on the separated data frames by region
library(dplyr)
df_northeast <- mutate(df_northeast, Region = 'Northeast')
df_south <- mutate(df_south, Region = 'South')
df_midwest <- mutate(df_midwest, Region = 'Midwest')
df_west <- mutate(df_west, Region = 'West')

# Join multiple data.frames
df <- rbind(df_northeast,df_south,df_midwest,df_west)

# box plots grouped by region
library(ggplot2)
df$Region <- as.factor(df$Region)
bp1 <- ggplot(df, aes(x=Region, y=Total_Discharges, color=Region)) + geom_boxplot(outlier.alpha = 0) + coord_cartesian(ylim = c(5, 70))
bp2 <- ggplot(df, aes(x=Region, y=Avg_Covered_Charges, color=Region)) + geom_boxplot(outlier.alpha = 0) + coord_cartesian(ylim = c(0, 250000))
bp3 <- ggplot(df, aes(x=Region, y=Avg_Total_Payments, color=Region)) + geom_boxplot(outlier.alpha = 0) + coord_cartesian(ylim = c(0, 50000))
bp4 <- ggplot(df, aes(x=Region, y=Avg_Medicare_Payments, color=Region)) + geom_boxplot(outlier.alpha = 0) + coord_cartesian(ylim = c(0, 45000))

# plotting histograms of numeric variables
h1 <- ggplot(data = df, aes(x = Total_Discharges)) + geom_histogram(fill = '#9ACD32') + xlim(0, 100)
h2 <- ggplot(data = df, aes(x = Avg_Covered_Charges)) + geom_histogram(fill = '#B8860B') + xlim(0, 300000)
h3 <- ggplot(data = df, aes(x = Avg_Total_Payments)) + geom_histogram(fill = '#DA70D6') + xlim(0, 50000)
h4 <- ggplot(data = df, aes(x = Avg_Medicare_Payments)) + geom_histogram(fill = '#FF6347') + xlim(0, 50000)
# arranging histograms
install.packages("ggpubr")
library(ggpubr)
ggarrange(h1,h2)
ggarrange(h3,h4)

#arranging boxplots
ggarrange(bp1,bp2)
ggarrange(bp3,bp4)
