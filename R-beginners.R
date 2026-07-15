
#### Install Packages ####
# you only need to do this once per package!
install.packages(‘dplyr’)
install.packages(‘tidyr’)
install.packages(‘lubridate’)
install.packages(‘ggplot2’)

#### Load Packages ####
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# set working directory
setwd("~/R-beginners")

# check working directory
getwd()

# assign a variable
x <- 10

# basic math in R
y <- 398
z <- x * y
z

# strings
x <- c(1, 2, 3, 4, 5)

# math with strings
z <- x * y
z

# dataframes - spreadsheets for R
?data.frame
df <- data.frame(ID = 1:30, VAL = sample(51:80))

# each column in a dataframe can act as a string
df$V2 <- df$VAL * y
df$V2
df

data(iris)

#### data transformation ####
# load a csv file as a dataframe
biomass_PIE <- read.csv("Biomass_PIE.csv")
biomass_NI <- read.csv("Biomass_NI.csv")

# combine the two biomass dataframes into one
# WARNING!!! make sure both dataframes have the same column order before you bind them - 
# you could accidentally combine two columns with different types of data
biomass <- bind_rows(biomass_PIE, biomass_NI)

# you can also bind columns
biomass_col <- bind_cols(biomass_PIE, biomass_NI)

# but more useful is to use join and specify identifiers to join by
biomass_join <- full_join(x = biomass_PIE, y = biomass_NI, by = join_by(Plot))

# use tidyr to put biomass df into a "tidy" format
?pivot_longer
biomass_tidy <- biomass %>% pivot_longer(AGB_Live:BGB_Dead, names_to = "Biomass_Category", values_to = "Biomass")

# view first 6 rows of a dataframe
head(biomass_tidy)

# view last 6 rows of a dataframe
tail(biomass_tidy)

# view a specific row
biomass_tidy[50,]

# view a specific column
biomass_tidy[,4]
biomass_tidy$Biomass_Category

# combine two columns
biomass_unite <- biomass_tidy %>% unite("Datetime", Date:Time, sep = " ", remove = T)

# working with dates
class(biomass_unite$Datetime)
biomass_unite$Datetime <- mdy_hm(biomass_unite$Datetime)

biomass_unite$Year <- year(biomass_unite$Datetime)

# separate two columns
biomass_sep <- biomass_unite %>% separate(Datetime, c("Date", "Time"), sep = " ")

# rename all columns
names(biomass_tidy) <- c("Date", "Time", "Site", "Plot", "Subplot", "Biomass_Category", "Biomass")

# rename just one column
names(biomass_tidy)[names(biomass_tidy) == 'Subplot'] <- "Random_Subplot"

# find and replace values
biomass_tidy$Biomass_Category[biomass_tidy$Biomass_Category == "BGB_Dead"] <- "Dead BGB"

# summarize the data
biomass_summary <- biomass_tidy %>% group_by(Site, Biomass_Category) %>% summarize(Biomass_avg = mean(Biomass),
                                                                               Biomass_sd = sd(Biomass))
biomass_total <- biomass_tidy %>% group_by(Site, Plot) %>% summarize(Biomass_total = sum(Biomass))

# filter data based on a set of criteria
biomass_filter <- biomass_tidy %>% filter(Biomass_Category == "AGB_Live")
biomass_filter <- biomass_tidy %>% filter(Biomass > 100)

# use "&" to filter by two criteria - results will have rows that meet BOTH
biomass_filter <- biomass_tidy %>% filter(Biomass_Category == "AGB_Live" & Biomass > 100)

# use "|" to filter by two criteria - results will have rows that meet "EITHER"
biomass_filter <- biomass_tidy %>% filter(Biomass_Category == "AGB_Live" | Biomass_Category == "AGB_Dead")

# find specific rows that meet criteria
which(biomass_tidy$Plot == 12)

# select specific columns
biomass_sel <- biomass_tidy %>% select(Site, Plot, Biomass_Category, Biomass)

# or use "!" to remove specific columns
biomass_sel <- biomass_tidy %>% select(!Random_Subplot)

#### statistics ####

# linear model
fiddler_lm <- lm(Body_weight ~ Claw_weight, fiddler)
fiddler_lm
summary(fiddler_lm)

# t-test
# note: uses Welch's t-test by default, which does not assume equal variances
with(fiddler, t.test(Claw_weight[Range == "Historical"], Claw_weight[Range == "Expanded"]))
# If the p-value is less than 0.05, you reject the null hypothesis
# and conclude that the difference between the means is statistically significant

#### visualizing data ####
# plot data
ggplot(fiddler, 
       aes(x = as.character(Lat), y = Claw_weight)) +
  geom_boxplot()

ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight)) +
  geom_point()

# add a trendline
ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight)) +
  geom_point() +
  geom_smooth(method = "lm")

# grouping
ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight, color = Range)) +
  geom_point()

ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight, shape = Range)) +
  geom_point()

# grouping with trendlines
ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight, color = Range)) +
  geom_point() +
  geom_smooth(method = "lm")

# split into two graphs
ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight)) +
  geom_point() +
  facet_wrap(~Range)

# add labels
ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight, color = Range)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Body Weight (g)", y = "Claw Weight (g)", 
       title = "Fiddler crab body vs claw weight across historical and expanded ranges")

# themes
ggplot(fiddler, 
       aes(x = Body_weight, y = Claw_weight, color = Range)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Body Weight (g)", y = "Claw Weight (g)", 
       title = "Fiddler crab body vs claw weight across historical and expanded ranges") +
  theme_classic()

