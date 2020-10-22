#Reading in SPSS data----
library(readr)
data <- read_csv("Survey Data 2-23-19_Demographics_Edited.csv")
View(data)
head(data) #first 6 rows
tail(data) #last 6 rows
str(data) #structure
View(data) #view dataframe
fix(data) #manually edit cell content
cbind(names(data)) #look at column names to remove unneeded columns

#all data has been fixed, load the environment file for the fixed data, DO NOT RE-IMPORT UNLESS 100% NEEDED

#Visualize all the simple plots----
library(ggplot2)
library(dplyr)
library(plyr) #for count, rename
library(ggthemes) #for other plotting themes
library(tidyr)

#County farm bureau members----
county_freq <- data %>%
                group_by(COUNTY) %>%
                summarise(Count = length(COUNTY))
county_freq <- county_freq[-c(38),] #removed NAs row

ggplot(county_freq, aes(x=reorder(COUNTY,Count), Count)) + 
  geom_bar(stat="identity", fill="red", colour="black", width=.7, size=1) +
  coord_flip() +
  scale_y_continuous(limits=c(0,14), breaks=c(0,2,4,6,8,10,12,14)) +
  labs(title="Count of County Farm Bureau Members in Illinois", caption="Total Count = 97") +
  theme_fivethirtyeight()

#gender----
gender_freq <- data %>%
  group_by(GENDER) %>%
  summarise(Count = length(GENDER))

ggplot(gender_freq, aes(x=reorder(GENDER, -Count), y=Count)) + 
  geom_bar(stat="identity", fill="gold", colour="black", width=.7, size=1) +
  labs(title="Gender Split", caption="Total Count = 115") +
  theme_fivethirtyeight()

#age---
age_freq <- data %>%
  group_by(AGE) %>%
  summarise(Count = length(AGE))

ggplot(age_freq, aes(x=AGE, y=Count)) + 
  geom_bar(stat="identity", fill="coral", colour="black", width=.7, size=1) +
  #coord_flip() +
  labs(title="Age Groups", caption="Total Count = 115") +
  theme_fivethirtyeight()

#living area----
living_area_freq <- data %>%
                      group_by(LIVE_AREA) %>%
                      summarise(Count = length(LIVE_AREA))

ggplot(living_area_freq, aes(x=LIVE_AREA, y=Count)) + 
  geom_bar(stat="identity", fill="forestgreen", colour="black", width=.7, size=1) +
  #coord_flip() +
  labs(title="Type of Living Area", caption="Total Count = 115") +
  theme_fivethirtyeight()

#education----
education_freq <- data %>%
                    group_by(EDUCATION) %>%
                    summarise(Count = length(EDUCATION))

ggplot(education_freq, aes(x=EDUCATION, y=Count)) + 
  geom_bar(stat="identity", fill="blue", colour="black", width=.7, size=1) +
  coord_flip() +
  scale_y_continuous(limits=c(0,60), breaks=c(0,10,20,30,40,50,60)) +
  labs(title="Level of Education", caption="Total Count = 115") +
  theme_fivethirtyeight()
  
#length of time in ag----
ag_length_freq <- data %>%
  group_by(AG_EXPERIENCE) %>%
  summarise(Count = length(AG_EXPERIENCE))

ggplot(ag_length_freq, aes(x=AG_EXPERIENCE, y=Count)) + 
  geom_bar(stat="identity", fill="purple", colour="black", width=.7, size=1) +
  #coord_flip() +
  scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40,50)) +
  labs(title="Length of Time in Agriculture", caption="Total Count = 115") +
  theme_fivethirtyeight()

#income----
income_freq <- data %>%
  group_by(INCOME) %>%
  summarise(Count = length(INCOME))

ggplot(income_freq, aes(x=INCOME, y=Count)) + 
  geom_bar(stat="identity", fill="tan1", colour="black", width=.7, size=1) +
  #coord_flip() +
  scale_y_continuous(limits=c(0,40), breaks=c(0,10,20,30,40)) +
  labs(title="Total Household Income", caption="Total Count = 115") +
  theme_fivethirtyeight()

#area of ag----
ag_area_freq <- data %>%
  group_by(AG_AREA) %>%
  summarise(Count = length(AG_AREA))

ggplot(ag_area_freq, aes(x=reorder(AG_AREA, Count), y=Count)) + 
  geom_bar(stat="identity", fill="springgreen", colour="black", width=.7, size=1) +
  coord_flip() +
  scale_y_continuous(limits=c(0,40), breaks=c(0,10,20,30,40)) +
  labs(title="Area of Ag Worked In", caption="Total Count = 115") +
  theme_fivethirtyeight()

#farming income----
farm_income_freq <- data %>%
  group_by(INCOME_FARMING) %>%
  summarise(Count = length(INCOME_FARMING))
farm_income_freq <- farm_income_freq[-c(6),] #removed NAs row

ggplot(farm_income_freq, aes(x=INCOME_FARMING, y=Count)) + 
  geom_bar(stat="identity", fill="orchid", colour="black", width=.7, size=1) +
  #coord_flip() +
  scale_y_continuous(limits=c(0,20), breaks=c(0,5,10,15,20)) +
  labs(title="Total Farming Income", caption="Total Count = 56") +
  theme_fivethirtyeight()

#theme(axis.text.x = element_text(angle=20, hjust=1))

#replacing NAs with the alternative value----
yt <- as.data.frame(data$AG_USE_YT, stringsAsFactors = FALSE) #force to character type
names(yt)[1] <- "use" #rename column

yt <- yt$use %>%
        replace_na("Not Used")

#chi-square test----
#need to make sure the data is turned into a contigency table first
#have to have minimum of 5 entries per entry to get the tables to work
tbl <- table(data$GENDER, yt)
tbl

c_test <- chisq.test(tbl)
c_test

chisq.test(tbl)$expected #get expected counts

#fisher's test----
#if cell counts are less than 5
# $ arguments: p.value, conf.int, estimate, null.value, alternative, method, data.name
f_test <- fisher.test(tbl)
f_test

#use exact2x2 library to get a p-value that matches the confidence interval (2-sided)
f_test2 <- exact2x2(tbl, tsmethod = "central")
f_test2

#need to drop unused factor/level to do chisquare right
dropped <- data$GENDER[data$GENDER !="Other"]
summary(dropped)
droplevels(dropped)
data$GENDER <- droplevels(dropped) #THIS WORKS

test$p.value
