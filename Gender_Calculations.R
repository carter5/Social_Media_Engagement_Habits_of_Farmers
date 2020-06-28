#Gender Calculations----
library(readr)
library(exact2x2)
library(tidyr)

#Load data----
data <- read_csv("Survey Data 2-23-19_Demographics_Edited.csv")
View(data)

#Create gender variable----
gender <- as.data.frame(data$GENDER, stringsAsFactors = FALSE)
names(gender)[1] <- "label" #rename column

#1 How long have you used Facebook?----

#Chi-square test/Fisher's test
#Create the categorical table w/ original dataframe
tbl <- table(data$GENDER, data$FB_LONG_USE)
tbl

#Chi-square test
chi_test <- chisq.test(tbl) #do not do if cells have < 5 count
chi_test

#Fisher's test, f cells have < 5 count
f_test <- exact2x2(tbl, tsmethod = "central") #can only be used with 2x2 tables
f_test

#One-Way ANOVA
#Create variable w/numeric category
fb_long_num <- as.data.frame(data$FB_LONG_USE, stringsAsFactors = FALSE)
names(fb_long_num)[1] <- "time" #rename column
fb_long_num$time[fb_long_num$time == "3+ years"] <- 3 #use this to replace values
fb_long_num$time[fb_long_num$time == "1-2 years"] <- 1.5
fb_long_num$time <- as.numeric(fb_long_num$time) #convert to numeric values

#Creating the new data frame
df_fb_long <- cbind.data.frame(fb_long_num=unlist(fb_long_num), gender)
rownames(df_fb_long) <- NULL #remove the odd labeling
names(df_fb_long)[1] <- "time"

#Doing the ANOVA
aov <- aov(time ~ label, data = df_fb_long) #number X category (variable order)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 

#2 How often do you use Facebook?----

#2x4 table - categorical
tbl <- table(data$GENDER, data$FB_OFTEN_USE)
tbl

chi_test <- chisq.test(tbl)
chi_test

#2x4 table - numerical
fb_often_num <- as.data.frame(data$FB_OFTEN_USE, stringsAsFactors = FALSE)
names(fb_often_num)[1] <- "time" #rename column
fb_often_num$time[fb_often_num$time == "A few times a month or less"] <- 27
fb_often_num$time[fb_often_num$time == "Every day or two"] <- 365
fb_often_num$time[fb_often_num$time == "Once a week"] <- 54
fb_often_num$time[fb_often_num$time == "Several times a day"] <- 730
fb_often_num$time <- as.numeric(fb_often_num$time) #convert to numeric values

df_fb_often <- cbind.data.frame(fb_often_num=unlist(fb_often_num), gender)
rownames(df_fb_often) <- NULL
names(df_fb_often)[1] <- "time"

#One-Way ANOVA 2x4 table
aov <- aov(time ~ label, data = df_fb_often)
summary(aov)

#2x2 table - categorical (combined data)
less_fb <- data[which(data$FB_OFTEN_USE=='A few times a month or less' | data$FB_OFTEN_USE=="Once a week" ),]
more_fb <- data[which(data$FB_OFTEN_USE=='Every day or two' | data$FB_OFTEN_USE=="Several times a day" ),]
less_fb$FB_OFTEN_USE <- "Once a week or less"
more_fb$FB_OFTEN_USE <- "Every day or two, or more"
df_fb_often_2x2 <- rbind.data.frame(less_fb, more_fb)

tbl <- table(df_fb_often_2x2$GENDER, df_fb_often_2x2$FB_OFTEN_USE)
tbl

f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#2x2 table - ANOVA
df_fb_often_2x2$FB_OFTEN_USE[df_fb_often_2x2$FB_OFTEN_USE == "Once a week or less"] <- 40.5
df_fb_often_2x2$FB_OFTEN_USE[df_fb_often_2x2$FB_OFTEN_USE == "Every day or two, or more"] <- 547.5
df_fb_often_2x2$FB_OFTEN_USE <- as.numeric(df_fb_often_2x2$FB_OFTEN_USE) #convert to numeric values

#Doing the ANOVA
aov <- aov(FB_OFTEN_USE ~ GENDER, data = df_fb_often_2x2) #number X category (variable order)
summary(aov)

#3 How long have you used social media (including Facebook)?----

tbl <- table(data$GENDER, data$SM_LONG_USE) 
tbl #counts are really small and only 1 answer different than Q1 which was highly unsignificant

#4 How often do you use other social media (not including Facebook)?----

#2x5 Table
tbl <- table(data$GENDER, data$SM_OFTEN_USE)
tbl

#2x5 Table - ANOVA
sm_often_num <- as.data.frame(data$SM_OFTEN_USE, stringsAsFactors = FALSE)
names(sm_often_num)[1] <- "time" #rename column
sm_often_num$time[sm_often_num$time == "A few times a month or less"] <- 27
sm_often_num$time[sm_often_num$time == "Every day or two"] <- 365
sm_often_num$time[sm_often_num$time == "Once a week"] <- 54
sm_often_num$time[sm_often_num$time == "Several times a day"] <- 730
sm_often_num$time[sm_often_num$time == "Never"] <- 0
sm_often_num$time <- as.numeric(sm_often_num$time) #convert to numeric values

df_sm_often <- cbind.data.frame(sm_often_num=unlist(sm_often_num), gender)
rownames(df_sm_often) <- NULL
names(df_sm_often)[1] <- "time"

aov <- aov(time ~ label, data = df_sm_often)
summary(aov)

#2x2 Table
less_sm <- data[which(data$SM_OFTEN_USE=="Never" | data$SM_OFTEN_USE=='A few times a month or less' | data$SM_OFTEN_USE=="Once a week"),]
more_sm <- data[which(data$SM_OFTEN_USE=='Every day or two' | data$SM_OFTEN_USE=="Several times a day" ),]
less_sm$SM_OFTEN_USE <- "Once a week or less"
more_sm$SM_OFTEN_USE <- "Every day or two, or more"
df_sm_often_2x2 <- rbind.data.frame(less_sm, more_sm)

tbl <- table(df_sm_often_2x2$GENDER, df_sm_often_2x2$SM_OFTEN_USE)
tbl

chi_test <- chisq.test(tbl)
chi_test

#2x2 Table - ANOVA
df_sm_often_2x2$SM_OFTEN_USE[df_sm_often_2x2$SM_OFTEN_USE == "Once a week or less"] <- 27
df_sm_often_2x2$SM_OFTEN_USE[df_sm_often_2x2$SM_OFTEN_USE == "Every day or two, or more"] <- 547.5
df_sm_often_2x2$SM_OFTEN_USE <- as.numeric(df_sm_often_2x2$SM_OFTEN_USE) #convert to numeric values

#Doing the ANOVA
aov <- aov(SM_OFTEN_USE ~ GENDER, data = df_sm_often_2x2) #number X category (variable order)
summary(aov)

#5 How would you rate your current social media skills?----

#2x5 Table
tbl <- table(data$GENDER, data$SM_SKILLS)
tbl

#2x5 Table - ANOVA
skills <- as.data.frame(data$SM_SKILLS, stringsAsFactors = FALSE)
names(skills)[1] <- "rating" #rename column
skills$rating[skills$rating == "Poor"] <- 1
skills$rating[skills$rating == "Fair"] <- 2
skills$rating[skills$rating == "Good"] <- 3
skills$rating[skills$rating == "Very good"] <- 4
skills$rating[skills$rating == "Excellent"] <- 5
skills$rating <- as.numeric(skills$rating) #convert to numeric values

df_skills <- cbind.data.frame(skills=unlist(skills), gender)
rownames(df_skills) <- NULL
names(df_skills)[1] <- "rating"

aov <- aov(rating ~ label, data = df_skills)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 

#6. Aside from Facebook, what other social media sites do you use? Please check all that apply.
#Instagram
#replacing NAs with the alternative value----
ig <- as.data.frame(data$SM_USE_IG, stringsAsFactors = FALSE) #force to character type
names(ig)[1] <- "use" #rename column
ig <- ig$use %>%
  replace_na("Not Used")

tbl <- table(data$GENDER, ig)
tbl

c_test <- chisq.test(tbl)
c_test

#Pinterest
pi <- as.data.frame(data$SM_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")

tbl <- table(data$GENDER, pi)
tbl

c_test <- chisq.test(tbl)
c_test

