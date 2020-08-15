#Member Calculations----
library(readr)
library(exact2x2)
library(tidyr)
library(MASS)

#Load data----
data <- read_csv("Survey Data 2-23-19_Demographics_Edited.csv")
View(data)

#1 How long have you used Facebook?----
#Chi-square test/Fisher's test
tbl <- table(data$MEMBER, data$FB_LONG_USE)
tbl

#Fisher's test
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#One-Way ANOVA
fb_long_num <- as.data.frame(data$FB_LONG_USE, stringsAsFactors = FALSE)
names(fb_long_num)[1] <- "time"
fb_long_num$time[fb_long_num$time == "3+ years"] <- 3
fb_long_num$time[fb_long_num$time == "1-2 years"] <- 1.5
fb_long_num$time <- as.numeric(fb_long_num$time)

#Creating the new data frame
df_fb_long <- cbind.data.frame(fb_long_num=unlist(fb_long_num), data$MEMBER)
rownames(df_fb_long) <- NULL
names(df_fb_long)[1] <- "time"
names(df_fb_long)[2] <- "member"

#Doing the ANOVA
aov <- aov(time ~ member, data = df_fb_long)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 

#residuals
stdres(aov)

#2 How often do you use Facebook?----

#2x4 table - categorical
tbl <- table(data$MEMBER, data$FB_OFTEN_USE)
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

df_fb_often <- cbind.data.frame(fb_often_num=unlist(fb_often_num), data$MEMBER)
rownames(df_fb_often) <- NULL
names(df_fb_often)[1] <- "time"
names(df_fb_often)[2] <- "member"

#One-Way ANOVA 2x4 table
aov <- aov(time ~ member, data = df_fb_often)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 
#residuals
stdres(aov)

#3 How long have you used social media (including Facebook)?----
tbl <- table(data$MEMBER, data$SM_LONG_USE) 
tbl #counts are really small and only 1 answer different than Q1 which was highly unsignificant

#4 How often do you use other social media (not including Facebook)?----

#2x5 Table
tbl <- table(data$MEMBER, data$SM_OFTEN_USE)
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

df_sm_often <- cbind.data.frame(sm_often_num=unlist(sm_often_num), data$MEMBER)
rownames(df_sm_often) <- NULL
names(df_sm_often)[1] <- "time"
names(df_sm_often)[2] <- "member"

aov <- aov(time ~ member, data = df_sm_often)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 
#residuals
stdres(aov)

#5 How would you rate your current social media skills?----

#2x5 Table
tbl <- table(data$MEMBER, data$SM_SKILLS)
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

df_skills <- cbind.data.frame(skills=unlist(skills), data$MEMBER)
rownames(df_skills) <- NULL
names(df_skills)[1] <- "rating"
names(df_skills)[2] <- "member"

aov <- aov(rating ~ member, data = df_skills)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)
#residuals
stdres(aov)

#6 Aside from Facebook, what other social media sites do you use? Please check all that apply.----
#Instagram
#replacing NAs with the alternative value
ig <- as.data.frame(data$SM_USE_IG, stringsAsFactors = FALSE) #force to character type
names(ig)[1] <- "use" #rename column
ig <- ig$use %>%
  replace_na("Not Used")
tbl <- table(data$MEMBER, ig)
tbl
c_test <- chisq.test(tbl)
c_test

#Pinterest
pi <- as.data.frame(data$SM_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")
tbl <- table(data$MEMBER, pi)
tbl
c_test <- chisq.test(tbl)
c_test

#LinkedIn
lk <- as.data.frame(data$SM_USE_LK, stringsAsFactors = FALSE) #force to character type
names(lk)[1] <- "use" #rename column
lk <- lk$use %>%
  replace_na("Not Used")
tbl <- table(data$MEMBER, lk)
tbl
c_test <- chisq.test(tbl)
c_test

#Twitter
tw <- as.data.frame(data$SM_USE_TW, stringsAsFactors = FALSE) #force to character type
names(tw)[1] <- "use" #rename column
tw <- tw$use %>%
  replace_na("Not Used")
tbl <- table(data$MEMBER, tw)
tbl
c_test <- chisq.test(tbl)
c_test

#YouTube
yt <- as.data.frame(data$SM_USE_YT, stringsAsFactors = FALSE) #force to character type
names(yt)[1] <- "use" #rename column
yt <- yt$use %>%
  replace_na("Not Used")
tbl <- table(data$MEMBER, yt)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Reddit
re <- as.data.frame(data$SM_USE_RE, stringsAsFactors = FALSE) #force to character type
names(re)[1] <- "use" #rename column
re <- re$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, re)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Tumblr
tu <- as.data.frame(data$SM_USE_TU, stringsAsFactors = FALSE) #force to character type
names(tu)[1] <- "use" #rename column
tu <- tu$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, tu)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Flickr
fl <- as.data.frame(data$SM_USE_FL, stringsAsFactors = FALSE) #force to character type
names(fl)[1] <- "use" #rename column
fl <- fl$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, fl)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Google+
go <- as.data.frame(data$SM_USE_GO, stringsAsFactors = FALSE) #force to character type
names(go)[1] <- "use" #rename column
go <- go$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, go)
tbl
c_test <- chisq.test(tbl)
c_test

#Snapchat
sc <- as.data.frame(data$SM_USE_SC, stringsAsFactors = FALSE) #force to character type
names(sc)[1] <- "use" #rename column
sc <- sc$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, sc)
tbl
c_test <- chisq.test(tbl)
c_test

#WhatsApp
wa <- as.data.frame(data$SM_USE_WA, stringsAsFactors = FALSE) #force to character type
names(wa)[1] <- "use" #rename column
wa <- wa$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, wa)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#WeChat
wc <- as.data.frame(data$SM_USE_WC, stringsAsFactors = FALSE) #force to character type
names(wc)[1] <- "use" #rename column
wc <- wc$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, wc)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#LINE
# No one chose this response

#Viber
vb <- as.data.frame(data$SM_USE_VB, stringsAsFactors = FALSE) #force to character type
names(vb)[1] <- "use" #rename column
vb <- vb$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, vb)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test