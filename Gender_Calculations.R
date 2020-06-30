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

#6 Aside from Facebook, what other social media sites do you use? Please check all that apply.----
#Instagram
#replacing NAs with the alternative value
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

#LinkedIn
lk <- as.data.frame(data$SM_USE_LK, stringsAsFactors = FALSE) #force to character type
names(lk)[1] <- "use" #rename column
lk <- lk$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, lk)
tbl
c_test <- chisq.test(tbl)
c_test

#Twitter
tw <- as.data.frame(data$SM_USE_TW, stringsAsFactors = FALSE) #force to character type
names(tw)[1] <- "use" #rename column
tw <- tw$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, tw)
tbl
c_test <- chisq.test(tbl)
c_test

#YouTube
yt <- as.data.frame(data$SM_USE_YT, stringsAsFactors = FALSE) #force to character type
names(yt)[1] <- "use" #rename column
yt <- yt$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, yt)
tbl
c_test <- chisq.test(tbl)
c_test

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

#7 Which devices do you use to check social media? Please check all that apply.----
#PC
pc <- as.data.frame(data$DEVICE_PC, stringsAsFactors = FALSE) #force to character type
names(pc)[1] <- "use" #rename column
pc <- pc$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, pc)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Phone
ph <- as.data.frame(data$DEVICE_PHONE, stringsAsFactors = FALSE) #force to character type
names(ph)[1] <- "use" #rename column
ph <- ph$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, ph)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Tablet
ta <- as.data.frame(data$DEVICE_TABLET, stringsAsFactors = FALSE) #force to character type
names(ta)[1] <- "use" #rename column
ta <- ta$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, ta)
tbl
c_test <- chisq.test(tbl)
c_test

#Other had one "other" response and it was 'None', so analysis not performed

#8 Where do you access social media?----
#Home
hm <- as.data.frame(data$ACCESS_HOME, stringsAsFactors = FALSE) #force to character type
names(hm)[1] <- "use" #rename column
hm <- hm$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, hm)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Work
wk <- as.data.frame(data$ACCESS_WORK, stringsAsFactors = FALSE) #force to character type
names(wk)[1] <- "use" #rename column
wk <- wk$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, wk)
tbl
c_test <- chisq.test(tbl)
c_test

#School
sl <- as.data.frame(data$ACCESS_SCHOOL, stringsAsFactors = FALSE) #force to character type
names(sl)[1] <- "use" #rename column
sl <- sl$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, sl)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Other public location
ot <- as.data.frame(data$ACCESS_OTHER, stringsAsFactors = FALSE) #force to character type
names(ot)[1] <- "use" #rename column
ot <- ot$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, ot)
tbl
c_test <- chisq.test(tbl)
c_test

#9 Aside from Facebook, what other social media do you use to seek agriculture information? Please check all that apply.----
#Instagram
#replacing NAs with the alternative value
ig <- as.data.frame(data$AG_USE_IG, stringsAsFactors = FALSE) #force to character type
names(ig)[1] <- "use" #rename column
ig <- ig$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, ig)
tbl
c_test <- chisq.test(tbl)
c_test

#Pinterest
pi <- as.data.frame(data$AG_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, pi)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#LinkedIn
lk <- as.data.frame(data$AG_USE_LK, stringsAsFactors = FALSE) #force to character type
names(lk)[1] <- "use" #rename column
lk <- lk$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, lk)
tbl
c_test <- chisq.test(tbl)
c_test

#Twitter
tw <- as.data.frame(data$AG_USE_TW, stringsAsFactors = FALSE) #force to character type
names(tw)[1] <- "use" #rename column
tw <- tw$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, tw)
tbl
c_test <- chisq.test(tbl)
c_test

#YouTube
yt <- as.data.frame(data$AG_USE_YT, stringsAsFactors = FALSE) #force to character type
names(yt)[1] <- "use" #rename column
yt <- yt$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, yt)
tbl
c_test <- chisq.test(tbl)
c_test

#Reddit
re <- as.data.frame(data$AG_USE_RE, stringsAsFactors = FALSE) #force to character type
names(re)[1] <- "use" #rename column
re <- re$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, re)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Tumblr
tu <- as.data.frame(data$AG_USE_TU, stringsAsFactors = FALSE) #force to character type
names(tu)[1] <- "use" #rename column
tu <- tu$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, tu)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Flickr
fl <- as.data.frame(data$AG_USE_FL, stringsAsFactors = FALSE) #force to character type
names(fl)[1] <- "use" #rename column
fl <- fl$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, fl)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#Google+
go <- as.data.frame(data$AG_USE_GO, stringsAsFactors = FALSE) #force to character type
names(go)[1] <- "use" #rename column
go <- go$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, go)
tbl
c_test <- chisq.test(tbl)
c_test

#Snapchat
sc <- as.data.frame(data$AG_USE_SC, stringsAsFactors = FALSE) #force to character type
names(sc)[1] <- "use" #rename column
sc <- sc$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, sc)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#WhatsApp
wa <- as.data.frame(data$AG_USE_WA, stringsAsFactors = FALSE) #force to character type
names(wa)[1] <- "use" #rename column
wa <- wa$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, wa)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#WeChat
# No one chose this response

#LINE
# No one chose this response

#Viber
# No one chose this response


#10 Do you post questions about agriculture on Facebook?----
tbl <- table(data$GENDER, data$AG_QS_FB)
tbl

chi_test <- chisq.test(tbl)
chi_test

#11 If yes, where do you post questions about agriculture on Facebook?----
ag_yes <- data[which(data$AG_QS_FB=="Yes, I post questions about agriculture on Facebook."),]
ag_yes #only want those who answered yes to Q10

#In my status updates
ag_yes$AG_QS_STATUS <- ag_yes$AG_QS_STATUS %>%
  replace_na("Not Used")
tbl <- table(ag_yes$GENDER, ag_yes$AG_QS_STATUS)
tbl
chi_test <- chisq.test(tbl)
chi_test

#In a group
ag_yes$AG_QS_GROUP <- ag_yes$AG_QS_GROUP %>%
  replace_na("Not Used")
tbl <- table(ag_yes$GENDER, ag_yes$AG_QS_GROUP)
tbl
chi_test <- chisq.test(tbl)
chi_test

#On a page
ag_yes$AG_QS_PAGE <- ag_yes$AG_QS_PAGE %>%
  replace_na("Not Used")
tbl <- table(ag_yes$GENDER, ag_yes$AG_QS_PAGE)
tbl
chi_test <- chisq.test(tbl)
chi_test

#Other
ag_yes$AG_QS_OTHER <- ag_yes$AG_QS_OTHER %>%
  replace_na("Not Used")
tbl <- table(ag_yes$GENDER, ag_yes$AG_QS_OTHER)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

#12 The following are types of content you find on Facebook. How helpful are each of them in providing you with agriculture information?----

data2 <- as.data.frame(data, stringsAsFactors=FALSE) #made a copy of the data to test
data2[ ,63:74][ data2[ ,63:74] == "Not at all helpful" ] <- 1 #convert multiple columns to scores
data2[ ,63:74][ data2[ ,63:74] == "Slightly helpful" ] <- 2
data2[ ,63:74][ data2[ ,63:74] == "Moderately helpful" ] <- 3
data2[ ,63:74][ data2[ ,63:74] == "Very helpful" ] <- 4
data2[ ,63:74][ data2[ ,63:74] == "Extremely helpful" ] <- 5
data2[,63:74] <- sapply(data2[ ,63:74],as.numeric) #convert multiple columns to numeric type

#Advertisements
df <- cbind.data.frame(data2$GENDER, data2$HELP_AD)
names(df) <- c("gender","ads_score")
df <- na.omit(df)
#Table
tbl <- table(df$gender, df$ads_score)
tbl
#ANOVA
aov <- aov(ads_score ~ gender, data = df)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 

#Apps
df <- cbind.data.frame(data2$GENDER, data2$HELP_AP)
names(df) <- c("gender","apps_score")
df <- na.omit(df)
#Table
tbl <- table(df$gender, df$apps_score)
tbl
#ANOVA
aov <- aov(apps_score ~ gender, data = df)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 
