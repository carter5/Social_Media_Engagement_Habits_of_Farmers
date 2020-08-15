#Age Calculations----
library(readr)
library(exact2x2)
library(tidyr)
library(MASS) #grabbing residuals

#Load data----
data <- read_csv("Survey Data 2-23-19_Demographics_Edited.csv")
View(data)

#Create Age variable----
age <- as.data.frame(data$AGE, stringsAsFactors = FALSE)
names(age)[1] <- "label" #rename column

#1 How long have you used Facebook?----

#Chi-square test/Fisher's test
#Create the categorical table w/ original dataframe
tbl <- table(data$AGE, data$FB_LONG_USE)
tbl

#One-Way ANOVA
#Create numeric variables for age and use
fb_long_num <- as.data.frame(data$FB_LONG_USE, stringsAsFactors = FALSE)
names(fb_long_num)[1] <- "time" #rename column
fb_long_num$time[fb_long_num$time == "3+ years"] <- 3 #use this to replace values
fb_long_num$time[fb_long_num$time == "1-2 years"] <- 1.5
fb_long_num$time <- as.numeric(fb_long_num$time) #convert to numeric values

age_num <- as.data.frame(data$AGE, stringsAsFactors = FALSE)
names(age_num)[1] <- "label" #rename column
age_num$label[age_num$label == "18-21"] <- 19.5 #use this to replace values
age_num$label[age_num$label == "22-25"] <- 23.5
age_num$label[age_num$label == "26-30"] <- 28
age_num$label[age_num$label == "31-40"] <- 35.5
age_num$label[age_num$label == "41-50"] <- 45.5
age_num$label[age_num$label == "51-60"] <- 55.5
age_num$label[age_num$label == "61 or over"] <- 61
age_num$label <- as.numeric(age_num$label) #convert to numeric values

#Creating the new data frames
df_fb_long <- cbind.data.frame(fb_long_num=unlist(fb_long_num), age) #age category x time
rownames(df_fb_long) <- NULL #remove the odd labeling
names(df_fb_long)[1] <- "time"

df_fb_long2 <- cbind.data.frame(age_num=unlist(age_num),data$FB_LONG_USE) #time category x age
rownames(df_fb_long2) <- NULL #remove the odd labeling
names(df_fb_long2)[1] <- "label"
names(df_fb_long2)[2] <- "use"

#Doing the ANOVA
aov <- aov(time ~ label, data = df_fb_long) #number X category (variable order)
summary(aov)

aov2 <- aov(label ~ use, data = df_fb_long2)
summary(aov2)

#2 How often do you use Facebook?----

#7x4 table - categorical
tbl <- table(data$AGE, data$FB_OFTEN_USE)
tbl

#7x4 table - numerical
fb_often_num <- as.data.frame(data$FB_OFTEN_USE, stringsAsFactors = FALSE)
names(fb_often_num)[1] <- "time" #rename column
fb_often_num$time[fb_often_num$time == "A few times a month or less"] <- 27
fb_often_num$time[fb_often_num$time == "Every day or two"] <- 365
fb_often_num$time[fb_often_num$time == "Once a week"] <- 54
fb_often_num$time[fb_often_num$time == "Several times a day"] <- 730
fb_often_num$time <- as.numeric(fb_often_num$time) #convert to numeric values

df_fb_often <- cbind.data.frame(fb_often_num=unlist(fb_often_num), age)
rownames(df_fb_often) <- NULL
names(df_fb_often)[1] <- "time"

df_fb_often2 <- cbind.data.frame(age_num=unlist(age_num), data$FB_OFTEN_USE)
rownames(df_fb_often2) <- NULL
names(df_fb_often2)[1] <- "age"
names(df_fb_often2)[2] <- "use"

#One-Way ANOVA
aov <- aov(time ~ label, data = df_fb_often)
summary(aov)

aov2 <- aov(age ~ use, data = df_fb_often2)
summary(aov2)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov2) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov2, 2)

#print the model tables from the anova
print(model.tables(aov2,"means"),digits=3) 

#get residuals
stdres(aov2)

#3 How long have you used social media (including Facebook)?----

tbl <- table(data$AGE, data$SM_LONG_USE) 
tbl #counts are really small and only 1 answer different than Q1 which was highly unsignificant, just gonna throw out this question, no analysis

#4 How often do you use other social media (not including Facebook)?----

#7x5 Table
tbl <- table(data$AGE, data$SM_OFTEN_USE)
tbl

#7x5 Table - ANOVA
sm_often_num <- as.data.frame(data$SM_OFTEN_USE, stringsAsFactors = FALSE)
names(sm_often_num)[1] <- "time" #rename column
sm_often_num$time[sm_often_num$time == "A few times a month or less"] <- 27
sm_often_num$time[sm_often_num$time == "Every day or two"] <- 365
sm_often_num$time[sm_often_num$time == "Once a week"] <- 54
sm_often_num$time[sm_often_num$time == "Several times a day"] <- 730
sm_often_num$time[sm_often_num$time == "Never"] <- 0
sm_often_num$time <- as.numeric(sm_often_num$time) #convert to numeric values

df_sm_often <- cbind.data.frame(sm_often_num=unlist(sm_often_num), age) #time numeric
rownames(df_sm_often) <- NULL
names(df_sm_often)[1] <- "time"
names(df_sm_often)[2] <- "age"

df_sm_often2 <- cbind.data.frame(age_num=unlist(age_num), data$SM_OFTEN_USE) #age numeric
rownames(df_sm_often2) <- NULL
names(df_sm_often2)[1] <- "age"
names(df_sm_often2)[2] <- "time"

aov <- aov(time ~ age, data = df_sm_often)
summary(aov)

aov2 <- aov(age ~ time, data = df_sm_often2)
summary(aov2)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

tukey2 <- TukeyHSD(aov2) #with 95% conf.level
tukey2
plot(tukey2)

#check for normality
plot(aov, 2)
plot(aov2, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)
print(model.tables(aov2,"means"),digits=3) 

#get residuals
stdres(aov)
stdres(aov2)

#5 How would you rate your current social media skills?----

#7x5 Table
tbl <- table(data$AGE, data$SM_SKILLS)
tbl

#7x5 Table - ANOVA
skills <- as.data.frame(data$SM_SKILLS, stringsAsFactors = FALSE)
names(skills)[1] <- "rating" #rename column
skills$rating[skills$rating == "Poor"] <- 1
skills$rating[skills$rating == "Fair"] <- 2
skills$rating[skills$rating == "Good"] <- 3
skills$rating[skills$rating == "Very good"] <- 4
skills$rating[skills$rating == "Excellent"] <- 5
skills$rating <- as.numeric(skills$rating) #convert to numeric values

df_skills <- cbind.data.frame(skills=unlist(skills), age) #skills numeric
rownames(df_skills) <- NULL
names(df_skills)[1] <- "rating"
names(df_skills)[2] <- "age"

df_skills2 <- cbind.data.frame(age_num=unlist(age_num), data$SM_SKILLS) #age numeric
rownames(df_skills2) <- NULL
names(df_skills2)[1] <- "age"
names(df_skills2)[2] <- "rating"

aov <- aov(rating ~ age, data = df_skills)
summary(aov)

aov2 <- aov(age ~ rating, data = df_skills2)
summary(aov2)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

tukey2 <- TukeyHSD(aov2) #with 95% conf.level
tukey2
plot(tukey2)

#check for normality
plot(aov, 2)
plot(aov2, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)
print(model.tables(aov2,"means"),digits=3)

#get residuals
stdres(aov)
stdres(aov2)

#6 Aside from Facebook, what other social media sites do you use? Please check all that apply.----
#Instagram
#replacing NAs with the alternative value
ig <- as.data.frame(data$SM_USE_IG, stringsAsFactors = FALSE) #force to character type
names(ig)[1] <- "use" #rename column
ig <- ig$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, ig)
tbl
c_test <- chisq.test(tbl)
c_test

df_ig <- cbind.data.frame(age_num=unlist(age_num), ig) #age numeric
rownames(df_ig) <- NULL
names(df_ig)[1] <- "age"
names(df_ig)[2] <- "use"

aov <- aov(age ~ use, data = df_ig)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Pinterest
pi <- as.data.frame(data$SM_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, pi)
tbl
c_test <- chisq.test(tbl)
c_test

df_pi <- cbind.data.frame(age_num=unlist(age_num), pi) #age numeric
rownames(df_pi) <- NULL
names(df_pi)[1] <- "age"
names(df_pi)[2] <- "use"

aov <- aov(age ~ use, data = df_pi)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#LinkedIn
lk <- as.data.frame(data$SM_USE_LK, stringsAsFactors = FALSE) #force to character type
names(lk)[1] <- "use" #rename column
lk <- lk$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, lk)
tbl
c_test <- chisq.test(tbl)
c_test

df_lk <- cbind.data.frame(age_num=unlist(age_num), lk) #age numeric
rownames(df_lk) <- NULL
names(df_lk)[1] <- "age"
names(df_lk)[2] <- "use"

aov <- aov(age ~ use, data = df_lk)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Twitter
tw <- as.data.frame(data$SM_USE_TW, stringsAsFactors = FALSE) #force to character type
names(tw)[1] <- "use" #rename column
tw <- tw$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, tw)
tbl
c_test <- chisq.test(tbl)
c_test

df_tw <- cbind.data.frame(age_num=unlist(age_num), tw) #age numeric
rownames(df_tw) <- NULL
names(df_tw)[1] <- "age"
names(df_tw)[2] <- "use"

aov <- aov(age ~ use, data = df_tw)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#YouTube
yt <- as.data.frame(data$SM_USE_YT, stringsAsFactors = FALSE) #force to character type
names(yt)[1] <- "use" #rename column
yt <- yt$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, yt)
tbl
c_test <- chisq.test(tbl)
c_test

df_yt <- cbind.data.frame(age_num=unlist(age_num), yt) #age numeric
rownames(df_yt) <- NULL
names(df_yt)[1] <- "age"
names(df_yt)[2] <- "use"

aov <- aov(age ~ use, data = df_yt)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Reddit
re <- as.data.frame(data$SM_USE_RE, stringsAsFactors = FALSE) #force to character type
names(re)[1] <- "use" #rename column
re <- re$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, re)
tbl
c_test <- chisq.test(tbl)
c_test

df_re <- cbind.data.frame(age_num=unlist(age_num), re) #age numeric
rownames(df_re) <- NULL
names(df_re)[1] <- "age"
names(df_re)[2] <- "use"

aov <- aov(age ~ use, data = df_re)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Tumblr
tu <- as.data.frame(data$SM_USE_TU, stringsAsFactors = FALSE) #force to character type
names(tu)[1] <- "use" #rename column
tu <- tu$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, tu)
tbl
c_test <- chisq.test(tbl)
c_test

df_tu <- cbind.data.frame(age_num=unlist(age_num), tu) #age numeric
rownames(df_tu) <- NULL
names(df_tu)[1] <- "age"
names(df_tu)[2] <- "use"

aov <- aov(age ~ use, data = df_tu)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Flickr
fl <- as.data.frame(data$SM_USE_FL, stringsAsFactors = FALSE) #force to character type
names(fl)[1] <- "use" #rename column
fl <- fl$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, fl)
tbl
c_test <- chisq.test(tbl)
c_test

df_fl <- cbind.data.frame(age_num=unlist(age_num), fl) #age numeric
rownames(df_fl) <- NULL
names(df_fl)[1] <- "age"
names(df_fl)[2] <- "use"

aov <- aov(age ~ use, data = df_fl)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Google+
go <- as.data.frame(data$SM_USE_GO, stringsAsFactors = FALSE) #force to character type
names(go)[1] <- "use" #rename column
go <- go$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, go)
tbl
c_test <- chisq.test(tbl)
c_test

df_go <- cbind.data.frame(age_num=unlist(age_num), go) #age numeric
rownames(df_go) <- NULL
names(df_go)[1] <- "age"
names(df_go)[2] <- "use"

aov <- aov(age ~ use, data = df_go)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Snapchat
sc <- as.data.frame(data$SM_USE_SC, stringsAsFactors = FALSE) #force to character type
names(sc)[1] <- "use" #rename column
sc <- sc$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, sc)
tbl
c_test <- chisq.test(tbl)
c_test

df_sc <- cbind.data.frame(age_num=unlist(age_num), sc) #age numeric
rownames(df_sc) <- NULL
names(df_sc)[1] <- "age"
names(df_sc)[2] <- "use"

aov <- aov(age ~ use, data = df_sc)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#WhatsApp
wa <- as.data.frame(data$SM_USE_WA, stringsAsFactors = FALSE) #force to character type
names(wa)[1] <- "use" #rename column
wa <- wa$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, wa)
tbl
c_test <- chisq.test(tbl)
c_test

df_wa <- cbind.data.frame(age_num=unlist(age_num), wa) #age numeric
rownames(df_wa) <- NULL
names(df_wa)[1] <- "age"
names(df_wa)[2] <- "use"

aov <- aov(age ~ use, data = df_wa)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#WeChat
wc <- as.data.frame(data$SM_USE_WC, stringsAsFactors = FALSE) #force to character type
names(wc)[1] <- "use" #rename column
wc <- wc$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, wc)
tbl
c_test <- chisq.test(tbl)
c_test

df_wc <- cbind.data.frame(age_num=unlist(age_num), wc) #age numeric
rownames(df_wc) <- NULL
names(df_wc)[1] <- "age"
names(df_wc)[2] <- "use"

aov <- aov(age ~ use, data = df_wc)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#LINE
# No one chose this response

#Viber
vb <- as.data.frame(data$SM_USE_VB, stringsAsFactors = FALSE) #force to character type
names(vb)[1] <- "use" #rename column
vb <- vb$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, vb)
tbl
c_test <- chisq.test(tbl)
c_test

df_vb <- cbind.data.frame(age_num=unlist(age_num), vb) #age numeric
rownames(df_vb) <- NULL
names(df_vb)[1] <- "age"
names(df_vb)[2] <- "use"

aov <- aov(age ~ use, data = df_vb)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#7 Which devices do you use to check social media? Please check all that apply.----
#PC
pc <- as.data.frame(data$DEVICE_PC, stringsAsFactors = FALSE) #force to character type
names(pc)[1] <- "use" #rename column
pc <- pc$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, pc)
tbl
c_test <- chisq.test(tbl)
c_test

df_pc <- cbind.data.frame(age_num=unlist(age_num), pc) #age numeric
rownames(df_pc) <- NULL
names(df_pc)[1] <- "age"
names(df_pc)[2] <- "use"

aov <- aov(age ~ use, data = df_pc)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Phone
ph <- as.data.frame(data$DEVICE_PHONE, stringsAsFactors = FALSE) #force to character type
names(ph)[1] <- "use" #rename column
ph <- ph$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, ph)
tbl
c_test <- chisq.test(tbl)
c_test

df_ph <- cbind.data.frame(age_num=unlist(age_num), ph) #age numeric
rownames(df_ph) <- NULL
names(df_ph)[1] <- "age"
names(df_ph)[2] <- "use"

aov <- aov(age ~ use, data = df_ph)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Tablet
ta <- as.data.frame(data$DEVICE_TABLET, stringsAsFactors = FALSE) #force to character type
names(ta)[1] <- "use" #rename column
ta <- ta$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, ta)
tbl
c_test <- chisq.test(tbl)
c_test

df_ta <- cbind.data.frame(age_num=unlist(age_num), ta) #age numeric
rownames(df_ta) <- NULL
names(df_ta)[1] <- "age"
names(df_ta)[2] <- "use"

aov <- aov(age ~ use, data = df_ta)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Other had one "other" response and it was 'None', so analysis not performed

#8 Where do you access social media?----
#Home
hm <- as.data.frame(data$ACCESS_HOME, stringsAsFactors = FALSE) #force to character type
names(hm)[1] <- "use" #rename column
hm <- hm$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, hm)
tbl
c_test <- chisq.test(tbl)
c_test

df_hm <- cbind.data.frame(age_num=unlist(age_num), hm) #age numeric
rownames(df_hm) <- NULL
names(df_hm)[1] <- "age"
names(df_hm)[2] <- "use"

aov <- aov(age ~ use, data = df_hm)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Work
wk <- as.data.frame(data$ACCESS_WORK, stringsAsFactors = FALSE) #force to character type
names(wk)[1] <- "use" #rename column
wk <- wk$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, wk)
tbl
c_test <- chisq.test(tbl)
c_test

df_wk <- cbind.data.frame(age_num=unlist(age_num), wk) #age numeric
rownames(df_wk) <- NULL
names(df_wk)[1] <- "age"
names(df_wk)[2] <- "use"

aov <- aov(age ~ use, data = df_wk)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#School
sl <- as.data.frame(data$ACCESS_SCHOOL, stringsAsFactors = FALSE) #force to character type
names(sl)[1] <- "use" #rename column
sl <- sl$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, sl)
tbl
c_test <- chisq.test(tbl)
c_test

df_sl <- cbind.data.frame(age_num=unlist(age_num), sl) #age numeric
rownames(df_sl) <- NULL
names(df_sl)[1] <- "age"
names(df_sl)[2] <- "use"

aov <- aov(age ~ use, data = df_sl)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Other public location
ot <- as.data.frame(data$ACCESS_OTHER, stringsAsFactors = FALSE) #force to character type
names(ot)[1] <- "use" #rename column
ot <- ot$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, ot)
tbl
c_test <- chisq.test(tbl)
c_test

df_ot <- cbind.data.frame(age_num=unlist(age_num), ot) #age numeric
rownames(df_ot) <- NULL
names(df_ot)[1] <- "age"
names(df_ot)[2] <- "use"

aov <- aov(age ~ use, data = df_ot)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#9 Aside from Facebook, what other social media do you use to seek agriculture information? Please check all that apply.----
#Instagram
#replacing NAs with the alternative value
ig <- as.data.frame(data$AG_USE_IG, stringsAsFactors = FALSE) #force to character type
names(ig)[1] <- "use" #rename column
ig <- ig$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, ig)
tbl
c_test <- chisq.test(tbl)
c_test

df_ig <- cbind.data.frame(age_num=unlist(age_num), ig) #age numeric
rownames(df_ig) <- NULL
names(df_ig)[1] <- "age"
names(df_ig)[2] <- "use"

aov <- aov(age ~ use, data = df_ig)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Pinterest
pi <- as.data.frame(data$AG_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, pi)
tbl
c_test <- chisq.test(tbl)
c_test

df_pi <- cbind.data.frame(age_num=unlist(age_num), pi) #age numeric
rownames(df_pi) <- NULL
names(df_pi)[1] <- "age"
names(df_pi)[2] <- "use"

aov <- aov(age ~ use, data = df_pi)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#LinkedIn
lk <- as.data.frame(data$AG_USE_LK, stringsAsFactors = FALSE) #force to character type
names(lk)[1] <- "use" #rename column
lk <- lk$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, lk)
tbl
c_test <- chisq.test(tbl)
c_test

df_lk <- cbind.data.frame(age_num=unlist(age_num), lk) #age numeric
rownames(df_lk) <- NULL
names(df_lk)[1] <- "age"
names(df_lk)[2] <- "use"

aov <- aov(age ~ use, data = df_lk)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Twitter
tw <- as.data.frame(data$AG_USE_TW, stringsAsFactors = FALSE) #force to character type
names(tw)[1] <- "use" #rename column
tw <- tw$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, tw)
tbl
c_test <- chisq.test(tbl)
c_test

df_tw <- cbind.data.frame(age_num=unlist(age_num), tw) #age numeric
rownames(df_tw) <- NULL
names(df_tw)[1] <- "age"
names(df_tw)[2] <- "use"

aov <- aov(age ~ use, data = df_tw)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#YouTube
yt <- as.data.frame(data$AG_USE_YT, stringsAsFactors = FALSE) #force to character type
names(yt)[1] <- "use" #rename column
yt <- yt$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, yt)
tbl
c_test <- chisq.test(tbl)
c_test

df_yt <- cbind.data.frame(age_num=unlist(age_num), yt) #age numeric
rownames(df_yt) <- NULL
names(df_yt)[1] <- "age"
names(df_yt)[2] <- "use"

aov <- aov(age ~ use, data = df_yt)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Reddit
re <- as.data.frame(data$AG_USE_RE, stringsAsFactors = FALSE) #force to character type
names(re)[1] <- "use" #rename column
re <- re$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, re)
tbl
c_test <- chisq.test(tbl)
c_test

df_re <- cbind.data.frame(age_num=unlist(age_num), re) #age numeric
rownames(df_re) <- NULL
names(df_re)[1] <- "age"
names(df_re)[2] <- "use"

aov <- aov(age ~ use, data = df_re)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Tumblr
tu <- as.data.frame(data$AG_USE_TU, stringsAsFactors = FALSE) #force to character type
names(tu)[1] <- "use" #rename column
tu <- tu$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, tu)
tbl
c_test <- chisq.test(tbl)
c_test

df_tu <- cbind.data.frame(age_num=unlist(age_num), tu) #age numeric
rownames(df_tu) <- NULL
names(df_tu)[1] <- "age"
names(df_tu)[2] <- "use"

aov <- aov(age ~ use, data = df_tu)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Flickr
fl <- as.data.frame(data$AG_USE_FL, stringsAsFactors = FALSE) #force to character type
names(fl)[1] <- "use" #rename column
fl <- fl$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, fl)
tbl
c_test <- chisq.test(tbl)
c_test

df_fl <- cbind.data.frame(age_num=unlist(age_num), fl) #age numeric
rownames(df_fl) <- NULL
names(df_fl)[1] <- "age"
names(df_fl)[2] <- "use"

aov <- aov(age ~ use, data = df_fl)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Google+
go <- as.data.frame(data$AG_USE_GO, stringsAsFactors = FALSE) #force to character type
names(go)[1] <- "use" #rename column
go <- go$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, go)
tbl
c_test <- chisq.test(tbl)
c_test

df_go <- cbind.data.frame(age_num=unlist(age_num), go) #age numeric
rownames(df_go) <- NULL
names(df_go)[1] <- "age"
names(df_go)[2] <- "use"

aov <- aov(age ~ use, data = df_go)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Snapchat
sc <- as.data.frame(data$AG_USE_SC, stringsAsFactors = FALSE) #force to character type
names(sc)[1] <- "use" #rename column
sc <- sc$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, sc)
tbl
c_test <- chisq.test(tbl)
c_test

df_sc <- cbind.data.frame(age_num=unlist(age_num), sc) #age numeric
rownames(df_sc) <- NULL
names(df_sc)[1] <- "age"
names(df_sc)[2] <- "use"

aov <- aov(age ~ use, data = df_sc)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#WhatsApp
wa <- as.data.frame(data$AG_USE_WA, stringsAsFactors = FALSE) #force to character type
names(wa)[1] <- "use" #rename column
wa <- wa$use %>%
  replace_na("Not Used")
tbl <- table(data$AGE, wa)
tbl
c_test <- chisq.test(tbl)
c_test

df_wa <- cbind.data.frame(age_num=unlist(age_num), wa) #age numeric
rownames(df_wa) <- NULL
names(df_wa)[1] <- "age"
names(df_wa)[2] <- "use"

aov <- aov(age ~ use, data = df_wa)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#WeChat
# No one chose this response

#LINE
# No one chose this response

#Viber
# No one chose this response


#10 Do you post questions about agriculture on Facebook?----
tbl <- table(data$AGE, data$AG_QS_FB)
tbl

chi_test <- chisq.test(tbl)
chi_test

df <- cbind.data.frame(age_num=unlist(age_num), data$AG_QS_FB) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "question"

aov <- aov(age ~ question, data = df)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#11 If yes, where do you post questions about agriculture on Facebook?----
ag_yes <- data[which(data$AG_QS_FB=="Yes, I post questions about agriculture on Facebook."),]
ag_yes #only want those who answered yes to Q10

#In my status updates
ag_yes$AG_QS_STATUS <- ag_yes$AG_QS_STATUS %>%
  replace_na("Not Used")
tbl <- table(ag_yes$AGE, ag_yes$AG_QS_STATUS)
tbl
chi_test <- chisq.test(tbl)
chi_test

df <- cbind.data.frame(ag_yes$AGE,ag_yes$AG_QS_STATUS) #age numeric
df$age <- as.character(df$age) #needed for some reason ?
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "post"

df$age[df$age == "18-21"] <- 19.5
df$age[df$age == "22-25"] <- 23.5
df$age[df$age == "26-30"] <- 28
df$age[df$age == "31-40"] <- 35.5
df$age[df$age == "41-50"] <- 45.5
df$age[df$age == "51-60"] <- 55.5
df$age[df$age == "61 or over"] <- 61
df$age <- as.numeric(df$age) #convert to numeric values

aov <- aov(age ~ post, data = df)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#In a group
ag_yes$AG_QS_GROUP <- ag_yes$AG_QS_GROUP %>%
  replace_na("Not Used")
tbl <- table(ag_yes$AGE, ag_yes$AG_QS_GROUP)
tbl
chi_test <- chisq.test(tbl)
chi_test

df <- cbind.data.frame(ag_yes$AGE,ag_yes$AG_QS_GROUP) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "post"
df$age <- as.character(df$age) #needed after renaming

df$age[df$age == "18-21"] <- 19.5
df$age[df$age == "22-25"] <- 23.5
df$age[df$age == "26-30"] <- 28
df$age[df$age == "31-40"] <- 35.5
df$age[df$age == "41-50"] <- 45.5
df$age[df$age == "51-60"] <- 55.5
df$age[df$age == "61 or over"] <- 61
df$age <- as.numeric(df$age) #convert to numeric values

aov <- aov(age ~ post, data = df)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#On a page
ag_yes$AG_QS_PAGE <- ag_yes$AG_QS_PAGE %>%
  replace_na("Not Used")
tbl <- table(ag_yes$AGE, ag_yes$AG_QS_PAGE)
tbl
chi_test <- chisq.test(tbl)
chi_test

df <- cbind.data.frame(ag_yes$AGE,ag_yes$AG_QS_PAGE) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "post"
df$age <- as.character(df$age) #needed after renaming

df$age[df$age == "18-21"] <- 19.5
df$age[df$age == "22-25"] <- 23.5
df$age[df$age == "26-30"] <- 28
df$age[df$age == "31-40"] <- 35.5
df$age[df$age == "41-50"] <- 45.5
df$age[df$age == "51-60"] <- 55.5
df$age[df$age == "61 or over"] <- 61
df$age <- as.numeric(df$age) #convert to numeric values

aov <- aov(age ~ post, data = df)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#Other
ag_yes$AG_QS_OTHER <- ag_yes$AG_QS_OTHER %>%
  replace_na("Not Used")
tbl <- table(ag_yes$AGE, ag_yes$AG_QS_OTHER)
tbl
chi_test <- chisq.test(tbl)
chi_test

df <- cbind.data.frame(ag_yes$AGE,ag_yes$AG_QS_OTHER) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "post"
df$age <- as.character(df$age) #needed after renaming

df$age[df$age == "18-21"] <- 19.5
df$age[df$age == "22-25"] <- 23.5
df$age[df$age == "26-30"] <- 28
df$age[df$age == "31-40"] <- 35.5
df$age[df$age == "41-50"] <- 45.5
df$age[df$age == "51-60"] <- 55.5
df$age[df$age == "61 or over"] <- 61
df$age <- as.numeric(df$age) #convert to numeric values

aov <- aov(age ~ post, data = df)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
plot(tukey)

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#get residuals
stdres(aov)

#12 The following are types of content you find on Facebook. How helpful are each of them in providing you with agriculture information?----

data2 <- as.data.frame(data, stringsAsFactors=FALSE) #made a copy of the data to test
data2[ ,63:74][ data2[ ,63:74] == "Not at all helpful" ] <- 1 #convert multiple columns to scores
data2[ ,63:74][ data2[ ,63:74] == "Slightly helpful" ] <- 2
data2[ ,63:74][ data2[ ,63:74] == "Moderately helpful" ] <- 3
data2[ ,63:74][ data2[ ,63:74] == "Very helpful" ] <- 4
data2[ ,63:74][ data2[ ,63:74] == "Extremely helpful" ] <- 5
data2[,63:74] <- sapply(data2[ ,63:74],as.numeric) #convert multiple columns to numeric type

myData <- myData[-c(2, 4, 6), ] #drop a row, don't run, this is example code
#Grab stadardized residuals to analyze
stdres(aov)

#Advertisements
df <- cbind.data.frame(data2$AGE, data2$HELP_AD) #rating number
names(df) <- c("age","ads_score")
df <- na.omit(df)
df <- df[-c(71), ] #droped outlier
#Table
tbl <- table(df$age, df$ads_score)
tbl
#ANOVA
aov <- aov(ads_score ~ age, data = df)
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

#Advertisements - age number, rating category
df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_AD) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "ads_category"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ ads_category, data = df)
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

#Apps
df <- cbind.data.frame(data2$AGE, data2$HELP_AP)
names(df) <- c("age","apps_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$apps_score)
tbl
#ANOVA
aov <- aov(apps_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_AP) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "apps_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ apps_score, data = df)
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

#Articles
df <- cbind.data.frame(data2$AGE, data2$HELP_AR)
names(df) <- c("age","articles_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$articles_score)
tbl
#ANOVA
aov <- aov(articles_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_AR) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "articles_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ articles_score, data = df)
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

#Events
df <- cbind.data.frame(data2$AGE, data2$HELP_EV)
names(df) <- c("age","events_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$events_score)
tbl
#ANOVA
aov <- aov(events_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_EV) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "events_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ events_score, data = df)
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

#Groups
df <- cbind.data.frame(data2$AGE, data2$HELP_GR)
names(df) <- c("age","groups_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$groups_score)
tbl
#ANOVA
aov <- aov(groups_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_GR) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "groups_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ groups_score, data = df)
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

#Links
df <- cbind.data.frame(data2$AGE, data2$HELP_LI)
names(df) <- c("age","links_score")
df <- na.omit(df)
df <- df[-c(74), ] #droped outlier
#Table
tbl <- table(df$age, df$links_score)
tbl
#ANOVA
aov <- aov(links_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_LI) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "links_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ links_score, data = df)
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

#Pages
df <- cbind.data.frame(data2$AGE, data2$HELP_PA)
names(df) <- c("age","pages_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$pages_score)
tbl
#ANOVA
aov <- aov(pages_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_PA) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "pages_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ pages_score, data = df)
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

#Photos
df <- cbind.data.frame(data2$AGE, data2$HELP_PH)
names(df) <- c("age","photos_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$photos_score)
tbl
#ANOVA
aov <- aov(photos_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_PH) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "photos_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ photos_score, data = df)
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

#Places
df <- cbind.data.frame(data2$AGE, data2$HELP_PL)
names(df) <- c("age","places_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$places_score)
tbl
#ANOVA
aov <- aov(places_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_PL) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "places_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ places_score, data = df)
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

#Related Searches
df <- cbind.data.frame(data2$AGE, data2$HELP_RS)
names(df) <- c("age","rs_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$rs_score)
tbl
#ANOVA
aov <- aov(rs_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_RS) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "rs_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ rs_score, data = df)
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

#Status Updates
df <- cbind.data.frame(data2$AGE, data2$HELP_SU)
names(df) <- c("age","su_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$su_score)
tbl
#ANOVA
aov <- aov(su_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_SU) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "su_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ su_score, data = df)
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

#Videos
df <- cbind.data.frame(data2$AGE, data2$HELP_VI)
names(df) <- c("age","videos_score")
df <- na.omit(df)
#Table
tbl <- table(df$age, df$videos_score)
tbl
#ANOVA
aov <- aov(videos_score ~ age, data = df)
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

df <- cbind.data.frame(age_num=unlist(age_num), data$HELP_VI) #age numeric
rownames(df) <- NULL
names(df)[1] <- "age"
names(df)[2] <- "videos_score"
df <- na.omit(df)
#ANOVA
aov <- aov(age ~ videos_score, data = df)
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
