#Descriptives, Plots, and Post-Hoc Analysis----
library(readr)
library(plyr) #for counting function
library(tibble) #add_row
library(DescTools) #cramer's V and phi test

#Load data----
data <- read_csv("Survey Data 2-23-19_Demographics_Edited.csv")
View(data)

#GENDER ANALYSIS----

#descriptive table
gender_df <- as.data.frame(count(data$GENDER))
names(gender_df)[1] <- "Label"
names(gender_df)[2] <- "Frequency"

#5 How would you rate your current social media skills?

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

df_skills <- cbind.data.frame(skills=unlist(skills), data$GENDER)
rownames(df_skills) <- NULL
names(df_skills)[1] <- "rating"
names(df_skills)[2] <- "label"

aov <- aov(rating ~ label, data = df_skills)
summary(aov)

#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey

#check for normality
plot(aov, 2)

#print the model tables from the anova
print(model.tables(aov,"means"),digits=3) 

#6 Aside from Facebook, what other social media sites do you use? Please check all that apply.

#Pinterest
pi <- as.data.frame(data$SM_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, pi)
tbl
c_test <- chisq.test(tbl, correct=FALSE)
c_test

c_test$expected
c_test$residuals
c_test$stdres

cramer_value <- CramerV(tbl) #used for bigger than 2x2 tables
cramer_value

phi_test <- Phi(tbl)  #the Phi function uses DescTools package, used for 2x2 tables
phi_test

#7 Which devices do you use to check social media? Please check all that apply.
#PC
pc <- as.data.frame(data$DEVICE_PC, stringsAsFactors = FALSE) #force to character type
names(pc)[1] <- "use" #rename column
pc <- pc$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, pc)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test

phi_test <- Phi(tbl)
phi_test

#9 Aside from Facebook, what other social media do you use to seek agriculture information? Please check all that apply.

#Pinterest
pi <- as.data.frame(data$AG_USE_PI, stringsAsFactors = FALSE) #force to character type
names(pi)[1] <- "use" #rename column
pi <- pi$use %>%
  replace_na("Not Used")
tbl <- table(data$GENDER, pi)
tbl
f_test <- exact2x2(tbl, tsmethod = "central")
f_test <- exact2x2(tbl[,c(2,1)], tsmethod = "central") #reversed the columns to get the right odds ratio interpretation
f_test
phi_test <- Phi(tbl)
phi_test

#12 The following are types of content you find on Facebook. How helpful are each of them in providing you with agriculture information?
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
df <- df[-c(71), ] #drop a row
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
#standard deviation - add to all code
sd()

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

#Photos
df <- cbind.data.frame(data2$GENDER, data2$HELP_PH)
names(df) <- c("gender","photos_score")
df <- na.omit(df)
#Table
tbl <- table(df$gender, df$photos_score)
tbl
#ANOVA
aov <- aov(photos_score ~ gender, data = df)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)

#Places
df <- cbind.data.frame(data2$GENDER, data2$HELP_PL)
names(df) <- c("gender","places_score")
df <- na.omit(df)
#Table
tbl <- table(df$gender, df$places_score)
tbl
#ANOVA
aov <- aov(places_score ~ gender, data = df)
summary(aov)
#multiple pairwise-comparison
tukey <- TukeyHSD(aov) #with 95% conf.level
tukey
#check for normality
plot(aov, 2)
#print the model tables from the anova
print(model.tables(aov,"means"),digits=3)


#plot the frequency table
library("vcd")
barplot(tbl, legend = rownames(tbl), beside = TRUE,
        xlab = "Usage", ylab = "Frequency", col=c("pink","blue"))

mosaic(tbl, shade=TRUE, legend=TRUE)
assoc(tbl, shade=TRUE)
