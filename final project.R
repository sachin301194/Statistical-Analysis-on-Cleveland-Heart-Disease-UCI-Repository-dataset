knitr::opts_chunk$set(echo = TRUE)
summary(cars)
install.packages("kableExtra")
install.packages("dpylr")
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
               xgboost, h2o, corrplot, rpart.plot, corrgram, ggplot2, highcharter, 
               ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
               RColorBrewer, plotrix, ggrepel, tidyverse, gridExtra, reshape2.)
library(readr)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(ggcorrplot)
library(ggplot2)
library(plyr)
library(caret)
library(caTools)
library(reader)
processed_cleveland <- read_csv("heart.csv")
head(processed_cleveland)
cor(processed_cleveland)
summary(processed_cleveland)
corrplot(cor(processed_cleveland))
# Coverting the categorical data to factor
processed_cleveland$sex <- as.factor(processed_cleveland$sex)
processed_cleveland$target <- as.factor(processed_cleveland$target)
processed_cleveland$cp <- as.factor(processed_cleveland$cp)
processed_cleveland$ca <- as.factor(processed_cleveland$ca)
processed_cleveland$exang <- as.factor(processed_cleveland$exang)
processed_cleveland$slope <- as.factor(processed_cleveland$slope)
processed_cleveland$thal <- as.factor(processed_cleveland$thal)
# Summary after pre-processing the data
summary(processed_cleveland)
# DISPLAY THE NUMBER OF NAs IN EACH COLUMN
colSums(is.na(processed_cleveland))
# Bar plot for target (Heart disease) 
processed_cleveland$target <- as.factor(processed_cleveland$target)
ggplot(processed_cleveland, aes(x=processed_cleveland$target, fill=processed_cleveland$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

# Counting the frequency of the values of the age
ageCount <- count(processed_cleveland, 'age')
ageCount <- subset(ageCount[which(ageCount$freq > 10), ])

#ploting the age with frquency greater than 10
ggplot(ageCount, aes(x=ageCount$age, y=ageCount$freq)) + 
  ggtitle("Age Analysis") +
  xlab("Age")  +
  ylab("Age Count") +
  geom_bar(stat="identity")
# Group the different ages in three groups (young, middle, old)
young <- processed_cleveland[which((processed_cleveland$age<45)), ]
middle <- processed_cleveland[which((processed_cleveland$age>=45)&(processed_cleveland$age<55)), ]
elderly <- processed_cleveland[which(processed_cleveland$age>55), ]
groups <- data.frame(age_group = c("young","middle","elderly"), group_count = c(NROW(young$age), NROW(middle$age), NROW(elderly$age)))

#ploting different age groups
ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) + 
  ggtitle("Age Analysis") +
  xlab("Age Group")  +
  ylab("group Count") +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Age Group", labels = c("Elderly", "Middle", "Young"))
processed_cleveland = subset(processed_cleveland, select = c(-age))
ggplot(processed_cleveland, aes(x= factor(processed_cleveland$sex), y=processed_cleveland$sex, colour=target)) + 
  geom_boxplot(stat = "boxplot",
               position = "dodge2") +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) +
  xlab("Age Groups") +
  ylab("Gender") +
  ggtitle("Analysis of gender with different age group with presence or absense of heart disease")

# Bar plot for sex
ggplot(processed_cleveland, aes(x= processed_cleveland$sex, fill=processed_cleveland$target)) + 
  geom_bar() +
  xlab("Gender") +
  ylab("Gender Count") +
  ggtitle("Analysis of Gender") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Bar plot for The chest pain experienced 
ggplot(processed_cleveland, aes(x= cp, fill=cp)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina pain", "Non-Anginal pain", "Asymptomatic pain"))


# Bar plot for The chest pain ~ target
ggplot(processed_cleveland, aes(x= cp, fill=target)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Bar for ca (number of major vessels (0-3))
ggplot(processed_cleveland, aes(x= ca, fill=ca)) + 
  geom_bar() +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  theme(legend.position="none")


# Bar for ca (number of major vessels (0-3))
ggplot(processed_cleveland, aes(x= ca, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Histogram for trestbps (resting blood pressure)
ggplot(processed_cleveland, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")

# removing the outliers
processed_cleveland$trestbps = ifelse(processed_cleveland$trestbps > 180, NA, processed_cleveland$trestbps)
processed_cleveland$trestbps = ifelse(is.na(processed_cleveland$trestbps), median(processed_cleveland$trestbps[which(!is.na(processed_cleveland$trestbps))]), processed_cleveland$trestbps)

# After the removal of outliers
ggplot(processed_cleveland, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")

# Density graph for trestbps (resting blood pressure)
ggplot(processed_cleveland, aes(x = trestbps, fill = target)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))
# Histogram for oldpeak (ST depression induced by exercise relative to rest)
ggplot(processed_cleveland, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")



length(processed_cleveland)
# Bar plot for slope (slope of the peak exercise ST segment) 
processed_cleveland$slope <- ifelse(processed_cleveland$slope == 0, 1, print(processed_cleveland$slope))

processed_cleveland$slope <- as.factor(processed_cleveland$slope)
ggplot(processed_cleveland, aes(x=processed_cleveland$slope, fill=processed_cleveland$slope)) + 
  geom_bar() +
  xlab("Slope of ST segment") +
  ylab("Count") +
  ggtitle("Analysis of slope of the peak exercise ST segment") +
  scale_fill_discrete(name = "Slope of ST segment", labels = c("Upsloping", "Flat", "Downsloping"))
processed_cleveland$thalach = ifelse(processed_cleveland$thalach < 75, NA, processed_cleveland$thalach)
processed_cleveland$thalach = ifelse(is.na(processed_cleveland$thalach), median(processed_cleveland$thalach[which(!is.na(processed_cleveland$thalach))]), processed_cleveland$thalach)

ggplot(processed_cleveland, aes(x=thalach)) + 
  geom_histogram() +
  xlab("Maximum heart rate achieved") +
  ylab("Count") +
  ggtitle("Analysis of maximum heart rate achieved")
# Density plot for thalach ~ target
ggplot(processed_cleveland, aes(x = thalach, fill = target)) +
  geom_density(alpha=0.5) +
  xlab("Maximum Heart Rate Achieved") +
  ylab("Count") +
  ggtitle("Analysis of relation of heart rate with presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


processed_cleveland$thal = ifelse(processed_cleveland$thal == 0, 2, processed_cleveland$thal)
processed_cleveland$thal <- as.factor(processed_cleveland$thal)

ggplot(processed_cleveland, aes(x=thal, fill=thal)) + 
  geom_bar() +
  xlab("Blood disorder type") +
  ylab("Count") +
  ggtitle("Analysis of blood disorder (thalassemia)") +
  scale_fill_discrete(name = "Blood disorder", labels = c("Normal", "Fixed defect", "reversable defect"))
processed_cleveland<-read.csv(file = 'heart.csv')
processed_cleveland <- subset(processed_cleveland, age != "NaN" & !(is.na(age)),)
processed_cleveland <- subset(processed_cleveland, sex != "NaN" & !(is.na(sex)),)
processed_cleveland <- subset(processed_cleveland, cp != "NaN" & !(is.na(cp)),)
processed_cleveland <- subset(processed_cleveland, trestbps != "NaN" & !(is.na(trestbps)),)
processed_cleveland <- subset(processed_cleveland, chol != "NaN" & !(is.na(chol)),)
processed_cleveland <- subset(processed_cleveland, fbs != "NaN" & !(is.na(fbs)),)
processed_cleveland<- subset(processed_cleveland, restecg != "NaN" & !(is.na(restecg)),)
processed_cleveland <- subset(processed_cleveland, thalach != "NaN" & !(is.na(thalach)),)
processed_cleveland <- subset(processed_cleveland, exang != "NaN" & !(is.na(exang)),)
processed_cleveland <- subset(processed_cleveland, oldpeak != "NaN" & !(is.na(oldpeak)),)
processed_cleveland <- subset(processed_cleveland,  slope!= "NaN" & !(is.na(slope)),)
processed_cleveland <- subset(processed_cleveland, ca!= "NaN" & !(is.na(ca)),)
processed_cleveland <- subset(processed_cleveland, thal!= "NaN" & !(is.na(thal)),)
processed_cleveland <- subset(processed_cleveland, target!= "NaN" & !(is.na(target)),)


head(processed_cleveland)
nrow(processed_cleveland)
ncol(processed_cleveland)
data(processed_cleveland)
head(processed_cleveland)
qqnorm(processed_cleveland$chol)

hist(processed_cleveland$chol)

# the parts of the test statistic
# sample mean
x_bar <- mean(processed_cleveland$chol)
# null hypothesized population Chol
mu_0 <- 240
# sample st. dev
s <- sd(processed_cleveland$chol)
# sample size
n <- length(processed_cleveland$chol)
# t-test test statistic
t <- (x_bar - mu_0)/(s/sqrt(n))
# two-sided p-value so multiply by 2
two_sided_t_pval <- pt(q = t, df = n-1, lower.tail = FALSE)*2
two_sided_t_pval
t.test(processed_cleveland$chol,
       alternative = "two.sided",
       mu = 240)

# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = processed_cleveland$chol,
                            size = n,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'average chlorstral of patient ', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(238, 260, .1), dnorm(seq(238, 260, .1),  mean = mean(results), sd = sd
                                   (results)))

# Shift the sample so that the null hypothesis is true
time_given_H0_true <- processed_cleveland$chol - mean(processed_cleveland$chol) + mu_0
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results_given_H0_true[i] <- mean(sample(x = time_given_H0_true,
                                          size = n,
                                          replace = TRUE))
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE, main='Sampling Distribution of the Sample Mea
n, Given Null Hypothesis is True', xlab = 'Average chlorastrol level of patient', ylab = 'Density')
# add line to show values more extreme on upper end
abline(v=x_bar, col = "red")
# add line to show values more extreme on lower end
low_end_extreme <- mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
abline(v=low_end_extreme, col="red")
# counts of values more extreme than the test statistic in our original sample, given H_0is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= x_bar)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
bootstrap_pvalue


# two sided t p-value
two_sided_t_pval
# need the standard error which is the standard deviation of the results
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
c(x_bar - 2*bootstrap_SE_X_bar, x_bar + 2*bootstrap_SE_X_bar)
# you can also use the 5th and 95th quantiles to determine the bounds:
c(quantile(results, c(.025, .975)))

# compare to our t-methods
c(x_bar+(qt(0.025, n-1)*(s/sqrt(n))), x_bar+(qt(0.975, n-1)*(s/sqrt(n))))
table(processed_cleveland$sex
)

p_hat <- 205/302
z <- (p_hat - .5) / sqrt((.5*(1-.5)) / 302)
z
# One-sided upper exact
binom.test(x=205, n = 302, p = 0.5, alternative="greater")
# One sided upper normal ppproximation
pnorm(z, lower.tail = FALSE)

# exact binomial test confidence interval
binom.test(x=205, n = 302, p=(.5), alternative="greater")$conf.int
# normal approx confidence interval
c(p_hat - (1.64)*sqrt(((p_hat)*(1 - p_hat))/302), 1)
#Bootstrap Method
geos <- factor(rep(c("male", "female"), c(205, 302-205)))
geos

table(geos)

# This is going to be easier to use for bootstrapping
geos <- rep(c(1, 0), c(205, 302-205))
geos
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = geos,
                            size = 302,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion', 
     xlab = 'Proportion of female patinets having heart diease ', ylab = 'Density')
# estimate a normal curve
lines(x = seq(.40, .60, .001), dnorm(seq(.40, .60, .001), mean = mean(results), sd = sd(results)))




# Bootstrap Confidence Interval
n = 302
c(quantile(results, c(.05, 1)))

# exact binomial test
binom.test(x=205, n = 302, p=(.5), alternative="greater")$conf.int

# normal approx
c(p_hat - (1.64)*sqrt(((p_hat)*(1 - p_hat))/302), 1)
# Under the assumption that the null hypothesis is true, we have 50% peanuts
geos <- rep(c(1, 0), c(151, 302-151))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = geos,
                            size = 302,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, 
     main='Sampling Distribution of the Sample Proportion under H_0:p=0.5', xlab = 'Proportion of women having heart diease ', 
     ylab = 'Density', xlim = c(0.30,0.70))
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.40, .60, .001), dnorm(seq(.40, .60, .001), mean = mean(results), sd = sd(results)))
abline(v=p_hat, col="red")


count_of_more_extreme_upper_tail <- sum(results >= p_hat)
bootstrap_pvalue <- count_of_more_extreme_upper_tail/num_sims

# Bootstrap p-value
bootstrap_pvalue

# Exact Binomial p-value
binom.test(x=205, n = 302, p=(.5), alternative="greater")$p.value

# Normal Approximation p-value
pnorm(z, lower.tail = FALSE)

# QQ Plot for clorostrol value 

qqnorm(processed_cleveland$chol)
qqline(processed_cleveland$chol, col = "blue")

hist(processed_cleveland$chol)


# QQ Plot for chlorestral level when  gender is  male .
qqnorm(processed_cleveland$chol[processed_cleveland$sex == "1"])
qqline(processed_cleveland$chol[processed_cleveland$sex == "1"], col = "blue")

hist(processed_cleveland$chol[processed_cleveland$sex == "1"])
# QQ Plot for Launch Mass when Gender is  female.

qqnorm(processed_cleveland$chol[processed_cleveland$sex == "0"])

qqline(processed_cleveland$chol[processed_cleveland$sex == "0"], col = "blue")

hist(processed_cleveland$chol[processed_cleveland$sex == "0"])



# sample means
x_bar_m <- mean(processed_cleveland$chol[processed_cleveland$sex =="1"])
x_bar_f <- mean(processed_cleveland$chol[processed_cleveland$sex =="0"])
x_bar_m
x_bar_f


# null hypothesized population mean difference between the two groups
mu_0 <- 0
mu_0

# sample variances
s_f_sq <- sd(processed_cleveland$chol[processed_cleveland$sex =="0"])**2
s_m_sq <- sd(processed_cleveland$chol[processed_cleveland$sex =="1"])**2
s_m_sq
s_f_sq
# sample size
n_f <- length(processed_cleveland$chol[processed_cleveland$sex=="1"])
n_m <- length(processed_cleveland$chol[processed_cleveland$sex =="0"])
n_m
n_f
t <- (x_bar_f - x_bar_m - mu_0)/sqrt((s_f_sq/n_f) + (s_m_sq/n_m))
t
# one sided upper p-value
two_sided_diff_t_pval <- pt(q = t, df = min(n_f, n_m)-1, lower.tail = TRUE)*2
two_sided_diff_t_pval
# Lower bound of Confidence Interval 
(x_bar_f-x_bar_m)+(qt(0.025, min(n_f, n_m)-1)*sqrt((s_f_sq/n_f) + (s_m_sq/n_m)))
# Upper bound of Confidence Interval
(x_bar_f-x_bar_m)+(qt(0.975, min(n_f, n_m)-1)*sqrt((s_f_sq/n_f) + (s_m_sq/n_m)))


t.test(processed_cleveland$chol[processed_cleveland$sex=="1"], 
       processed_cleveland$chol[processed_cleveland$sex=="0"])
table(processed_cleveland$sex)



num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)

# A loop for completing the simulation
for(i in 1:num_sims){
  mean_male <- mean(sample(x = processed_cleveland$chol[processed_cleveland$sex == "1"],
                           size = 207,
                           replace = TRUE))
  mean_female <- mean(sample(x = processed_cleveland$chol[processed_cleveland$sex == "0"],
                             size = 96,
                             replace = TRUE))
  
  results[i] <- mean_female  - mean_male
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', 
     xlab = 'Average Difference clostral value ', ylab = 'Density')
# estimate a normal curve
lines(x = seq(-10, 50, .001), dnorm(seq(-10, 50, .001), mean = mean(results), sd = sd(results)))

# Bootstrap one-sided CI


c(quantile(results, c(.025, .975)))
# compare to our t-methods
t.test(processed_cleveland$chol[processed_cleveland$sex=="1"], 
       processed_cleveland$chol[processed_cleveland$sex=="0"])$conf.int


set.seed(0)
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(processed_cleveland, sex=sample(sex))
  mean_male <- mean(shuffled_groups$chol[shuffled_groups$sex == "1"])
  mean_female <- mean(shuffled_groups$chol[shuffled_groups$sex == "0"])
  results_given_H0_true[i] <- mean_female - mean_male
}

# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Means Under Null',
     xlab = 'Average Difference of clostreal level is  under Null',
     ylab = 'Density')
diff_in_sample_means <- mean(processed_cleveland$chol[processed_cleveland$sex == "1"]) - 
  mean(processed_cleveland$chol[processed_cleveland$sex == "0"])
abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= diff_in_sample_means)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= abs(diff_in_sample_means))
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims

## Bootstrap p-value
bootstrap_pvalue

## t-test p-value
t.test(processed_cleveland$chol[processed_cleveland$sex=="0"], 
       processed_cleveland$chol[processed_cleveland$sex=="1"])$p.value
p_hat_f <- sum(processed_cleveland$sex == "0" & processed_cleveland$target == "1")/sum(processed_cleveland$sex == "0")
p_hat_m <- sum(processed_cleveland$sex == "1" & processed_cleveland$target == "1")/sum(processed_cleveland$sex== "1")
p_hat_f
p_hat_m
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_f <- sum(processed_cleveland$sex == "0")
n_m <- sum(processed_cleveland$sex == "1")
n_f
n_m
# sample variances
den_p_m <- (p_hat_m*(1-p_hat_m))/n_m
den_p_f <- (p_hat_f*(1-p_hat_f))/n_f
den_p_m
den_p_f
# z-test test statistic
z <- (p_hat_f - p_hat_m - p_0)/sqrt(den_p_f + den_p_m)
z

# two sided p-value
two_sided_diff_prop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diff_prop_pval

# lower bound
(p_hat_f - p_hat_m)+(qnorm(0.025)*sqrt(den_p_f + den_p_m))


# upper bound
(p_hat_f - p_hat_m)+(qnorm(0.975)*sqrt(den_p_f + den_p_m))

# Bootstrap Approach
# Make Data
female <- rep(c(1,0), c(sum(processed_cleveland$sex == "0" & processed_cleveland$target == "1"), n_f - sum(processed_cleveland$sex == "0" & processed_cleveland$target == "1")))
male<- rep(c(1, 0), c(sum(processed_cleveland$sex == "1" & processed_cleveland$target == "1"), n_m - sum(processed_cleveland$sex == "1" & processed_cleveland$target == "1")))
num_sims <- 10000

# A vector to store my results
results <- rep(NA, num_sims)

n_f
n_m

# A loop for completing the simulation
for(i in 1:num_sims){
  prop_female <- mean(sample(x = female,
                             size = n_f,
                             replace = TRUE))
  prop_male<- mean(sample(x = male,
                          size = n_m,
                          replace = TRUE))
  results[i] <- prop_female - prop_male
}

# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', 
     xlab = 'Difference in Prop. male male and female having heart dieaseas ', ylab = 'Density')
# Bootstrap
c(quantile(results, c(.025, .975)))


# Normal Approximation
c((p_hat_f - p_hat_m)+(qnorm(0.025)*sqrt(den_p_f + den_p_m)), 
  (p_hat_f - p_hat_m)+(qnorm(0.975)*sqrt(den_p_f + den_p_m)))

# Make the data
df_combined <- data.frame("count_heart_disease" = c(female,male),
                          "sex" = rep(c("0", "1"), c(n_f, n_m
                          )))

df_combined
# Sanity checks
summary(df_combined$users)

mean(df_combined$count_heart_disease[df_combined$sex == "0"]) == p_hat_f

mean(df_combined$count_heart_disease[df_combined$sex == "1"]) == p_hat_m

num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)

# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationship we should be able to shuffle the groups
  shuffled_groups <- transform(df_combined, sex = sample(sex))
  prop_male <- mean(shuffled_groups$count_heart_disease[shuffled_groups$sex == "1"])
  prop_female <- mean(shuffled_groups$count_heart_disease[shuffled_groups$sex == "0"])
  results_given_H0_true[i] <- prop_female - prop_male
}
results_given_H0_true

# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Sample Props Under Null',
     xlab = 'Average Difference in Prop of male and female having heart diease ',
     ylab = 'Density', xlim = c(-0.30, 0.30))
diff_in_sample_props <- p_hat_f - p_hat_m
abline(v=diff_in_sample_props, col = "blue")
abline(v=-diff_in_sample_props, col = "red")
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= -diff_in_sample_props)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true > diff_in_sample_props)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims

count_of_more_extreme_lower_tail
count_of_more_extreme_lower_tail

# Bootstrap p-value
bootstrap_pvalue
# Normal Approx p-value
two_sided_diff_prop_pval


table(processed_cleveland$thal)
prop.table(table(processed_cleveland$thal))

sum(((table(processed_cleveland$thal) - 75.75)^2)/75.75)
pchisq(245.8185, df = 4-1, lower.tail = FALSE)