## explore the data
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
summary(pf)

## plot fb friend birthdays by date, one plot per month
qplot(x = dob_day, data = pf) +
  scale_x_discrete(breaks=1:31) +
  facet_wrap(~dob_month, ncol = 3)

## plot fb friend counts by gender
qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 25) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

## omit any record w/ NA
qplot(x = friend_count, data = na.omit(pf), binwidth = 25) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

## take a look at genders
table(pf$gender)

## a more detailed "table"
by(pf$friend_count, pf$gender, summary)

## graph by years that each friend has been on fb
qplot(x = tenure/365, data = pf, binwidth = 0.2,
      color = I('black'), fill = I('#899DD9'))

## change color to orange, x-axis increments to 1, x-axis to end at 7 years
qplot(x = tenure/365, data = pf, binwidth = 0.2,
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7))

## change axes labels
qplot(x = tenure/365, data = pf, binwidth = 0.2,
      xlab = 'Number of Years Using FB',
      ylab = 'Number of Users',
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7))

## plot fb users by age, one bar per age
qplot(x = age, data=pf, binwidth = 1,
      xlab = 'FB User Age',
      ylab = 'Number of Users',
      color = I('black'), fill = I('pink')) +
  scale_x_continuous(breaks = seq(10, 120, 10))

## transforming friend count data
summary(pf$friend_count)
summary(log10(pf$friend_count + 1))
summary(sqrt(pf$friend_count))

## 3 graphs of friend count data
library(gridExtra)
p1 <- qplot(x = friend_count, data = pf) + 
  scale_x_continuous(breaks = seq(0, 5000, 1000));
p2 <- qplot(x = log10(friend_count + 1), data = pf) +
  scale_x_continuous(breaks = seq(0, 4, 1));
p3 <- qplot(x = sqrt(friend_count), data = pf) +
  scale_x_continuous(breaks = seq(0, 72, 8));
grid.arrange(p1, p2, p3, ncol = 1)

## same as above, but using ggplot; need to tell system we want to make the histogram "geom type"
p1 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()
grid.arrange(p1, p2, p3, ncol = 1)

## frequency polygons: allowing overlays of data curves, to compare distributions
qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

## showing proportions instead of raw counts
qplot(x = friend_count, y = ..count../sum(..count..), 
      data = subset(pf, !is.na(gender)),
      xlab = 'Friend Count',
      ylab = 'Proportion of Users',
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

## which gender creates more likes?
summary(pf$www_likes)
by(pf$www_likes, pf$gender, summary)

qplot(x = www_likes, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      xlab = 'Likes Count',
      ylab = 'Proportion of Users',
      geom = 'freqpoly', color = gender) +
  scale_x_continuous() +
  scale_x_log10()

## get total likes per gender
library(dplyr)
pfMales <- filter(pf, gender == "male")
sum(pfMales$www_likes)
pfFemales <- filter(pf, gender == "female")
sum(pfFemales$www_likes)

## same as above, but using "by"
by(pf$www_likes, pf$gender, sum)

## box plots
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot') +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 500))

## same as above, but using ylim instead of setting y-axis scale (same results)
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot', ylim = c(0, 1000))

## same as above, but using coord cartesian layer so as not to remove data points (different results!)
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot') + 
  coord_cartesian(ylim = c(0, 1000))

## who initiated more friendships?
by(pf$friendships_initiated, pf$gender, summary)
qplot(x = gender, y = friendships_initiated,
      data = subset(pf, !is.na(gender)), geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 150))

## what % of users have checked in using mobile?
summary(pf$mobile_likes)
summary(pf$mobile_likes > 0)
### create new variable to track mobile check-ins
mobile_check_in <- NA
### assign value 1 if user has checked in; 0 if user has not
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
### convert this to a factor variable
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)