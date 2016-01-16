## load the data set
library(ggplot2)
data(diamonds)

## learn about the data set
summary(diamonds)
?diamonds
str(diamonds)
diamonds$color
summary(diamonds$price)

## plot a histogram of the prices
qplot(x = price, data = diamonds, binwidth = 250,
      color = I('black'), fill = I('#86C67C')) +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0, 20000, 2500))
ggsave('UD651_ProblemSet3_PriceHistogram.png')

## count the diamonds at certain prices
sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)

## plot histogram of prices by cut
qplot(x = price, data = diamonds, binwidth = 250,
      color = I('black'), fill = I('#86C67C')) +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0, 20000, 2500)) +
  facet_wrap(~cut, ncol = 3)

## learn about each cut
summary(diamonds$cut)
faircut <- subset(diamonds, diamonds$cut == "Fair")
goodcut <- subset(diamonds, diamonds$cut == "Good")
vgoodcut <- subset(diamonds, diamonds$cut == "Very Good")
premcut <- subset(diamonds, diamonds$cut == "Premium")
idealcut <- subset(diamonds, diamonds$cut == "Ideal")
maxprices <- c(max(faircut$price), max(goodcut$price), max(vgoodcut$price),
             max(premcut$price), max(idealcut$price))
minprices <- c(min(faircut$price), min(goodcut$price), min(vgoodcut$price),
               min(premcut$price), min(idealcut$price))
medprices <- c(median(faircut$price), median(goodcut$price), median(vgoodcut$price),
               median(premcut$price), median(idealcut$price))
match(max(maxprices), maxprices)
match(min(minprices), minprices)
match(min(medprices), medprices)
maxprices #answers were still wrong, so examine the vectors manually
minprices
medprices

## plot the previous histogram again, with y-axes not fixed
qplot(x = price, data = diamonds, binwidth = 250,
      color = I('black'), fill = I('#86C67C')) +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0, 20000, 2500)) +
  facet_wrap(~cut, ncol = 3, scales = "free_y")

## plot histogram of price per carat, faceted by cut
qplot(x = price/carat, data = diamonds, 
      color = I('black'), fill = I('#86C67C')) +
 # scale_x_continuous(limits = c(0,20000), breaks = seq(0, 20000, 2500)) +
  facet_wrap(~cut, ncol = 3, scales = "free_y")

## box plots for price by cut/clarity/color
qplot(x = cut, y = price, 
      data = diamonds, 
      geom = 'boxplot')
ggsave('UD651_ProblemSet3_PriceCutBox.png')
qplot(x = clarity, y = price, 
      data = diamonds, 
      geom = 'boxplot')
ggsave('UD651_ProblemSet3_PriceClarityBox.png')
qplot(x = color, y = price, 
      data = diamonds, 
      geom = 'boxplot')
ggsave('UD651_ProblemSet3_PriceColorBox.png')

## some numerical summaries for price by cut/clarity/color
summary(subset(diamonds$price, diamonds$cut == "Fair"))
summary(subset(diamonds$price, diamonds$cut == "Good"))
summary(subset(diamonds$price, diamonds$color == "D"))
summary(subset(diamonds$price, diamonds$color == "J"))
4214-911 #answer IQR questions
7695-1860

## box plots for price per carat by color
qplot(x = color, y = price/carat, 
      data = diamonds, 
      geom = 'boxplot')
ggsave('UD651_ProblemSet3_PpcColorBox.png')

## frequency polygon for carat
qplot(x = carat, data = diamonds,
      binwidth = 0.05, 
      geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.3))

## plot gapminder data
### load data set about average ages of billionaires across the globe
gb <- read.csv("Indicator_Average age.csv", header = TRUE)
str(gb)


### fixing up the columns a little
gb[6:9] <- list(NULL)
colnames(gb) <- c("country", "2004", "2005", "2006", "2007")
str(gb)

library(tidyr)
gbtidy <- gather(gb, "year", "avg_age", 2:5)
gbtidy2 <- transform(gbtidy, country = as.character(country))
str(gbtidy)

### remove countries with no billionaires
gbtidy2[gbtidy2 == 0] <- NA
head(gbtidy2)
gbdata <- na.omit(gbtidy2)
str(gbdata)
head(gbdata)
quantile(gbdata$avg_age)

### some plots
hist(gbdata$avg_age)

library(ggplot2)
qplot(x = avg_age, data = gbdata, binwidth = 1,
      color = I('black'), fill = I('#EEAEEE')) +
  facet_wrap(~year) # count of each average age per year
ggsave('UD651_ProblemSet3_GbAgeYearsHistogram.png')

plot(gbdata$avg_age~gbdata$year) # boxplot of ages per year
ggsave('UD651_ProblemSet3_GbAgeYearsBox.png')


## facebook friends' birthdays
fb <- read.csv("Facebook_Birthdays.csv", header = FALSE, sep = ",",
               col.names = c("birthday", "name"), colClasses = c("character", "character"))
str(fb)
fbdates <- as.Date(fb$birthday, format = "%m/%d/%Y")
fbnames <- as.character(fb$name)
fb2 <- data.frame(fbdates, fbnames)
str(fb2)

### split date variable into month & day, remove year
library(tidyr)
fbsplit <- separate(fb2, fbdates, c("year", "month", "day"))
str(fbsplit)

library(dplyr)
fbdata <- select(fbsplit, month, day, fbnames)
str(fbdata)

### birthday day counts by month
library(ggplot2)
bdayplot <- qplot(x = day, data = fbdata, binwidth = 1,
      color = I('black'), fill = I('#2E0854')) +
  facet_wrap(~month)

### how many people share my birthday?
library(dplyr)
count(filter(fbdata, month == 11 & day == 20))

### how many birthdays are in each month?
bdays_by_month <- tally(group_by(fbdata, month))
bdays_by_month

### which month contains the most birthdays?
bdays_by_month[bdays_by_month$n == max(bdays_by_month$n),]

### which day contains the most birthdays?
bdays_by_day <- tally(group_by(fbdata, day))
bdays_by_day[bdays_by_day$n == max(bdays_by_day$n),]

### is there at least 1 birthday on each day of the year? --> no
bdayplot
