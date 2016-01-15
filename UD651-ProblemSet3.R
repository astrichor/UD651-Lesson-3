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

