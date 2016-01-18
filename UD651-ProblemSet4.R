?diamonds
str(diamonds)

# scatterplot of price vs. x
library(ggplot2)
ggplot(aes(x = x, y = price), data = diamonds) +
       geom_point()

# correlations
library(alr3)
cor.test(diamonds$x, diamonds$price)
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

# scatterplot of price vs. depth
max(diamonds$depth)
ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0, 80, 2))

cor.test(diamonds$depth, diamonds$price)

# scatterplot of price vs. carat
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point() +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))

# scatterplot of price vs. volume
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point()

dvol <- subset(diamonds, volume > 0)
dvol2 <- subset(dvol, volume < 800)
cor.test(dvol2$volume, dvol2$price)

ggplot(aes(x = volume, y = price), data = dvol2) +
  geom_point(alpha = 1/100) +
  geom_smooth(method = 'lm', color = 'green')

# diamonds by clarity
library(dplyr)
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n())
head(diamondsByClarity)

# two bar plots for mean prices by clarity and color
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
head(diamonds_mp_by_clarity)

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
head(diamonds_mp_by_color)

library(gridExtra)
p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")
p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")
grid.arrange(p1, p2, ncol = 1)

# plotting gapminder data: traffic mortality rates by males and females
tm <- read.csv("RTI mortality male indicator file UL 20100916.csv", header = TRUE)
tf <- read.csv("RTI mortality female indicator file UL 20100916.csv", header = TRUE)

str(tm)
str(tf)

## clean up the data
library(tidyr)
tmtidy <- gather(tm, "year", "mrate", 2:56)
tmtidy2 <- transform(tmtidy, 
                     Traffic.mortality.men..per.100.000..age.adjusted = as.character(Traffic.mortality.men..per.100.000..age.adjusted))
tftidy <- gather(tf, "year", "mrate", 2:56)
tftidy2 <- transform(tftidy, 
                     Traffic.mortality.women..per.100.000..age.adjusted = as.character(Traffic.mortality.women..per.100.000..age.adjusted))

colnames(tmtidy2) <- c("country", "year", "m_rate")
colnames(tftidy2) <- c("country", "year", "f_rate")
str(tmtidy2)

ta <- left_join(tmtidy2, tftidy2)
str(ta)

tadata <- na.omit(ta)
tadata$year = substr(tadata$year, 2, 5)

## some plots
### compare rates of male mortality vs. female mortality
p1 <- ggplot(aes(x = as.numeric(year), y = m_rate), data = tadata) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p2 <- ggplot(aes(x = as.numeric(year), y = f_rate), data = tadata) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'blue')
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

### look for trends in countries with most data
str(tadata)
tadata$t_rate <- tadata$m_rate + tadata$f_rate
head(arrange(tadata, desc(t_rate)))
ta_data <- tadata %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
head(ta_data, 8)

p3 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "Australia")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p4 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "Canada")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p5 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "Ireland")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p6 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "Japan")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p7 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "Netherlands")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p8 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "New Zealand")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p9 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "United Kingdom")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
p10 <- ggplot(aes(x = as.numeric(year), y = t_rate), data = subset(tadata, country == "United States")) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2004, 4)) +
  geom_smooth(method = 'lm', color = 'red')
grid.arrange(p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)

### plot those 8 graphs on the same line graph
tatop <- tadata %>%
  filter(country == "Australia" | country == "Canada" |
           country == "Ireland" | country == "Japan" |
           country == "Netherlands" | country == "New Zealand" |
           country == "United Kingdom" | country == "United States")
ggplot(aes(x = as.numeric(year), y = t_rate), data = tatop) +
  geom_line(aes(colour = country))