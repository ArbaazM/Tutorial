library(tidyverse)
library(ggplot2)
library(cowplot)

set.seed(123)
n = 1000
x = rnorm(n, mean = 10, sd = 1)
y2e = 2*x + 0.5*x^2 + rnorm(n, sd = 1)
y3e = 0.7*x + 2/3*x^2 + 0.2*x^3 + rnorm(n, sd = 20)
y4e = 0.5*x + 0.4*x^2 + 0.05*x^3 + 0.01*x^4 + rnorm(n, sd = 100)

dat = data.frame(x, y2e, y3e, y4e)

p1 = dat%>% ggplot(aes(x = x))+ geom_density()
p1

p2 = dat %>% ggplot(aes(x = x, y = y2e)) + geom_point()
p3 = dat %>% ggplot(aes(x = x, y = y3e)) + geom_point()
p4 = dat %>% ggplot(aes(x = x, y = y4e)) + geom_point()

plot_grid(p1, p2, p3, p4, ncol = 2)


library(readr)
offense <- read_csv("offense.csv")

offense%>%
  group_by(State, Year)%>%
  summarise(citypop = mean(Offense_City_Pop, na.rm = T),
            crime = mean(Crime_Index_10k, na.rm = T))%>%
  mutate(Year = as.factor(Year))%>%
  ggplot(aes(x = log(citypop), y = log(crime), col = Year))+
  geom_point()+
  geom_smooth(se = F)
