library(dplyr)
library(tidyverse)
library(ggplot2)


View(diamonds)
ggplot(diamonds, aes(x = diamonds$carat , y = diamonds$price,  color = diamonds$cut )) + geom_point()


