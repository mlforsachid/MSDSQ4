#--------------------------------------------------------------------------------------------------------
# This notebook demonstrates different data visualization techniques unig ggplot2
#--------------------------------------------------------------------------------------------------------

# Load libraries
library(ggplot2)
library(gcookbook)

# Dataset - We will use built in mtcars dataset for plotting

#1] Create scatter plot

qplot(wt, mpg, data=mtcars)

#2] Create line plot

qplot(temperature, pressure, data=pressure, geom = 'line')

#3] Count plot is defualt qplot

qplot(mtcars$mpg, binwidth=10)

#4] BoxPlot

boxplot(len~supp +dose, data=ToothGrowth)

unique(ToothGrowth$dose)

#5] Create Bar Plot with fill and labels position = Dodge
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(aes(label=Weight), color='white', size=3,position=position_dodge(.9ggplot(df, aes(x=reorder(Seller, Num), y=Avg_Cost)) +
                                                                               geom_bar(stat='identity') +
                                                                               coord_flip()), vjust=2)

#6] Create bar plot with fill and labels. Position = Stack

library(dplyr)
library(plyr)
ce = arrange(cabbage_exp, Date, Weight)
ce = ddply(ce, 'Date', transform, label_y = cumsum(Weight))

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar))+
  geom_bar(stat='identity') +
  geom_text(aes(y=label_y, label=Weight), color='white', vjust=1.5)

