head(cars)
cor(cars$speed, cars$dist)
li <- lm(dist ~  speed , data=cars)
print(li)
summary(li)
reqd <- as.vector(c("absences","G1"))
Result <- df[,reqd]
colnames(df)
df <- read.csv("bikeshare.csv",sep=",")
head(df)
library(ggplot2)
ggplot(df,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()
colnames(df)
df$datetime <- as.POSIXct(df$datetime)
ggplot(df,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()
cor(df$temp,df$count)
cor(df[,c('temp','count')])
ggplot(df,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()
time.stamp <- df$datetime[4]
format(time.stamp, "%H")
head(df$datetime)
library(dplyr)
library(stringr)
library(tidyr)
df %>% separate(df, datetime, into = c("day", "hour"), sep = "\\s")
head(df)
df$hour
df2 <- df %>% mutate(datetime = str_replace(datetime, "\\s", "|")) %>% 
  separate(datetime, into = c("day", "hour"), sep = "\\|")
head(df2)
library(dplyr)
pl <- ggplot(filter(df2,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()


pl <- ggplot(filter(df2,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()


model <- lm (count~temp,df2)
summary(model)


df2$hour <- sapply(df2$hour,as.numeric)
model2 <- lm(count ~ . -casual - registered -datetime -atemp,df )
summary(model)