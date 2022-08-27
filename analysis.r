#Import data set
cereal <- read.csv("cereal.csv")

#Explore data
library('tidyverse')
head(cereal)
glimpse(cereal)
unique(cereal$name)
summary(cereal)

#Correlation Anaylsis
library(reshape2)
cormatrix <- cereal[,4:16] %>% cor() %>% melt()
cormatrix %>% ggplot()+geom_tile(aes(Var1,Var2,fill=value),color='white')+scale_fill_gradient2(low='pink2',high='lightblue')+theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))
#finding 1. strong negative correlation between rating vs calories & rating vs sugars. 
#finding 2. positive correlation between rating vs protein & rating vs fiber & rating vs potass.

#which manufacturer produces cereal with avg highest calories ?
cereal %>% group_by(mfr) %>% summarise(avg_calories= mean(calories)) %>% ggplot()+geom_col(aes(mfr,avg_calories,fill=mfr))+labs(title='AVG Calories vs Manufacturer ',xlabs='manufacturer', ylabs='avg_calories')+theme_bw()

#which manufacturer produces cereal with avg highest sugars ?
cereal %>% group_by(mfr) %>% summarise(avg_sugars= mean(sugars)) %>% ggplot()+geom_col(aes(mfr,avg_sugars,fill=mfr))+labs(title='AVG Sugars vs Manufacturer ',xlabs='manufacturer', ylabs='avg_sugars')+theme_bw()

#which manufacturer produces cereal with avg highest rating ?
cereal %>% group_by(mfr) %>% summarise(avg_rating= mean(rating)) %>% ggplot()+geom_col(aes(mfr,avg_rating,fill=mfr))+labs(title='AVG Rating vs Manufacturer ',xlabs='manufacturer', ylabs='avg_rating')+theme_bw()

#Spotted that manufacturer N produces cereal with lowest calories & sugars.and, earns the highest rating.

#which manufacturer produces cereal with avg highest protein ?
cereal %>% group_by(mfr) %>% summarise(avg_protein= mean(protein)) %>% ggplot()+geom_col(aes(mfr,avg_protein,fill=mfr))+labs(title='AVG protein vs Manufacturer ',xlabs='manufacturer', ylabs='avg_protein')+theme_bw()
#which manufacturer produces cereal with avg highest fiber ?
cereal %>% group_by(mfr) %>% summarise(avg_fiber= mean(fiber)) %>% ggplot()+geom_col(aes(mfr,avg_fiber,fill=mfr))+labs(title='AVG fiber vs Manufacturer ',xlabs='manufacturer', ylabs='avg_fiber')+theme_bw()
#which manufacturer produces cereal with avg highest potass ?
cereal %>% group_by(mfr) %>% summarise(avg_potass= mean(potass)) %>% ggplot()+geom_col(aes(mfr,avg_potass,fill=mfr))+labs(title='AVG potass vs Manufacturer ',xlabs='manufacturer', ylabs='avg_potass')+theme_bw()

#Spotted that manufacturer N produces cereal with high fiber & potass, which helped with the rating.
#Spotted that manufacturer A produces cereal with high protein & potass, which also helped with the rating. 
