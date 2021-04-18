install.packages('tidytuesdayR')
library(tidytuesdayR)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(gganimate)

theme_set(theme_bw())

library(gapminder)
###
##RB
##Post Office Scratch Script For Testing different functions 
#4 16 2021
## I saw some data scientists use gganimate to make a gif of post offices. Going to try that and maybe add some flair? 
##

tuesdata <- tidytuesdayR::tt_load(2021, week = 16)
postofficedata = tuesdata$post_offices
setwd(here::here())
#Start by just ripping everything from this tutorial https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
#install.packages('gganimate)

MainStates <- map_data("state")


US = ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="white" )
cont_us = postofficedata[!(postofficedata$state %in% c('AK','HI')),]
cont_us = cont_us[!is.na(cont_us$latitude),]

US = US + geom_point(data =cont_us,aes(x =longitude,y=latitude)
                     ,color = '#add8e6',size = .01,alpha =.1) + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


cont_us$id = 1:length(cont_us$name)
fordt = cont_us[,c('established','discontinued','id','latitude','longitude')]


fordt = as.data.frame(fordt)
##RM bad chars 
fordt[which(nchar(as.character(fordt$discontinued)) != 4),] #Trim the 5s, add 1 to the 3
fordt[which(nchar(as.character(fordt$discontinued)) == 5),2]= as.integer(
  substr(
    as.character(
      as.data.frame(
        fordt[which(nchar(as.character(fordt$discontinued)) == 5),])[,2]),1,4))
fordt[which(nchar(as.character(fordt$discontinued)) != 4),]$discontinued = as.integer(paste('1',fordt[which(nchar(as.character(fordt$discontinued)) != 4),]$discontinued,sep = '') )
# Discontinueds cleaned :)

fordt[which(nchar(as.character(fordt$established)) != 4),]$established = as.integer(paste(fordt[which(nchar(as.character(fordt$established)) != 4),]$established,'0',sep = '')) # Add 0s to all of these. 

fordt[which(fordt$established>fordt$discontinued),]# SWAP these after cleaning the above
swap_subset = function(X){
  disc = X$discontinued
  X$discontinued = X$established
  X$established = disc
  return(X)
}
fordt$discontinued = ifelse(is.na(fordt$discontinued),max(fordt$discontinued,na.rm = T),fordt$discontinued)
fordt[which(fordt$established>fordt$discontinued),] = swap_subset(fordt[which(fordt$established>fordt$discontinued),])








fordt$established[which(is.na(fordt$established))] = min(fordt$established,na.rm = TRUE) #assume if NA its min, bad assumption


newdt = setDT(fordt)[ , list(latitude = latitude, longitude = longitude,Year = seq(established, discontinued, by = as.integer(1))), by = id]
newdt = newdt[newdt$Year%%10== 0 & newdt$Year > 1800 ,]
#Garbage Cleanup to reduce memory for my super old laptop
rm(cont_us)
rm(postofficedata)
rm(tuesdata)
gc()

newdt$Year = as.integer(newdt$Year)
US = ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="white" )+ geom_point(data =newdt,aes(x =longitude,y=latitude)
                                                          ,color = 'white',size = .01,alpha =.1) 
newdt$Year
US = US + transition_time(Year) +
  labs(title = "Year: {frame_time}")
animate(US, height = 800, width =800)
anim_save("Gapminder_example.gif")
class(newdt$Year  )
#Smarter way to join?
#WHICH VALUES ARE NOT in the middle of nowhere?!?!

#Go download the over function and drop AK and HI
