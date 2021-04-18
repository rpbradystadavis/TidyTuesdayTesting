library(rvest)

library(dplyr)
library(rgdal)
library(gganimate)
library(maps)
library(data.table)
library(ggplot2)
library(gganimate)
library(tidyverse)
###Webscrape census state stuff
decades = seq(1810,2000,10) #census years
url = 'https://en.wikipedia.org/wiki/1810_United_States_census'
Poplevels = lapply(decades,function(X){
  #Seems to work for most of 1900 - 2000 Different format though
  #need new functions for 1800-1900
  #Problem decades are 1820 1850? 1860 1870
  url = paste('https://en.wikipedia.org/wiki/',X,'_United_States_census',sep = '')
  if(!X %in% c(1830,1860,1870) ){
    Popstate = session(url) %>%
              html_elements('table') %>%
              .[[3]] %>%
              html_table()
    Popstate$Year = X
   }
  else if (X == 1830){
    Popstate = session(url) %>%
      html_elements('table') %>%
      .[[4]] %>%
      html_table()
    Popstate$Year = X
    
  }
  else if(X == 1860){
    Popstate = session(url) %>%
      html_elements('table') %>%
      .[[4]] %>%
      html_table()
    Popstate$Year = X
  }
  else{
    Popstate = session(url) %>%
      html_elements('table') %>%
      .[[5]] %>%
      html_table()
    Popstate$Year = X
  }
  return(Popstate)
  })
nineteencent = lapply(Poplevels[1:9],function(X){
  #Format Websrape Years 1810-1900
 newcol = X[,names(Poplevels[[1]])]
 newcol$Population = as.integer(gsub(',','',newcol$Population))
 newcol$Population_Proportion = newcol$Population*100/sum(newcol$Population,na.rm = T)
 return(newcol)
 
})
NineteenC = do.call(rbind, nineteencent)

twentycent = lapply(Poplevels[10:20],function(X){
  #Formate Webscrape years 1900-2000
  newcol = as.data.frame(X[,c(1,2,4,7)])
  names(newcol) = names(Poplevels[[1]])
  newcol$Population = as.integer(gsub(',','',newcol$Population))
  newcol$Population_Proportion = newcol$Population*100/sum(newcol$Population,na.rm = T) #proportions
  return(newcol)
})

TwentyC = do.call(rbind, twentycent)

AllDecades = rbind(NineteenC,TwentyC)

MainStates <- map_data("state")

AllDecades$State = tolower(AllDecades$State)

MainStates$ID = 1:nrow(MainStates) #Add unique id for SetDT
NewMain= setDT(MainStates)[ , list(order = order, #Data Table Group By 
                                   group = group,
                                   region = region,
                                   latitude = lat, 
                                   longitude = long,
                                   Year = seq(1810, 2000, by = as.integer(10))),
                            by = ID]



AllDecades$State = tolower(AllDecades$State) #make sure names match
mergestates = left_join(NewMain,AllDecades,by =c('region'='State','Year'='Year') )
mergestates$Year = as.integer(mergestates$Year)


US = ggplot() + 
  geom_polygon( data=mergestates[mergestates$Year == 2000,], aes(x=longitude, y=latitude, group=group,fill = Population_Proportion),
                color="black" )+ 
  scale_fill_viridis_c(option = "B",na.value = 'black')+
  theme_void() +  geom_point(data =newdt,aes(x =longitude,y=latitude)
                             ,color = 'white',size = .03,alpha =.1) 

US

US = ggplot() + 
  geom_polygon( data=mergestates, aes(x=longitude, y=latitude, group=group,fill = Population_Proportion),
                color="black" )+ 
  scale_fill_viridis_c(option = "B",na.value = 'black')+
  theme_void() +  geom_point(data =newdt,aes(x =longitude,y=latitude)
                              ,color = 'white',size = .03,alpha =.1) 
 

US = US + transition_states(Year) +
  labs(title = "Post Office Locations Plotted over Census Population in Year: {closest_state}")

animate(US, height = 450, width =700,fps = 4)

