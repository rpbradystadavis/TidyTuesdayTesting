library(rvest)

library(dplyr)
library(rgdal)
library(gganimate)
library(maps)
library(data.table)
library(ggplot2)
library(gganimate)
library(tidyverse)
###Combining webscrape and Tidy Tuesday cleaning into a single script
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
 newcol$Population_Proportion = newcol$Population/sum(newcol$Population,na.rm = T)
 return(newcol)
 
})
NineteenC = do.call(rbind, nineteencent)

twentycent = lapply(Poplevels[10:20],function(X){
  #Formate Webscrape years 1900-2000
  newcol = as.data.frame(X[,c(1,2,4,7)])
  names(newcol) = names(Poplevels[[1]])
  newcol$Population = as.integer(gsub(',','',newcol$Population))
  newcol$Population_Proportion = newcol$Population/sum(newcol$Population,na.rm = T) #proportions
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

#Washington has a year Gap; should recalculate all 1860 but close enough :/
mergestates$Population_Proportion[mergestates$region == 'washington' &
                                    mergestates$Year == 1860] = 
  mergestates$Population_Proportion[mergestates$region == 'washington' & 
                                      mergestates$Year == 1850]

#####Load and edit tidy tuesday data
tuesdata <- tidytuesdayR::tt_load(2021, week = 16)
postofficedata = tuesdata$post_offices

#Sorry AK And HI you make the graph look bad w/o special projections :(
cont_us = postofficedata[!(postofficedata$state %in% c('AK','HI')),]
cont_us = cont_us[!is.na(cont_us$latitude),]

cont_us$id = 1:length(cont_us$name)
#Shrink the dataset, no need for extra columns
fordt = cont_us[,c('established','discontinued','id','latitude','longitude')]

#### Data Cleaning Section Clean established and Discontinued Years
##RM bad chars 
swap_subset = function(X){
  #swapping discontinued and established when established > discontinued
  disc = X$discontinued
  X$discontinued = X$established
  X$established = disc
  return(X)
}
year_cleaner = function(X){
        #Clean the years to 4 digits and ensuring established< discontinued
            X[which(nchar(as.character(X$discontinued)) != 4),] #Trim the 5s, add 1 to the 3
            X[which(nchar(as.character(X$discontinued)) == 5),2]= as.integer(
              substr(
                as.character(
                  as.data.frame(
                    X[which(nchar(as.character(X$discontinued)) == 5),])[,2]),1,4))
            X[which(nchar(as.character(X$discontinued)) != 4),]$discontinued = as.integer(paste('1',X[which(nchar(as.character(X$discontinued)) != 4),]$discontinued,sep = '') )
            # Discontinueds cleaned :)
            
            X[which(nchar(as.character(X$established)) != 4),]$established = as.integer(paste(X[which(nchar(as.character(X$established)) != 4),]$established,'0',sep = '')) # Add 0s to all of these. 
            
            X[which(X$established>X$discontinued),]# SWAP these after cleaning the above
            
            X$discontinued = ifelse(is.na(X$discontinued),max(X$discontinued,na.rm = T),X$discontinued)
            X[which(X$established>X$discontinued),] = swap_subset(X[which(X$established>X$discontinued),])
            
            
            
            
            X$established[which(is.na(X$established))] = min(X$established,na.rm = TRUE) #assume if NA its min, bad assumption
            
            return(X)
            }
fordt = year_cleaner(fordt)
newdt = setDT(fordt)[ , list(latitude = latitude, longitude = longitude,Year = seq(established, discontinued, by = as.integer(1))), by = id]
newdt = newdt[newdt$Year%%10== 0 & newdt$Year > 1800 ,] #Get only 10 year increments
#Garbage Cleanup to reduce memory for my super old laptop
rm(cont_us)
rm(postofficedata)
rm(tuesdata)
gc()

newdt$Year = as.integer(newdt$Year)




US = ggplot() + 
  geom_polygon( data=mergestates, aes(x=longitude, y=latitude, group=group,fill = Population_Proportion),
                color="black" )+ 
  scale_fill_viridis_c(option = "C",
                       na.value = 'black', 
                       breaks = c(0.02*0:9,max(mergestates$Population_Proportion,na.rm = T))
                       , labels = percent(c(0.02*0:9,max(mergestates$Population_Proportion,na.rm = T))))+
  theme_void() +  geom_point(data =newdt,aes(x =longitude,y=latitude)
                              ,color = 'white',size = .03,alpha =.1)  +
  guides(fill=guide_legend(title="Percentage of US Population"))
 

US = US + transition_states(Year) +
  labs(title = "Post Office Locations Plotted over Census Population in Year: {closest_state}")

animate(US, height = 450, width =700,fps = 4)
anim_save('Post_Office_Pop.gif')
