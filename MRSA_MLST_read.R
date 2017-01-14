# MRSA MLST Database

rm(list=ls())

# Taken from http://saureus.beta.mlst.net/ Jan 8, 2017

setwd("/Users/eugenejoh/Documents/Post-Grad/ID Data/MRSA/MLST")

# portable_profile_edit #includes 
# filenames follow format...
# portable_genename_edit

#

profile<-read.csv("portable_profile_edit.csv",header=T,na.strings=c("",NA))
dim(profile) #dimensions of the read-in dataset
names(profile) # names of the columns in dataset
str(profile) #structure of the data frame

# Identify missing values
levels(profile$country) #names of countries included in the dataset
summary(profile$country) #summary of counts for each country


#mis_profile<-sapply(profile,function(x) sum(is.na(x)))

report.NA <- function(v){
	nam <- deparse(substitute(v))
	varNA <-paste0(nam,"NAs")
	#assign(newvar,sapply(v,function(x) sum(is.na(x))),envir = parent.frame())
	#assign(newvar,apply(is.na(v),2,sum))
	assign(varNA,as.data.frame(colSums(is.na(v))),envir=parent.frame())
	message(paste("Sum of NAs in",nam,"dataset:",varNA),appendLF=T)
}

report.NA(profile);ls()

table(profile$sex,profile$methicillin)

summary(profile$methicillin)

profileNAs
# 20 strain names are missing
# 4 STs are missing
# 15 count

no.st<-which(is.na(profile$st))

profile[no.st,]

# Why you need to change the function environment, http://emilkirkegaard.dk/en/?p=5718

# Which countries have the 

# Mapping out Countries using ggmap() package



install.packages(c("ggmap","maptools","maps"))
library(ggplot2,ggmap,maptools)
library(maps)

# HOW TO PLOT POINTS ON MAPS IN R
# https://www.r-bloggers.com/r-beginners-plotting-locations-on-to-a-world-map/
# http://stackoverflow.com/questions/11201997/world-map-with-ggmap/13222504#13222504

# setup cities of interest
visited <- c("SFO", "Chennai", "London", "Melbourne", "Johannesbury, SA")
ll.visited <- geocode(visited) #get 
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 

# http://stackoverflow.com/questions/41018634/continuous-colour-gradient-that-applies-to-a-single-geom-polygon-element-with-gg
library(ggplot2)
map <- map_data("world") #map of the world from
map$value <- setNames(sample(1:50, 252, T), unique(map$region))[map$region]

world.p <- ggplot(map, aes(long, lat, group=group, fill=value)) +
  geom_polygon()+
  coord_map(projection = "mercator",xlim=c(180,-180),ylim=c(75,-75))


p <- ggplot(map, aes(long, lat, group=group, fill=value)) + 
  geom_polygon() + 
  coord_quickmap(xlim = c(-50,50), ylim=c(25,75))
p + geom_polygon(data = subset(map, region=="Germany"), fill = "red")
