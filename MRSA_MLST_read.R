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

#################
# DATA CLEANING #
#################

# Identify missing values
levels(profile$country) #names of countries included in the dataset
summary(profile$country) #summary of counts for each country

# Change country to character from factor type
profile$country <- as.character(profile$country)

# Standardize resistiance factors ($methicillin, $vancomycin)
summary(profile$methicillin)
str(profile$methicillin) # nine levels for definitions for resistance?

# we want to simplify this to binary output for resistant(R) and susceptible(S)

factor(profile$methicillin)
# factor(profile$methicillin) gives us I, MIC 4mgl/L, MIC 64mg/L, r, R, s, S, Unknown, Unspecified
# now we merge r with R and s with S, while renaming the MICs
levels(profile$methicillin) <- c("I","MIC4","MIC64","R","R","S","S","Unknown","Unspecified")
# we also recognize MIC's of 4mg/L and 64mg/L are considered resistant. Important to note that breakpoints for methicillin MIC tests are now limited - see CDC website for info - https://www.cdc.gov/mrsa/lab/

# we are also going to combine the I, MIC4, MIC64 into the resistant category (R)

levels(profile$methicillin) <- c("R","R","R","R","S","Unknown","Unspecified")
# now we only have 4 levels : R, S, Unknown, Unspecified
factor(profile$methicillin)
################
################

#mis_profile<-sapply(profile,function(x) sum(is.na(x)))

report.NA <- function(v){
	nam <- deparse(substitute(v))
	varNA <-paste0(nam,"NAs")
	#assign(newvar,sapply(v,function(x) sum(is.na(x))),envir = parent.frame())
	#assign(newvar,apply(is.na(v),2,sum))
	assign(varNA,as.data.frame(colSums(is.na(v))),envir=parent.frame())
	message(paste("Sum of NAs in",nam,"dataset:",varNA),appendLF=T)
} # Why you need to change the function environment, http://emilkirkegaard.dk/en/?p=5718

report.NA(profile);ls()

summary(profile$st) #2594 different types of STs, 4 missing values

summary(profile$methicillin) #review the MIC for each one (which ones are considered R or S)

profileNAs
# 20 strain names are missing
# 4 STs are missing
# 15 countries

################
################

no.st<-which(is.na(profile$st)) #row index of those with missing STs in dataset

profile[no.st,] # info on the missing values of STs
# England, Eire (Ireland), 2 USA

profile[which(is.na(profile$st)),]

####################
# QUESTIONS TO ASK #
####################

### Which countries have reported ST 239?
id.239 <- which(profile$st==239) #index of rows with ST-239
st.239 <-profile[id.239,] #new dataframe of only ST-239
sort(unique(st.239$country)) # alphabetical list of countries with ST-239

### Which countries have report ST8 spa type t008?
id.ST8.t008 <- which(profile$st==8 & profile$spa_type=="t008")
ST8.t008 <- profile[id.ST8.t008,]
dim(ST8.t008)
sort(unique(ST8.t008$country))
length(sort(unique(ST8.t008$country)))

# What is the proportion of S vs. R S. aureus reports are there in North America?
table(profile$methicillin,useNA="always") #number of counts for R, S, Unknown, Unspecified and NA values for methicillin resistance
sum(is.na(profile$methicillin)) #counts of NAs in dataset
NA.MRSA <- profile[profile$country=="USA" | profile$country=="Canada",] #selection of North American countries
table(NA.MRSA$methicillin,useNA="always")
NA.MRSA <- NA.MRSA[complete.cases(NA.MRSA$country),] #removal of NAs

# ggplot
# http://stackoverflow.com/questions/16184188/ggplot-facet-piechart-placing-text-in-the-middle-of-pie-chart-slices
library(ggplot2)
install.packages("viridis")
library(viridis)
NA.MRSA$st <- as.numeric(NA.MRSA$st) #convert the STs to numeric type
c.NorthA.MRSA <-as.data.frame(table(NA.MRSA$st));names(c.NorthA.MRSA) <- c("ST","count") #data frame containing count frequencies of the STs
c.NorthA.MRSA<-c.NorthA.MRSA[order(-c.NorthA.MRSA$count),] #order the STs by count frequencies
c.NorthA.MRSA[1:10,] # top ten 

blank_t <- theme_minimal()+
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=20, face="bold",hjust=0.6),
  plot.subtitle=element_text(size=15,face=c("bold","italic"),hjust=0.6),
  axis.text.x=element_blank(),
  legend.title=element_text(size=14,face="bold",vjust=0.5),
  panel.margin=unit(2,"cm")
  )

ggplot(c.NorthA.MRSA[1:10,], aes(x="",y=count,fill=ST,order=ST)) + 
	geom_bar(width=1, stat="identity") +
	ggtitle(element_text(size=50))+
	labs(title = "STs in North America", subtitle = "Canada and United States",y=NULL) +
	coord_polar(theta="y")+
	geom_text(aes(label = scales::percent(count/sum(c.NorthA.MRSA[1:10,2]))),size=4, position = position_stack(vjust = 0.6)) +
	blank_t + 
	scale_fill_manual(guide_legend(title="STs"),
values=c("#e6b4c9",
"#bce5c2", # colour palette resource for data scientists
"#c7b5de", # http://tools.medialab.sciences-po.fr/iwanthue/
"#e3e4c5",
"#a1bbdd",
"#e5b6a5",
"#99d5e5",
"#bdbda0",
"#dcd1e1",
"#99c6b8"))


# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
# http://graphicdesign.stackexchange.com/questions/3682/where-can-i-find-a-large-palette-set-of-contrasting-colors-for-coloring-many-d


# changing order of scale # https://learnr.wordpress.com/2010/03/23/ggplot2-changing-the-default-order-of-legend-labels-and-stacking-of-data/
install.packages("randomcoloR")
library(randomcoloR)
distinctColorPalette(k=15)

	#+facet_grid(Channel ~ ., scales = "free")

bp <- ggplot(NA.MRSA,aes(x="",y=st,fill=country)) + geom_bar(width = 1,stat = "identity")
bp
pie <- bp + coord_polar("y",start=0)
pie + geom_text(aes(label = country), position = position_stack(vjust = 0.5))

bp1 <- ggplot(NA.MRSA,aes(x=factor(1),fill=country)) + geom_bar(width = 1)
bp1
pie1 <- bp1 + coord_polar("y",start=0)
pie1

pie + scale_fill_brewer(palette="Blues")

# How many STs are there in China, S Korea, Taiwan and Japan?
F.east <- profile[profile$country=="China" | profile$country=="South Korea" | profile$country=="Japan" | profile$country=="Taiwan",] #subset the data by Far East countries. | logic operator is OR, & would be AND
dim(F.east) #573 entries
factor(F.east$st) # 289 different STs
sum(is.na(F.east$st)) #15 missing values
table(F.east$st) #counts of STs
table(F.east$country) #counts of STs by country
FE.final<-F.east[complete.cases(F.east$country),] #removal of all rows that are missing an entry for country
dim(FE.final) #check the dimensions is 573-15 = ?

# Country Specific Details
# China
length(which(FE.final$country=="China")) #counts for China
FE.China <- FE.final[FE.final$country=="China",]
FE.China$country
dim(FE.China) # 229 entries
sort(table(FE.China$st),decreasing=T) #most common STs are 97, 239 and 398
sort(table(FE.China$st),decreasing=T)[1:5] #top 10 most frequent

factor(FE.China$st) #163 different type of STs
China.tab <- table(FE.China$st)

bp <- ggplot(FE.China,aes(x=st)) + geom_bar()
bp + coord_polar("y",start=0)

x <- c("Insurance", "Insurance", "Capital Goods", "Food markets", "Food markets")
tt <- table(x)
names(tt[tt==max(tt)])

# S Korea

# Taiwan

# Japan


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
