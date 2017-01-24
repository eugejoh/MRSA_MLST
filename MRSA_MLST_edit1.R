# MRSA MLST Database
sessionInfo()
rm(list=ls())

# Taken from http://saureus.beta.mlst.net/ Jan 8, 2017

setwd("/Users/eugenejoh/Documents/Post-Grad/ID Data/MRSA/MLST")

profile<-read.csv("portable_profile_edit.csv",header=T,na.strings=c("",NA))
dim(profile) #dimensions of the read-in dataset
names(profile) # names of the columns in dataset
str(profile) #structure of the data frame


#################
# DATA CLEANING #
#################

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

# Looking for missing values in ST column
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


### What is the proportion of S vs. R S. aureus reports are there in North America?
table(profile$methicillin,useNA="always") #number of counts for R, S, Unknown, Unspecified and NA values for methicillin resistance
sum(is.na(profile$methicillin)) #counts of NAs in dataset
NA.MRSA <- profile[profile$country=="USA" | profile$country=="Canada",] #selection of North American countries
table(NA.MRSA$methicillin,useNA="always")
NA.MRSA <- NA.MRSA[complete.cases(NA.MRSA$country),] #removal of NAs


### How many STs are there in China, S Korea, Taiwan and Japan?
F.east <- profile[profile$country=="China" | profile$country=="South Korea" | profile$country=="Japan" | profile$country=="Taiwan",] #subset the data by Far East countries. | logic operator is OR, & would be AND
dim(F.east) #573 entries
factor(F.east$st) # 289 different STs
sum(is.na(F.east$st)) #15 missing values
table(F.east$st) #counts of STs
table(F.east$country) #counts of STs by country
FE.final<-F.east[complete.cases(F.east$country),] #removal of all rows that are missing an entry for country
dim(FE.final) #check the dimensions is 573-15 = ?

# Setup for using ggplot2 for pie chart for STs in North America
library(ggplot2)
c.NorthA.MRSA <-as.data.frame(table(NA.MRSA$st));names(c.NorthA.MRSA) <- c("ST","count") #data frame containing count frequencies of the STs
c.NorthA.MRSA<-c.NorthA.MRSA[order(-c.NorthA.MRSA$count),] #order the STs by count frequencies
c.NorthA.MRSA[1:10,] # top ten 

# setting theme and creating the plot
blank_t <- theme_minimal()+
  theme(
  axis.title.x = element_blank(), #remove axis titles
  axis.title.y = element_blank(),
  panel.border = element_blank(), #removes border
  panel.grid=element_blank(), #removes background grid
  axis.ticks = element_blank(), #removes axis ticks
  plot.title=element_text(size=20, face="bold",hjust=0.6), #set the plot title
  plot.subtitle=element_text(size=15,face=c("bold","italic"),hjust=0.6), #set subtitle
  axis.text.x=element_blank(), #remove text on x-axis
  legend.title=element_text(size=14,face="bold",vjust=0.5), #specify legend title
  panel.margin=unit(2,"cm") #set margins
  )

pie1<-ggplot(c.NorthA.MRSA[1:10,], aes(x="",y=count,fill=ST,order=ST)) + 
	geom_bar(width=1, stat="identity") +
	labs(title = "STs in North America", subtitle = "Canada and United States") +
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

print(pie1)
ggsave(plot=pie1,filename="North_America_STs_MLSTdatabase.png", width=5.75, height=7, dpi=120)
getwd()