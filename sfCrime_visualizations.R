##R script to produce visualizations for San Francisco Crime Data

# Map the occurences of the top 12 crimes in San Francisco

library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(readr)
library(lubridate)
library(stringr)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

train <- read_csv("") #TODO: Enter working directory

#a Google Cloud Platform API key must be entered in order to produce the required map 
register_google(key = "") #TODO: enter API key here 

#Enter San Francisco coordinates and define map attributes
map <- get_googlemap(center = c(lon = -122.448777, lat = 37.756891), zoom = 12, size = c(640, 640), scale= 2, 
                     format = c("png8"), maptype = c("roadmap"), language = "en-EN")

#plot the map
plot(map)

#define counts of Categories from the training dataset
counts <- summarise(group_by(train, Category), Counts=length(Category))
counts <- counts[order(-counts$Counts),]

#define top 12 crime categories
top12 <- train[train$Category %in% counts$Category[c(1,3:13)],]

#define map of top 12 crimes and its attributes
top_crimes_map <- ggmap(map) +
  geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Type of Crime")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("Top Crimes in San Francisco") +
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

#plot top 12 crimes map
plot(top_crimes_map)

#save the produced map as an png image 
ggsave("sf_top_crimes_map.png", top_crimes_map, width=15, height=10)

##Function to map occurance of specific crime
map_crime <- function(crime_df, crime) {
  filtered <- filter(crime_df, Category %in% crime)
  plot <- ggmap(map, extent='device') + 
    geom_point(data=filtered, aes(x=X, y=Y, color=Category), alpha=0.6)
  return(plot)
}

map_crime(train, c('WARRANTS'))

##Crime by Day
# Build a contingency table of all combinations of top 12 crime categories and days of the week
crimes_by_day <- table(top12$Category, top12$DayOfWeek)

# Reshape the table to plot
crimes_by_day <- melt(crimes_by_day)

# define column names of the table
names(crimes_by_day) <- c("Category","DayOfWeek","Count")

# Bar plot of crimes by Day
g <- ggplot(crimes_by_day,aes(x=Category, y=Count, fill = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~DayOfWeek) +
  theme(legend.position = "none")

plot(g)

#save produced bar plot
ggsave(g, file="Crimes_by_day.png", width=20, height=8)

##Crime by year
# Add a column for Year data
data <- top12
data$Year <- year(parse_date_time(data$Dates, "dmy HM"))

#reshape the table to plot
crimes_by_year <- table(data$Category,data$Year)

#combine dataframe occurance based on category
crimes_by_year <- melt(crimes_by_year)

#define column name of tables
names(crimes_by_year) <- c("Category","Year","Count")

#plot summary bar charts of top 12 categories vs year
g <- ggplot(crimes_by_year,aes(x=Category, y=Count,fill = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~Year) +
  theme(legend.position = "none")

plot(g)

ggsave(g, file="Crimes_by_year.png", width=20, height=8)


##Heatmap of Crimes
#define top 12 crimes
top12_crimes <- train[train$Category == c("WARRANTS", "ASSAULT", "BURGLARY", "DRUG/NARCOTIC", "FRAUD", "LARCENY/THEFT",
                                   "MISSING PERSON", "NON-CRIMINAL", "ROBBERY", "SUSPICIOUS OCC", "VANDALISM",
                                   "VEHICLE THEFT"),]

#Plot summary heatmaps of top 12 crimes
p <- ggmap(map) + stat_density2d(aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                                 bins = 15, geom = "polygon", data = top12_crimes) +
  scale_fill_gradient(low = "green", high = "red", name = "Crime\nDensity\nLevels") +
  scale_alpha(range = c(0.25, 0.75), guide = FALSE)  +
  facet_wrap(~Category, ncol =3) + ggtitle("Heatmap of San Francisco Crimes by Category")                                                                                             

#plot map
plot(p)

ggsave("Heatmap_Crimes.png", p, width=14, height=14)

##Heatmap of Individual Crime
top12_crimes <- train[train$Category == c("LARCENY/THEFT"),] #insert Category of choice e.g. "ASSAULT"

#plot heatmap of specific crime category
q <- ggmap(map) + stat_density2d(aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                                 bins = 15, geom = "polygon", data = top12_crimes) +
  scale_fill_gradient(low = "green", high = "red", name = "Crime\nDensity\nLevels") +
  scale_alpha(range = c(0.25, 0.75), guide = FALSE)  +
  ggtitle("Heatmap of San Francisco Larceny/Theft Crimes")                                                                                             

plot(q)

ggsave("Heatmap_Theft.png", q, width=14, height=10, units="in")


##Categorising based on 1st Month Day
crimes_on_1st <- top12

#Add data frame of month day to data
crimes_on_1st$mDay <- day(parse_date_time(crimes_on_1st$Dates, "dmy HM")) #using lubridate

#Extract from data where month day == 1
crimes_on_1st_day <- subset(crimes_on_1st, mDay == 1)

crimes_on_1st_day <- table(crimes_on_1st_day$Category)

crimes_on_1st_day <- melt(crimes_on_1st_day)

names(crimes_on_1st_day) <- c("Category", "Count")

#plot bar chart of number of crimes committed on the 1st by top 12 categories
ggplot(crimes_on_1st_day, aes(x = Category, y = Count)) +
  geom_bar(stat = "Identity", width = 0.5, fill = "tomato3")

#Add data frame of month day and Year to data
crimes_on_1st <- top12
crimes_on_1st$mDay <- day(parse_date_time(crimes_on_1st$Dates, "dmy HM"))
crimes_on_1st$Year <- year(parse_date_time(crimes_on_1st$Dates, "dmy HM"))

#Extract from data where month day == 1
crimes_on_1st_day <- subset(crimes_on_1st, mDay == 1)

crimes_on_1st_day <- table(crimes_on_1st_day$Category, crimes_on_1st_day$Year)

crimes_on_1st_day <- melt(crimes_on_1st_day)

names(crimes_on_1st_day) <- c("Category", "Year", "Count")

#plot top 12 crimes categories committed on the 1st, per year
a <- ggplot(crimes_on_1st_day, aes(x = Year, y = Count)) +
  geom_bar(stat = "Identity", width = 0.5, fill = "springgreen") +
  geom_smooth(method="lm") +
  scale_x_continuous(name = "Year", limits = c(2002, 2015), breaks = seq(2003,2014,2)) +
  ggtitle("Top 12 Crime Categories Committed On the 1st") +
  facet_wrap(.~Category)

plot(a)
ggsave(a, file="crimes_on_1st.png", width = 14, height = 10)

#Plot for total number of top 12 crimes commited on the 1st for every year
crimes_on_1st <- top12
crimes_on_1st$mDay <- day(parse_date_time(crimes_on_1st$Dates, "dmy HM"))
crimes_on_1st$Year <- year(parse_date_time(crimes_on_1st$Dates, "dmy HM"))
crimes_on_1st_day <- subset(crimes_on_1st, mDay == 1)

crimes_on_1st_day <- table(crimes_on_1st_day$Year)

crimes_on_1st_day <- melt(crimes_on_1st_day)

names(crimes_on_1st_day) <- c("Year", "Count")

b <- ggplot(crimes_on_1st_day, aes(x = Year, y = Count)) +
  geom_bar(stat = "Identity", fill = "springgreen") +
  geom_smooth(method = "lm") +
  scale_x_continuous(name = "Year", limits = c(2002, 2015), breaks = seq(2003, 2014, 2)) +
  ggtitle("Number of Crimes Committed on the 1st of every Month per Year")

plot(b)

ggsave(b, file = "crimes_on_1st_yearly.png", width = 14 ,height = 10)

##Crimes committed during day vs. night hours
crime_time <- top12

#create hour column
crime_time$hour <- hour(parse_date_time(crime_time$Dates, "dmy HM"))

#Classify and create dayOrNight column
crime_time$DayOrNight <- ifelse(crime_time$hour >= 8 & crime_time$hour <= 18, "day", "night" )

crime_time <- table(crime_time$Category, crime_time$DayOrNight)

crime_time <- melt(crime_time)

names(crime_time) = c("Category", "DayOrNight", "Count")

c <- ggplot(crime_time, aes(x=Category, y =Count, fill=DayOrNight)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
  ggtitle("Top 12 Crimes committed during Day vs Night")
plot(c)

ggsave(c, file="crime_dayOrNight.png", width = 14, height = 10)

##Crimes committed on the street or in a block
crime_loc <- top12

#identify and create StreetOrBlock Column through an ifelse
crime_loc$StreetOrBlock <- ifelse(str_detect(crime_loc$Address,"Block"), "Block", "Street")

#Create table of specific Columns
crime_loc <- table(crime_loc$Category, crime_loc$StreetOrBlock)

crime_loc <- melt(crime_loc)

names(crime_loc) = c("Category", "StreetOrBlock", "Count")

d <- ggplot(crime_loc, aes(x=Category, y =Count, fill=StreetOrBlock)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
  ggtitle("Top 12 Crimes committed on a Street vs Block")

plot(d)

ggsave(d, file="crime_loc.png", width = 14, height= 10)
