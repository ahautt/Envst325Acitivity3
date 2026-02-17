#IN CLASS WORK
# THE DATA
# read in data
# cloud is always lowercase
datCO2 <- read.csv("/cloud/project/annual-co-emissions-by-region.csv")

# check column names
colnames(datCO2)

# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

name.Ent

# PLOTTING DATA IN BASE R
plot(datCO2$Year, datCO2$CO2)

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (tons CO2)", #y axis label
     xlab = "Year") #x axis label

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )


# make a plot of US CO2 ----
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

# make a plot of US CO2 ----

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n", # turn off y axis
     ylim = c(0,6200000000)) # change y axis range
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")
legend("topleft",
       c("United States", "Mexico"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

##GGPLOT

ggplot(data = US, # data for plot
       aes(x = Year, y=CO2 ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)") # make axis labels +
  theme_classic()
  
  
  # subset data for just
  NorthA <- datCO2[datCO2$Entity == "United States" |
                     datCO2$Entity == "Canada" |
                     datCO2$Entity == "Mexico", ]
  
  ggplot(data = NorthA, # data for plot
         aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
    geom_point()+ # make points at data point
    geom_line()+ # use lines to connect data points
    scale_color_manual(values=c("lightsalmon1","lightpink2","goldenrod"))
    labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
    theme_classic()
  
#ClASSWORK
    
#read in data
datClimateChange <- read.csv("/cloud/project/climate-change.csv")
datClimateChange$date <- ymd(datClimateChange$Day)

#example plot
plot(datClimateChange$date, datClimateChange$temperature_anomaly)


#PROMPT 1
#Base R plot
NH <- datClimateChange[datClimateChange$Entity == "Northern Hemisphere",]
SH <- datClimateChange[datClimateChange$Entity == "Southern Hemisphere",]

plot(NH$date,
     NH$temperature_anomaly,
     type = "b",
     pch = 19, # symbol shape
     ylab = "tempurature anomaly (Degrees Celcius)", 
     xlab = "Year",
     yaxt = "n",
     col = "salmon")
axis ( 2, las=2)
points(SH$date, 
       SH$temperature_anomaly, 
       type = "b", 
       pch = 19, 
       col= "darkgoldenrod3")
legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("salmon", "darkgoldenrod3"),
       pch=19, bty= "n")

#GGPLOT

Hemispheres <- datClimateChange[datClimateChange$Entity == "Northern Hemisphere" | datClimateChange$Entity == "Southern Hemisphere", ]
ggplot(data = Hemispheres,
       aes(x = date, y=temperature_anomaly, color=Entity ) )+ 
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  scale_color_manual(values=c("salmon","darkgoldenrod3")) +
  labs(x="Year", y="temperature anomaly (Degrees Celcius") 

#PROMPT 2

totalCO2 <- NorthA %>% 
  group_by(Entity) %>%
  summarize( total = sum(CO2))

