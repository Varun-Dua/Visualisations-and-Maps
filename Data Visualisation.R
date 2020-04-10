obs_data <- read.csv("mly2275.csv", skip = 19, header = TRUE)
head(obs_data)

hist_base <- hist(obs_data$rain, main = "Histogram of Rain - base plotting system", xlab = "Rain", col = "Green")


with(obs_data, plot(rain, sun, main = "Scatter plot - base plotting system",xlab = "Rain", ylab = "Sun", type = "p", pch = 1 ))
with(subset(obs_data, year<2000), points(rain,sun, col = "blue"))
with(subset(obs_data, year>2000), points(rain,sun, col = "red"))
legend("topright", pch = 1, col = c("blue","red"), legend = c("1939-2000", "2000-2020"))

model<- lm(sun ~ rain, obs_data)
abline(model, lwd=2)

smoothScatter(obs_data$rain, obs_data$sun, xlab = "Rain", ylab = "Sun", main = "Smooth Scatter Plot")

library(ggplot2)
qplot(rain, data = obs_data, binwidth = 30)
qplot(rain,sun, data = obs_data, color = month, geom = c("point", "smooth"))

ggplot(obs_data, aes(rain)) + geom_histogram(color = "steelblue")
g<- ggplot(obs_data, aes(rain,sun))
g + geom_point(color = "steelblue") + geom_smooth()

apt_data <- read.csv("Second hand Appartment Prices.csv", skip = 1, header = TRUE)
apt_data

apt_data$National<-as.numeric(gsub(",", "", apt_data$National))
apt_data$Dublin<-as.numeric(gsub(",", "", apt_data$Dublin))
apt_data$Cork<-as.numeric(gsub(",", "", apt_data$Cork))
apt_data$Galway<-as.numeric(gsub(",", "", apt_data$Galway))
apt_data$Limerick<-as.numeric(gsub(",", "", apt_data$Limerick))
apt_data$Waterford<-as.numeric(gsub(",", "", apt_data$Waterford))
apt_data$Other.Areas<-as.numeric(gsub(",", "", apt_data$Other.Areas))

par(mfrow =c(2,2))

with(apt_data, plot(X, National, type = 'l', col = 2, main = "National Time Series Plot", xlab = "Year", lwd = 3))
with(apt_data, plot(X, Dublin, type = 'l', col = 3, main = "Dublin Time Series Plot", xlab = "Year", lwd = 3))
with(apt_data, plot(X, Cork, type = 'l', col = 4, main = "Cork Time Series Plot", xlab = "Year", lwd = 3)) 
with(apt_data, plot(X, Galway, type = 'l', col = 5, main = "Galway Time Series Plot", xlab = "Year", lwd = 3))
with(apt_data, plot(X, Limerick, type = 'l', col = 6, main = "Limerick Time Series Plot", xlab = "Year", lwd = 3))
with(apt_data, plot(X, Waterford, type = 'l', col = 7, main = "Waterford Time Series Plot", xlab = "Year", lwd = 3))
with(apt_data, plot(X, Other.Areas, type = 'l', col = 8, main = "Other Time Series Plot", xlab = "Year", lwd = 3))


xyplot(National ~ X, data = apt_data, type = 'l', lwd = 2, main = "National Time series", xlab = "Time")
xyplot(Dublin ~ X, data = apt_data, type = 'l', lwd = 2, main = "Dublin Time series", xlab = "Time")
xyplot(Cork ~ X, data = apt_data, type = 'l', lwd = 2, main = "Cork Time series", xlab = "Time")
xyplot(Galway ~ X, data = apt_data, type = 'l', lwd = 2, main = "Galway Time series", xlab = "Time")
xyplot(Limerick ~ X, data = apt_data, type = 'l', lwd = 2, main = "Limerick Time series", xlab = "Time")
xyplot(Waterford ~ X, data = apt_data, type = 'l', lwd = 2, main = "Waterford Time series", xlab = "Time")
xyplot(Other.Areas ~ X, data = apt_data, type = 'l', lwd = 2, main = "Other Areas Time series", xlab = "Time")



ggplot(apt_data, aes(X)) + geom_line(aes(X,National, color = "red")) + geom_line(aes(X,Dublin, color = "green")) + geom_line(aes(X,Cork, color = "yellow")) + geom_line(aes(X,Galway, color = "blue")) + geom_line(aes(X,Limerick, color = "brown")) + geom_line(aes(X,Waterford, color = "purple")) + geom_line(aes(X,Other.Areas, color = "orange"))


library(leaflet)
library(sp)
map_data<-read.csv("Irish Towns Co Ordinates.csv", header = TRUE)
t<- map_data$county == "Galway"
galway_data<-map_data[t,]
coords<-data.frame(galway_data$longitude, galway_data$latitude)
sp<-SpatialPointsDataFrame(coords = coords, galway_data)
map<-leaflet() %>% addProviderTiles(providers$OpenStreetMap) 
map_ireland<-map %>% addMarkers(data = sp, label = galway_data$name)


qplot(easting, northing, data = galway_data, geom = "text", label = name, size=0.25)