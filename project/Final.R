install.packages("tidyverse")
library(tidyverse)
# Load necessary libraries
library(ggplot2)


# List of countries for  time series analysis
countries <- c("United States", "Russia", "Germany", "China", "Japan", "India", "France", "United Kingdom", "Brazil")


# Filter the data for the countries of interest
data_filtered <- data[data$country %in% countries,]

# Time series chart for all selected countries
p <- ggplot(data_filtered, aes(x=time_period, y=obs_value, color=country))+
  geom_line()+  
  labs(title = "Time Series Chart for Global Superpowers",
  x="Year",
  y="% Vaccinated") +
  theme(legend.position="bottom", legend.box = "horizontal") +  
  scale_color_discrete(name = "Country") 
print(p)

# Bar chart for "Korea, Democratic People's Republic of"
data_korea <- data[data$country == "Korea, Democratic People's Republic of",]
p1 <- ggplot(data_korea, aes(x=time_period, y=obs_value)) +
  geom_bar(stat="identity") +
  labs(title = "Bar Chart for Democratic People's Republic of North Korea",
  y="% Vaccinated",
  x="Year")
print(p1)

# Filtered countries for the scatterplot
data_filtered <- data[data$country %in% countries,]

# Scatterplot
p2 <- ggplot(data_filtered, aes(x=time_period, y=obs_value, color=country)) +
  geom_point() +  
  geom_smooth(method=lm , color="red", se=FALSE) +  
  labs(title = "Scatterplot of vaccinations over the years",
  x="Year",
  y="% Vaccinated")+
  theme(legend.position="bottom", legend.box = "horizontal")+  
  scale_color_discrete(name = "Country")
print(p2)


# World map
map_world <-map_data("world")
map_data_join <- full_join(data, map_world, by = c("country" = "region"))
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()


# World Map 2
data_2022 <- data %>%
  filter(time_period == 2022)
map_data_join <- full_join(data_2022, map_world, by = c("country" = "region"))
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()



