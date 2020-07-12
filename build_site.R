#Set our working directory. 
#This helps avoid confusion if our working directory is 
#not our site because of other projects we were 
#working on at the time. 
setwd("~/website3")


#install.packages("rmarkdown", type = "source")
library(rmarkdown)
library(rsconnect)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggridges)
library(devtools)
library(data.table)
library(scales)
library(ggrepel)
library(shinydashboard)
library(fontawesome)
library(sf)
library(ggiraph)
library(googleway)
library(sf)
library(cowplot)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(rgeos)
library(tools)
library(ggiraph)
library(ggrepel)
#Next we need to create a couple empty files inside your repository.


#render your sweet site. 
rmarkdown::render_site()

