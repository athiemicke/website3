# install.packages(c("cowplot", "googleway", "ggplot2", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
# install.packages('libwgeom')
rm(list = ls())
library(googleway)
library(sf)
library(cowplot)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

#install.packages('rgeos')
library(rgeos)
#install.packages('tools')
library(tools)
#install.packages("ggiraph")
library(ggiraph)
library(ggrepel)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)

#Jena 	geo:50.927222,11.586111
#Oslo 	geo:59.916667,10.733333
#Berkeley 	geo:37.871667,-122.272778
#Pittsburgh  	geo:40.439722,-79.976389
#Nashville 	geo:36.166667,-86.783333
#Cold spring harbor 	geo:40.861944,-73.467222
#Irvine 	geo:33.669444,-117.823056
#Birmingham 		geo:33.653333,-86.808889

sites <- data.frame(longitude = c(11.586111, 10.733333,-122.272778,-79.976389,-86.783333,-73.467222,-117.823056,-86.808889
                                   ), latitude = c(50.927222, 59.916667,37.871667,40.439722,36.166667,40.861944,
                                                                  33.669444,  33.653333),
                    purpose= c('Education', 'Education', 'Research', 'Research', 'Education', 'Presentation',
                               'Presentation', 'Presentation'),
                    Institution=c('Friedrich-Schiller-University Jena','University of Oslo','UC Berkeley',
                                  'University of Pittsburgh', 'Vanderbilt University', 'Cold Spring Harbor',
                                  'UC Irvine','UA Birmingham'),
                    explanation=c('Bachelor of Science 2011\nMaster of Science 2014', 
                                  'Erasmus exchange 2010', 
                                  'Master Thesis student 2013/14',
                                  'RISE Intern 2011',
                                  'PhD Candidate 2014-2020',
                                  'CSHL Systtems Immunology 2019\nCSHL Cell Death 2019',
                                  'Cell Fate Symposium 2019',
                                  'Southeastern Immunology 2018\nNSF Computational Modeling Workshop 2019'
                                  )
                   #, onclick=sprintf("window.open(\"%s%s\")",'https://athiemicke.com')
                                  
)

#sites$onclick <- sprintf("window.open(\"%s%s\")",'https://athiemicke.com')
  #sprintf("window.open(\"%s%s\")","http://en.wikipedia.org/wiki/", as.character(crimes$state) )

#sites$onclick <-sprintf("window.open(\"%s%s\")",'https://athiemicke.com', '/')
sites$onclick <-'https://athiemicke.com'
sites$onclick <- sprintf("window.open(\"%s%s\")",
        "http://athiemicke.com/", as.character('projects.html') )

CV_Map <- ggplot(data = world) +
  geom_sf()+
  #geom_sf(data = states, fill = NA) +
  geom_label_repel(data = sites,aes(x = longitude, y = latitude,label=Institution
                                    ,color=purpose  #, fill=purpose
                                    ),
                  # alpha=0.3,
                   box.padding   = 0.35,
                   point.padding = 0.5,
                  # position = position_dodge(0.5),
                   label.size = 0.2,
                   size        = 3,
                   segment.color = 'grey50') +
  geom_point_interactive(data = sites, aes(x = longitude, y = latitude, color=purpose, fill=purpose
                                          , tooltip = explanation
                                          #, data_id = purpose
                                          , onclick=onclick
                                           ), size = 2, 
                         shape = 23) +
  # geom_point(data = sites, aes(x = longitude, y = latitude, color=purpose, fill=purpose), size = 2, 
  #            shape = 23) +
  coord_sf(xlim = c(-138, 28), ylim = c(24.5, 73), expand = FALSE)+
  theme(
    legend.box.background = element_rect(fill = "white", colour = "black"),
   # legend.key = element_rect(fill = "white", colour = "black",size = 1),
    # legend.position=c(0.6,1), legend.justification=c(0,1), 
    legend.title = element_blank(),
    legend.position="bottom",
    legend.justification = "center", 
   # legend.key.size = unit(1,"cm"),
  #  legend.key.height = unit(0.5,"cm"),
   # legend.key.width=unit(0.5, "cm"),
    panel.grid.major = element_blank(), 
       panel.background = element_rect(fill = "aliceblue"),
       axis.ticks= element_blank(),
       axis.text = element_blank(),
       axis.title = element_blank()
       )

x <- girafe(ggobj = CV_Map)
if( interactive() ) print(x)



###################


install.packages('plotly')
library(plotly)
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)



library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)

myData <- data.frame(
  x=c(1,2,3), 
  y=c(3,2,1),
  label=c("Google", "Bing", "R"),
  category=c("search", "search", "other"),
  urls=c("http://google.de", "http://bing.com", "http://r-project.org")
)

f <- function(p) {
  ply <- ggplotly(p)
  
  javascript <- HTML(paste("
                           var myPlot = document.getElementById('", ply$elementId, "');
                           myPlot.on('plotly_click', function(data){
                           var urls = ", toJSON(split(myData, myData$category)), ";
                           window.open(urls[data.points[0].data.name][data.points[0].pointNumber]['urls'],'_blank');
                           });", sep=''))  
  prependContent(ply, onStaticRenderComplete(javascript))
}

f(ggplot(myData, aes(x=x, y=y)) + geom_point(aes(text=label, color=category)))

