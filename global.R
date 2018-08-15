

#GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL

#install packages
library(devtools)
library(shiny)
library(rgdal)
library("googlesheets")
library(dplyr)
library(leaflet)
library(highcharter)
#library(xts)
library(zoo)
library(ggplot2)
#install_github("nara907/IDPukr")
library(IDPukr)
library(rmapshaper)
library(classInt)
library(geosphere)



#Read in shapefiles from IDPukr package

oblastsF<- system.file("extdata/Admin1_Oblasts.shp", package= "IDPukr")
oblasts<- readOGR(dsn= oblastsF, layer= "Admin1_Oblasts")
rayonsF<- system.file("extdata/Admin2_noNGCA.shp", package="IDPukr")
rayons<- readOGR(dsn= rayonsF, layer="Admin2_noNGCA")
LoCF<- system.file("extdata/ContactLine.shp", package="IDPukr")
LoC<- readOGR(dsn= LoCF, layer="ContactLine")
NGCAF<- system.file("extdata/NGCA.shp", package="IDPukr")
NGCA<- readOGR(dsn= NGCAF, layer="NGCA")
LymanskyiF<- system.file("extdata/NoData.shp", package="IDPukr")
Lymanskyi<- readOGR(dsn= LymanskyiF, layer="NoData")



#project to lat/lng to work in leaflet

rayons<-spTransform(x= rayons, CRSobj = proj4string(oblasts))



#Read in GoogleSheets
GSh<- gs_key('1hmA1CKxe560GOBesi7UQ4AKwFzxdItb48SW4U6QEgdE')
IDPdata<- GSh %>% gs_read(ws = 1) #first worksheet



#Wrangle Data into appropriate formats
IDPdata<- as.data.frame(IDPdata)
IDPd<- IDPdata %>% mutate_if(is.integer, as.numeric) #Change number data types to numeric

rayons@data$KOATUU<- as.numeric(as.character(rayons@data$KOATUU)) #change KOATUU from factor to numeric
rayons@data$Total_2017<- as.numeric(as.character(rayons@data$Total_2017)) #change KOATUU from factor to numeric



#Merge data from Google Sheet with Rayon shp file
Rshp <- merge(x=rayons,y=IDPdata, by.x="KOATUU", by.y= "KOATUU") 



#add columns for proportion and change
IDPcol<-ncol(Rshp) #get number of IDP column
Rshp$prop<- as.integer((Rshp[[IDPcol]]*1000)/ Rshp$Total_2017) #prop column
Rshp$chng<- (Rshp[[IDPcol]]-Rshp[[IDPcol-1]]) #chng column

#bins for data

bins1 <- classIntervals(Rshp[[IDPcol]], n = 5, style = "jenks")
bins1<-bins1$brks

bins2<- classIntervals(Rshp$prop, n = 6, style = "jenks")
bins2<-bins2$brks

b3breaks<- c(min(Rshp$chng), -200, 0, 50, 200, max(Rshp$chng))
bins3<-classIntervals(Rshp$chng, 5, style="fixed", fixedBreaks=b3breaks)
bins3<-bins3$brks


# Pretty rounded pals for the legend
palnumL <- colorBin(
  palette = "BuPu",
  domain = Rshp[[IDPcol]],
  bins = signif(bins1, digits=1)
)

palpropL <- colorBin(
  palette = "OrRd",
  domain = Rshp$prop,
  bins = signif(bins2, digits=1),
  reverse = FALSE
)
palchngL <- colorBin(
  palette = "RdBu",
  domain = Rshp$chng,
  bins = signif(bins3, digits=2),
  reverse= TRUE
)



#POPUPS
numIDP_Popup <- paste0( Rshp$Adm2TYPE, ": ", "<b>", Rshp$adm2NameLa, "</b>", "<br>",
                        " Number of IDPs: ", "<b><h4>",Rshp[[IDPcol]], "</h4></b>" )

propIDP_Popup <- paste0( Rshp$Adm2TYPE, ": ", "<b>", Rshp$adm2NameLa, "</b>", "<br>",
                         " Proportion of IDPs: ","<b><h4>", Rshp$prop, "</h4></b>" )

chngIDP_Popup <- paste0( Rshp$Adm2TYPE, ": ", "<b>", Rshp$adm2NameLa, "</b>", "<br>",
                         " Change in number of IDPs: ","<b><h4>", Rshp$chng, "</h4></b>" )



#Create variables for current and previous data dates
currentD<-as.Date(colnames(Rshp@data[IDPcol]))
currentD <- format(currentD, "%d %B %Y")
currentD <- as.character(currentD)

priorDn<-IDPcol-1
priorD<- as.Date(colnames(Rshp@data[priorDn]))
priorD <- format(priorD, "%d %B %Y")
priorD <- as.character(priorD)



# Reduce shapfile complexity for fast leaflet loading,  AND project
Rshp<-ms_simplify(Rshp)
Rshp <- spTransform(x = Rshp, 
                    CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
oblasts<-ms_simplify(oblasts)
oblasts<- spTransform(x = oblasts, 
                      CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
LoC<-ms_simplify(LoC)
LoC <- spTransform(x = LoC, 
                   CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
NGCA<-ms_simplify(NGCA)
NGCA <- spTransform(x = NGCA, 
                    CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Lymanskyi<-ms_simplify(Lymanskyi)
Lymanskyi <- spTransform(x = Lymanskyi, 
                         CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


lopt = labelOptions(noHide = TRUE,
                    direction = 'top',
                    textOnly = TRUE)
#Create Table for Chart values

IDPtable<- select(IDPd,-1, -3:-5)

# Get polygons centroids
centroids <- as.data.frame(centroid(oblasts))
colnames(centroids) <- c("lon", "lat")
centroids <- data.frame("ID" = 1:nrow(centroids), centroids)

# Create SpatialPointsDataFrame object
coordinates(centroids) <- c("lon", "lat") 
proj4string(centroids) <- proj4string(oblasts) # assign projection
centroids@data <- sp::over(x = centroids, y = oblasts, returnList = FALSE)
centroids1 <- as.data.frame(centroid(oblasts))
colnames(centroids1) <- c("lon", "lat")
centroids@data<- cbind(centroids@data, centroids1)

#fix name issues
centroids@data$adm1NameLa<-as.character(centroids@data$adm1NameLa)
centroids@data[12,4]<-as.character("Odeska")
centroids<-centroids[-3,]
centroids@data[1,4]<-as.character("Autonomous Republic of Crimea")


#Create UKR label

UKRl<- as.data.frame(cbind(31.090,  49.947908))
colnames(UKRl) <- c("lon", "lat")
UKRl <- data.frame("ID" = 1:nrow(UKRl), UKRl)
coordinates(UKRl) <- c("lon", "lat") 
proj4string(UKRl) <- proj4string(oblasts)
UKRl1<- as.data.frame(cbind(31.090,  49.947908))
UKRl@data<-cbind(UKRl@data, "UKRAINE", UKRl1 )
colnames(UKRl@data) <- c("index","name","lon", "lat")





# highchart()%>% 
# hc_add_series_times_values(dates=df$ind,
#                            values= df$values, 
#                            name = "Number of IDPs") %>% 
# hc_yAxis(title=list(text="Number of IDPs"), opposite = FALSE)

# # 
# numIDP<- leaflet() %>%
#     addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
#     setView(37.393, 48.732, zoom = 7)%>%
#     addPolygons(data = Lymanskyi,     #NO VALUES RAYON
#                 color = "grey",
#                 fillOpacity = 0.6,
#                 label = "No Data",
#                 highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                     bringToFront = TRUE, sendToBack = TRUE),
#                 popup = paste0('<h7 style="color:black;">',  "Rayon:  ", "<b>", Lymanskyi$adm2NameLa, "</b>", '</h7>', "<br>",
#                                " Number of IDPs: N/A"),
#                 opacity = 0.1) %>%
# 
#     addPolygons(data = oblasts,    # OBLASTS , LABELS
#                 # label = oblasts$adm1NameLa,
#                 options = pathOptions(clickable = FALSE),
#                 # labelOptions= labelOptions(
#                 #   interactive= FALSE,
#                 #   permanent= TRUE,
#                 #   opacity= 1,
#                 #   textOnly = TRUE,
#                 #   style = list(
#                 #          "color"= "black",
#                 #          "font-weight" = "normal",
#                 #          "font-size" = "15px",
#                 #          "font-family"= "Impact")
#                 # ),
#                 color = "black",
#                 weight= 1,
#                 fill=TRUE,
#                 fillOpacity = 0.1,
#                 opacity = 0.9 ) %>%
# 
#     addPolygons(data = NGCA,    # NON-GOVERNMENT CONTROLED AREA
#                 color = "black",
#                 fill= TRUE,
#                 label = "Non-Government Controled Area",
#                 fillColor= "black",
#                 fillOpacity = 0.5,
#                 opacity = 1) %>%
# 
#   addPolygons(data= Rshp,    # NUMBER OF IDPS
#                 color = "black",
#                 weight = .4,
#                 opacity = 1.0,
#                 smoothFactor = 0.5,
#                 fill = TRUE,
#                 fillColor = ~palIDPnum(Rshp[[IDPcol]]),
#                 fillOpacity = .8,
#                 layerId = ~Raion_Name_Eng,
#                 highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                     bringToFront = TRUE, sendToBack = TRUE),
#                 popup = numIDP_Popup,
#                 group= "Number of IDPs") %>%
# 
#   addLegend("bottomright", pal = palIDPnum, values = Rshp[[IDPcol]],
#             title = "Number of IDPs",
#             opacity = 1) %>%
# 
#     addPolylines(data = LoC,    #LINE OF CONTACT
#                  color = "red",
#                  opacity = 0.7)


# 
# ChngIDP<- leaflet() %>%
#   addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
#   setView(37.393, 48.732, zoom = 7)%>%
#   addPolygons(data = Lymanskyi,     #NO VALUES RAYON
#               color = "grey",
#               fillOpacity = 0.6,
#               label = "No Data",
#               highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                   bringToFront = TRUE, sendToBack = TRUE),
#               popup = paste0('<h7 style="color:black;">',  "Rayon:  ", "<b>", Lymanskyi$adm2NameLa, "</b>", '</h7>', "<br>",
#                              " Number of IDPs: N/A"),
#               opacity = 0.1) %>%
#   
#   addPolygons(data = oblasts,    # OBLASTS , LABELS
#               label = oblasts$adm1NameLa,
#               options = pathOptions(clickable = FALSE),
#               labelOptions= labelOptions(
#                 interactive= FALSE,
#                 permanent= TRUE,
#                 opacity= 1,
#                 textOnly = TRUE,
#                 style = list(
#                   "color"= "black",
#                   "font-weight" = "normal",
#                   "font-size" = "15px",
#                   "font-family"= "Impact")
#               ),
#               color = "black",
#               weight= 1,
#               fill=TRUE,
#               fillOpacity = 0.1,
#               opacity = 0.9 ) %>%
#   
#   addPolygons(data = NGCA,    # NON-GOVERNMENT CONTROLED AREA
#               color = "black",
#               fill= TRUE,
#               label = "Non-Government Controled Area",
#               fillColor= "black",
#               fillOpacity = 0.5,
#               opacity = 1) %>%
#   
#   addPolygons(data= Rshp,     # CHANGE IN IDPS
#               color = "black",
#               weight = .4,
#               opacity = 1.0,
#               smoothFactor = 0.5,
#               label = Rshp$adm2NameLa,
#               fill = TRUE,
#               fillColor = ~palIDPchng(Rshp$chng),
#               fillOpacity = 0.6,
#               layerId = ~Raion_Name_Eng,
#               highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                   bringToFront = TRUE, sendToBack = TRUE),
#               popup = chngIDP_Popup,
#               group= "Change in IDPs") %>%
#   
#   addLegend("bottomright", pal = palIDPchng, values = Rshp$prop,
#             title = "IDPs per 1000 Hosting Population",
#             opacity = 1) %>%
#   
#   addPolylines(data = LoC,    #LINE OF CONTACT
#                color = "red",
#                opacity = 0.7)
# 
# 
# PropIDP<- leaflet() %>%
#   addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
#   setView(37.393, 48.732, zoom = 7)%>%
#   addPolygons(data = Lymanskyi,     #NO VALUES RAYON
#               color = "grey",
#               fillOpacity = 0.6,
#               label = "No Data",
#               highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                   bringToFront = TRUE, sendToBack = TRUE),
#               popup = paste0('<h7 style="color:black;">',  "Rayon:  ", "<b>", Lymanskyi$adm2NameLa, "</b>", '</h7>', "<br>",
#                              " Number of IDPs: N/A"),
#               opacity = 0.1) %>%
#   
#   addPolygons(data = oblasts,    # OBLASTS , LABELS
#               label = oblasts$adm1NameLa,
#               options = pathOptions(clickable = FALSE),
#               labelOptions= labelOptions(
#                 interactive= FALSE,
#                 permanent= TRUE,
#                 opacity= 1,
#                 textOnly = TRUE,
#                 style = list(
#                   "color"= "black",
#                   "font-weight" = "normal",
#                   "font-size" = "15px",
#                   "font-family"= "Impact")
#               ),
#               color = "black",
#               weight= 1,
#               fill=TRUE,
#               fillOpacity = 0.1,
#               opacity = 0.9 ) %>%
#   
#   addPolygons(data = NGCA,    # NON-GOVERNMENT CONTROLED AREA
#               color = "black",
#               fill= TRUE,
#               label = "Non-Government Controled Area",
#               fillColor= "black",
#               fillOpacity = 0.5,
#               opacity = 1) %>%
#   
#   addPolygons(data= Rshp,     # PROPORTION OF IDPS
#               color = "black",
#               weight = .4,
#               opacity = 1.0,
#               smoothFactor = 0.5,
#               fill = TRUE,
#               fillColor = ~palIDPprop(Rshp$prop),
#               fillOpacity = .8,
#               layerId = ~Raion_Name_Eng,
#               highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                   bringToFront = TRUE, sendToBack = TRUE),
#               popup = propIDP_Popup,
#               group= "Proportion of IDPs") %>%
#   addLegend("bottomright", pal = palIDPchng, values = Rshp$chng,
#             title = "Change in Number IDPs",
#             opacity = 1) %>%
#   
#   addPolylines(data = LoC,    #LINE OF CONTACT
#                color = "red",
#                opacity = 0.7) 




