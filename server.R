


# SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER

server <- function(input, output) {
  
  #__________________________________Add legend toggling capabiltiies____________________________
  
  # observeEvent(input$map_groups,{
  #   map <- leafletProxy("map")
  #   map %>% clearControls()
  #   if (input$map_groups == "Number of IDPs") {
  #     map %>% addLegend("bottomleft", pal = palIDPnum, values = Rshp[[IDPcol]],
  #                       title = "Number of IDPs",
  #                       opacity = 1)
  #   }
  #   else if (input$map_groups == "Proportion of IDPs") {
  #     map %>% addLegend("bottomleft", pal = palIDPprop, values = Rshp$prop,
  #                       title = "Proportion of IDPs",
  #                       opacity = 1)
  #   }
  #   else if (input$map_groups == "Change in IDPs") {
  #     map %>% addLegend("bottomleft", pal = palIDPchng, values = Rshp$chng,
  #                       title = "Change in IDPs",
  #                       opacity = 1)
  # }})
  
  #_________________________Add map consisting of several layers and customization___________________
  
  output$map <- renderLeaflet({
    numIDP<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = Lymanskyi,     #NO VALUES RAYON
                  color = "grey",
                  fillOpacity = 0.6,
                  label = "No Data",
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  popup = paste0('<h7 style="color:black;">',  "Rayon:  ", "<b>", Lymanskyi$adm2NameLa, "</b>", '</h7>', "<br>",
                                 " Number of IDPs: N/A"),
                  opacity = 0.1)%>%
      addLabelOnlyMarkers(centroids, lat=centroids$lat, lng=centroids$lon, label=as.character(centroids$adm1NameLa),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 0.6,
                            style = list(
                              "color"= "black",
                              "font-size" = "15px",
                              "font-family"= "Helvetica",
                              "font-weight"= 600)
                          )) %>%
      addLabelOnlyMarkers(UKRl, lat=UKRl$lat, lng=UKRl$lon, label=as.character(UKRl$name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 1,
                            style = list(
                              "color"= "black",
                              "font-size" = "24px",
                              "font-family"= "Helvetica",
                              "font-weight"= 800,
                              "letter-spacing"= "3px")
                          )) %>%
      
      addPolygons(data = NGCA,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  label = "Non-Government Controled Area",
                  fillColor= "black",
                  fillOpacity = 0.5,
                  weight= 1,
                  opacity = 1) %>%
      
      addPolygons(data= Rshp,    # NUMBER OF IDPS
                  color = "grey",
                  weight = .3,
                  label= Rshp$adm2NameLa,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = ~palnumL(Rshp[[IDPcol]]),
                  fillOpacity = .7,
                  layerId = ~Raion_Name_Eng,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  popup = numIDP_Popup,
                  group= "Number of IDPs") %>%
      addPolylines(data = oblasts,
                   weight= 2,
                   color = "black",
                   fill=FALSE,
                   fillOpacity = 0,
                   opacity = 0.6 ) %>%
      addLegend("topleft", pal = palnumL, values = Rshp[[IDPcol]],
                title = "Number of IDPs",
                opacity = 0.7) %>%
      addLegend("topleft", colors= "red", labels="Line of Contact",
                data=  LoC,
                opacity = 0.7) %>%
      addPolylines(data = LoC,    #LINE OF CONTACT
                   color = "red",
                   opacity = 0.7) 
    
  }) #NUMBER of IDPs MAP
  output$Cmap <- renderLeaflet({
    ChngIDP<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = Lymanskyi,     #NO VALUES RAYON
                  color = "grey",
                  fillOpacity = 0.6,
                  label = "No Data",
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  popup = paste0('<h7 style="color:black;">',  "Rayon:  ", "<b>", Lymanskyi$adm2NameLa, "</b>", '</h7>', "<br>",
                                 " Number of IDPs: N/A"),
                  opacity = 0.1) %>%
      addLabelOnlyMarkers(centroids, lat=centroids$lat, lng=centroids$lon, label=as.character(centroids$adm1NameLa),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 0.6,
                            style = list(
                              "color"= "black",
                              "font-size" = "15px",
                              "font-family"= "Helvetica",
                              "font-weight"= 600)
                          )) %>%
      addLabelOnlyMarkers(UKRl, lat=UKRl$lat, lng=UKRl$lon, label=as.character(UKRl$name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 1,
                            style = list(
                              "color"= "black",
                              "font-size" = "24px",
                              "font-family"= "Helvetica",
                              "font-weight"= 800,
                              "letter-spacing"= "3px")
                          )) %>%
      
      addPolygons(data = NGCA,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  label = "Non-Government Controled Area",
                  fillColor= "black",
                  fillOpacity = 0.5,
                  weight= 1,
                  opacity = 1) %>%
      
      addPolygons(data= Rshp,     # CHANGE IN IDPS
                  color = "grey",
                  weight = .3,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  label = Rshp$adm2NameLa,
                  fill = TRUE,
                  fillColor = ~palchngL(Rshp$chng),
                  fillOpacity = 0.5,
                  layerId = ~Raion_Name_Eng,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  popup = chngIDP_Popup,
                  group= "Change in IDPs") %>%
      addPolylines(data = oblasts,
                   weight= 2,
                   color = "black",
                   fill=FALSE,
                   fillOpacity = 0,
                   opacity = 0.6 ) %>%
      addLegend("topleft", pal = palchngL, values = Rshp$chng,
                title = "Change in Number of IDPs",
                opacity = 0.5) %>%
      addLegend("topleft", colors= "red", labels="Line of Contact",
                data=  LoC,
                opacity = 0.7) %>%
      
      addPolylines(data = LoC,    #LINE OF CONTACT
                   color = "red",
                   opacity = 0.7)
  }) # CHANGE in IDPs MAP
  output$Pmap <- renderLeaflet({
    PropIDP<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = Lymanskyi,     #NO VALUES RAYON
                  color = "grey",
                  fillOpacity = 0.6,
                  label = "No Data",
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  popup = paste0('<h7 style="color:black;">',  "Rayon:  ", "<b>", Lymanskyi$adm2NameLa, "</b>", '</h7>', "<br>",
                                 " Number of IDPs: N/A"),
                  opacity = 0.1) %>%
      addLabelOnlyMarkers(centroids, lat=centroids$lat, lng=centroids$lon, label=as.character(centroids$adm1NameLa),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 0.6,
                            style = list(
                              "color"= "black",
                              "font-size" = "15px",
                              "font-family"= "Helvetica",
                              "font-weight"= 600)
                          )) %>%
      addLabelOnlyMarkers(UKRl, lat=UKRl$lat, lng=UKRl$lon, label=as.character(UKRl$name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 1,
                            style = list(
                              "color"= "black",
                              "font-size" = "24px",
                              "font-family"= "Helvetica",
                              "font-weight"= 800,
                              "letter-spacing"= "3px")
                          )) %>%
      
      addPolygons(data = NGCA,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  label = "Non-Government Controled Area",
                  fillColor= "black",
                  fillOpacity = 0.5,
                  weight= 1,
                  opacity = 1) %>%
      
      addPolygons(data= Rshp,     # PROPORTION OF IDPS
                  color = "grey",
                  weight = .3,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  label = Rshp$adm2NameLa,
                  fill = TRUE,
                  fillColor = ~palpropL(Rshp$prop),
                  fillOpacity = .6,
                  layerId = ~Raion_Name_Eng,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  popup = propIDP_Popup,
                  group= "Proportion of IDPs") %>%
      addPolylines(data = oblasts,
                   weight= 2,
                   color = "black",
                   fill=FALSE,
                   fillOpacity = 0,
                   opacity = 0.6 ) %>%
      addLegend("topleft", pal = palpropL, values = Rshp$prop,
                title = "IDPs per 1000 Hosting Population",
                opacity = 0.6) %>%
      addLegend("topleft", colors= "red", labels="Line of Contact",
                data=  LoC,
                opacity = 0.7) %>%
      
      addPolylines(data = LoC,    #LINE OF CONTACT
                   color = "red",
                   opacity = 0.7)
  }) # PROPORTION OF IDPs MAP
  
  clicked_state<- eventReactive(input$map_shape_click,{
    return(input$map_shape_click$id)
  })
  
  state_data <- reactive({
    n<-match(clicked_state, IDPtable[,1])
    state_data <- stack(IDPtable[n,], select= -Raion_Name_Eng)
    state_data$ind<- as.Date(state_data$ind)
    state_data
  })
  
  
  
  output$text1 <- renderUI({
    HTML(paste("Contact:  kyiv.gis@reach-initiative.org",
               "IDP Data:  MoSP, UNHCR",
               "Population:  State Statistics Service of Ukraine",
               "Coordinate System:  WGS 1984",
               "Administrative boundaries, contact line:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;  
         font-family: Helvetica} </style>')
})
  
  output$text2 <- renderUI({
    HTML(paste("<i>Note: Data, designations and boundaries contained on 
               this map are not warranted to error-free and do not
               imply acceptance by the REACH partners, associated
               donors mentioned on this map</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;  
         font-family: Helvetica} </style>')
  })
  
  output$text3 <- renderUI({
    HTML(paste("Contact:  kyiv.gis@reach-initiative.org",
               "IDP Data:  MoSP, UNHCR",
               "Population:  State Statistics Service of Ukraine",
               "Coordinate System:  WGS 1984",
               "Administrative boundaries, contact line:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;  
         font-family: Helvetica} </style>')
  })
  output$text4 <- renderUI({
    HTML(paste("<i>Note: Data, designations and boundaries contained on 
               this map are not warranted to error-free and do not
               imply acceptance by the REACH partners, associated
               donors mentioned on this map</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;  
         font-family: Helvetica} </style>')
  })
  
  output$text5 <- renderUI({
    HTML(paste("Contact:  kyiv.gis@reach-initiative.org",
               "IDP Data:  MoSP, UNHCR",
               "Population:  State Statistics Service of Ukraine",
               "Coordinate System:  WGS 1984",
               "Administrative boundaries, contact line:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;  
         font-family: Helvetica} </style>')
})
  output$text6 <- renderUI({
    HTML(paste("<i>Note: Data, designations and boundaries contained on 
               this map are not warranted to error-free and do not
               imply acceptance by the REACH partners, associated
               donors mentioned on this map</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;  
         font-family: Helvetica} </style>')
  })
  
  
  output$hcontainer <- renderHighchart({
    event <- (input$map_shape_click) #Critical Line!!!
    
    validate(need(event$id != "",
                  "Please click on a Raion to display its IDP history."))
    title <- paste(as.character(event, "IDPs", sep = " "))
    
    state_data <-
      n<-match(event$id, IDPtable[,1])
    state_data <- stack(IDPtable[n,], select= -Raion_Name_Eng) #rename to df if necessary
    state_data$ind<- as.Date(state_data$ind)
    state_data
    
    highchart()%>%
      hc_add_series_times_values(dates=state_data$ind,
                                 values= state_data$values,
                                 name = "Number of IDPs") %>%
      hc_yAxis(title=list(text="Number of IDPs"), opposite = FALSE) %>%
      hc_title(text=as.character(event$id))%>%
      hc_add_theme(hc_theme_gridlight())%>%
      hc_plotOptions(line = list(
        lineWidth=2,
        dataLabels = list(enabled = FALSE)))
    
  })
  
  }



#FRoM ONLINE https://github.com/ua-snap/shiny-apps/blob/master/cc4liteFinal/server.R


