


# UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI


ui<- navbarPage(title=strong(HTML("<span style='font-size:30px'>UKRAINE: Internally Displaced Persons</span>")), # id="nav",
                windowTitle = "REACH: Ukraine IDPs",
                
                tabPanel(strong("Number of IDPs"),
                         icon= icon("map-marker"),
                         div(class="outer",
                             
                             tags$head(
                               # Include our custom CSS
                               includeCSS("styles.css")
                             ),
                             
                             # If not using custom CSS, set height of leafletOutput to a number instead of percent
                             leafletOutput("map", width="100%", height="100%"),
                             
                             
                             # Shiny versions prior to 0.11 should use class = "modal" instead.
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = 1,
                                           width = 500, height = 715,
                                           
                                           h4("Number of Internally Displaced Persons (IDPs)"),
                                           h5( paste("As of: ", currentD)),
                                           h5("This data represents the number of IDPs registered by the Ministry 
                                              of Social Policy (MoSP). Note that this includes people who are not 
                                              permanently residing in GCAs but need to be included to have access 
                                              to social benefits, while excluding others who prefer not to register 
                                              for a variety of reasons. Assessments conducted by REACH have found 
                                              that IDP households have moved in geographically scattered areas for 
                                              reasons primarily revolving around safety and existing social networks."),
                                           
                                           h6("For more information please read: ",a("Inter-agency Vulnerability Assessment, November 2016", target="_blank",    href="http://www.reachresourcecentre.info/system/files/resource-documents/ukr_report_inter_agency_vulnerability_assessment_nov16_1.pdf")),
                                           
                                           
                                           h3("IDPs Over Time"),
                                           highchartOutput("hcontainer", height= 220, width = 450),
                                           h6(htmlOutput("text1")),
                                           h6(htmlOutput("text2")),
                                           
                                           column(width=12, align="center", div(id="cite2", "Funded by: "), img(src='EUFlag_CP-HA_Color_EN_1.png', width= "60px"),
                                                  img(src='Vertical_RGB_294.png', width= "88px"))
                                           
                                           
                                           ),
                             
                             
                             
                             
                             tags$div(id="cite",
                                      a(img(src='REACH logo maps only.png', width= "260px"), target="_blank", href="http://www.reach-initiative.org/where-we-work/ongoing-field-presence/ukraine"))
                             )
                         ),
                
                tabPanel(strong("Change in Number of IDPs"),
                         icon= icon("map-marker"),
                         div(class="outer",
                             
                             tags$head(
                               # Include our custom CSS
                               includeCSS("styles.css")
                             ),
                             
                             # If not using custom CSS, set height of leafletOutput to a number instead of percent
                             leafletOutput("Cmap", width="100%", height="100%"),
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 300, height = "auto",
                                           
                                           h4("Change in Number of Internally Displaced People"),
                                           h5(paste("From ", priorD, "to ", currentD)),
                                           
                                           
                                           h5("Most IDP households were displaced for the first time around 
the beginning of the conflict, between May and November 2014.
REACH assessments found that rising cost of living, access to income generating activities and proximity to existing social support networks as factors that might encourage displaced households to relocate. "),
                                           h6("For more information please read: ",a("Inter-agency Vulnerability Assessment, November 2016", target="_blank",    href="http://www.reachresourcecentre.info/system/files/resource-documents/ukr_report_inter_agency_vulnerability_assessment_nov16_1.pdf")),
                                           
                                           
                                           h6(htmlOutput("text3")),
                                           h6(htmlOutput("text4")),
                                           
                                           column(width=12, align="center", div(id="cite2", "Funded by: "), img(src='EUFlag_CP-HA_Color_EN_1.png', width= "60px"),
                                                  img(src='Vertical_RGB_294.png', width= "88px"))
                                           
                                           
                                           ),
                             
                             tags$div(id="cite",
                                      a(img(src='REACH logo maps only.png', width= "260px"), target="_blank", href="http://www.reach-initiative.org/where-we-work/ongoing-field-presence/ukraine")))),
                
                tabPanel(strong("Proportion of IDPs"),
                         
                         icon= icon("map-marker"),
                         div(class="outer",
                             
                             tags$head(
                               # Include our custom CSS
                               includeCSS("styles.css")
                             ),
                             
                             # If not using custom CSS, set height of leafletOutput to a number instead of percent
                             leafletOutput("Pmap", width="100%", height="100%"),
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 300, height = "auto",
                                           
                                           h4("Proportion of Internally Displaced People"),
                                           h5(paste("As of: ", currentD)),
                                           
                                           
                                           h5(" This map shows the proportion of registered IDPs per 
                                              1000 hosting population. While discriminatory or hostile 
                                              behavior towards IDPs from host communities is uncommon, 
                                              perceived increase in competition for jobs and delivery of 
                                              humanitarian assistance has been reported. The influx of 
                                              IDPs has added pressure on already weak markets, resulting 
                                              in growing increases in the price of basic commodities and 
                                              non-food items. "),
                                           h6("For more information please read: ",a("Inter-agency Vulnerability Assessment, November 2016", target="_blank",    href="http://www.reachresourcecentre.info/system/files/resource-documents/ukr_report_inter_agency_vulnerability_assessment_nov16_1.pdf")),
                                           
                                           
                                           h6(htmlOutput("text5")),
                                           h6(htmlOutput("text6")),
                                           column(width=12, align="center", div(id="cite2", "Funded by: "), img(src='EUFlag_CP-HA_Color_EN_1.png', width= "60px"),
                                                  img(src='Vertical_RGB_294.png', width= "88px"))
                                           
                                           
                             ),
                             
                             tags$div(id="cite",
                                      a(img(src='REACH logo maps only.png', width= "260px"), target="_blank", href="http://www.reach-initiative.org/where-we-work/ongoing-field-presence/ukraine")))),
                
                
                conditionalPanel("false", icon("crosshairs"))
                )

