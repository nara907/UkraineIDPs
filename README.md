# UkraineIDPs
R Scripts used to create the shiny REACH interactive map/dashboard of Ukraine IDPs
Interactive IDP Maps



Concept:

From July 2017 until July 2018, the Ukraine GIS team has been producing monthly static IDP maps using data made available by the Ministry of Social Policy (MoSP) (https://www.msp.gov.ua/en/). Three static maps are published to the REACH resource centre using this data; number of IDPs, proportion of IDPs to host population and change in IDPs per Rayon (county administrative level).

To streamline the spatial visualization of monthly updates in IDP data we have automated the production of the three maps using R and google sheets though a Shiny app. The application shows the same information as the static maps, but is interactive which allows the user to click polygons to see actual data values per administrative unit, along with a line graph which highlights the change in IDPs over time. 

This interface will streamline this task and improve the usability of these map products. 



Data:

The data is provided monthly by the Ministry of Social Policy. This data represents the number of people registered as Internally Displaced Persons. It is important to note that the numbers to not reflect the actual number of IDPs. The values may include those registered but not residing in the Government Controlled Area (GCA), and exclude those who are displaced but not registered.



Methods:

The interactive map is hosted online as a Shiny app. This was created by integrating data hosted on Google Sheets and Github to automate updates to the the IDP data. The interface was created using R programming in Rstudio. 





The R script pulls in data from Github and Google Drive in order to create the map. The shapefiles of the administrative boundaries, the contact line and the logos are stored as a package on Github. The package can then be installed and used just as packages on CRAN. The IDP data is stored in a Google Sheet. This allows for a new column to be added the spreadsheet everytime there is new monthly data. The Rcode pulls the information displayed in the map from the google sheet using the Google API. This remote storage of data and automation allows the application to not require data from a local source and easy map updating by non-R-users.


Packages used in R: 
shiny
leaflet
highcharter
devtools
rgdal
ggplot2
rmapshaper
IDPukr (Github Package)
googlesheets
dplyr
geosphere


