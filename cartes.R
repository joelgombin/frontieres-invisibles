library(cartography)
library(rgeos)
library(maptools)
library(leaflet)
load("./data/IRISPACA.Rdata")

load("./data/POP_2012_IRIS.Rdata")

load("./data/RFDUiris20012011.Rdata")


IRISPACAconsolide <- unionSpatialPolygons(IRISPACA, IDs = IRISPACA@data$DComIris)
IRISPACA <- SpatialPolygonsDataFrame(IRISPACAconsolide, data.frame(ID = row.names(IRISPACAconsolide), row.names = row.names(IRISPACAconsolide), stringsAsFactors = FALSE))

IRISMarseille <- IRISPACA[substr(IRISPACA@data$ID, 1, 5) %in% as.character(13201:13216),]

iris_borders <- getBorders(IRISMarseille, "ID")
IRISMarseille <- spTransform(IRISMarseille, CRSobj = CRS("+init=epsg:4326"))
iris_borders <- spTransform(iris_borders, CRSobj = CRS("+init=epsg:4326"))

IRISMarseille <- IRISMarseille %>% tmap::append_data(POP_2012_IRIS, key.shp = "ID", key.data = "IRIS")
```

```{r leaflet1, cache=FALSE}
iris_borders@data$ratio <- abs(POP_2012_IRIS[match(iris_borders@data$id1, POP_2012_IRIS$IRIS), "P12_POP_ETR_pc"] - POP_2012_IRIS[match(iris_borders@data$id2, POP_2012_IRIS$IRIS), "P12_POP_ETR_pc"])
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)

library(leaflet)

pal <- colorQuantile("Greens", IRISMarseille@data$P12_POP_ETR_pc, n = 10)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(P12_POP_ETR_pc), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~P12_POP_ETR_pc, title = "Étrangers") %>% 
  addPolylines(data = iris_borders, weight = ~sqrt(ratio), stroke = TRUE, color = "red") 

iris_borders@data$ratio <- abs(POP_2012_IRIS[match(iris_borders@data$id1, POP_2012_IRIS$IRIS), "C12_POP15P_CS3_pc"] - POP_2012_IRIS[match(iris_borders@data$id2, POP_2012_IRIS$IRIS), "C12_POP15P_CS3_pc"])
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)


pal <- colorQuantile("Greens", IRISMarseille@data$C12_POP15P_CS3_pc, n = 10)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(C12_POP15P_CS3_pc), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~C12_POP15P_CS3_pc, title = "Cadres et prof. intell. sup.") %>% 
  addPolylines(data = iris_borders, weight = ~sqrt(ratio), stroke = TRUE, color = "red") %>% 
  htmlwidgets::saveWidget(file = "carte1.html")

iris_borders@data$ratio <- abs(POP_2012_IRIS[match(iris_borders@data$id1, POP_2012_IRIS$IRIS), "C12_POP15P_CS3_pc"] - POP_2012_IRIS[match(iris_borders@data$id2, POP_2012_IRIS$IRIS), "C12_POP15P_CS3_pc"])
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)
pal <- colorQuantile("Greens", IRISMarseille@data$C12_POP15P_CS3_pc, n = 10)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(C12_POP15P_CS3_pc), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~C12_POP15P_CS3_pc, title = "Cadres et prof. intell. sup.") %>% 
  addPolylines(data = iris_borders, weight = ~sqrt(ratio), stroke = TRUE, color = "red") %>% 
  htmlwidgets::saveWidget(file = "carte2.html")

iris_borders@data$ratio <- abs(POP_2012_IRIS[match(iris_borders@data$id1, POP_2012_IRIS$IRIS), "C12_POP15P_CS4_pc"] - POP_2012_IRIS[match(iris_borders@data$id2, POP_2012_IRIS$IRIS), "C12_POP15P_CS4_pc"])
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)
pal <- colorQuantile("Greens", IRISMarseille@data$C12_POP15P_CS4_pc, n = 10)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(C12_POP15P_CS4_pc), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~C12_POP15P_CS4_pc, title = "Prof. intermédiaires") %>% 
  addPolylines(data = iris_borders, weight = ~sqrt(ratio), stroke = TRUE, color = "red") %>% 
  htmlwidgets::saveWidget(file = "carte3.html")


iris_borders@data$ratio <- abs(POP_2012_IRIS[match(iris_borders@data$id1, POP_2012_IRIS$IRIS), "C12_POP15P_CS6_pc"] - POP_2012_IRIS[match(iris_borders@data$id2, POP_2012_IRIS$IRIS), "C12_POP15P_CS6_pc"])
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)
pal <- colorQuantile("Greens", IRISMarseille@data$C12_POP15P_CS6_pc, n = 10)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(C12_POP15P_CS6_pc), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~C12_POP15P_CS6_pc, title = "Ouvriers") %>% 
  addPolylines(data = iris_borders, weight = ~sqrt(ratio), stroke = TRUE, color = "red")  %>% 
  htmlwidgets::saveWidget(file = "carte4.html")

iris_borders@data$ratio <- abs(POP_2012_IRIS[match(iris_borders@data$id1, POP_2012_IRIS$IRIS), "C12_POP15P_CS7_pc"] - POP_2012_IRIS[match(iris_borders@data$id2, POP_2012_IRIS$IRIS), "C12_POP15P_CS7_pc"])
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)
pal <- colorQuantile("Greens", IRISMarseille@data$C12_POP15P_CS_pc, n = 10)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(C12_POP15P_CS7_pc), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~C12_POP15P_CS7_pc, title = "Retraités") %>% 
  addPolylines(data = iris_borders, weight = ~sqrt(ratio), stroke = TRUE, color = "red")   %>% 
  htmlwidgets::saveWidget(file = "carte5.html")

iris_borders@data$ratio <- pmax(RFDU2011iris[match(iris_borders@data$id1, RFDU2011iris$CodeIris), "RFUCQ211"] / RFDU2011iris[match(iris_borders@data$id2, RFDU2011iris$CodeIris), "RFUCQ211"], RFDU2011iris[match(iris_borders@data$id2, RFDU2011iris$CodeIris), "RFUCQ211"] / RFDU2011iris[match(iris_borders@data$id1, RFDU2011iris$CodeIris), "RFUCQ211"], na.rm = TRUE)
iris_borders@data[is.na(iris_borders@data$ratio), "ratio"] <- 0
iris_borders@data$ratio <- ifelse(iris_borders@data$ratio > quantile(iris_borders@data$ratio, 0.8, na.rm = TRUE), iris_borders@data$ratio, 0)

IRISMarseille@data$RFUCQ211 <- RFDU2011iris[match(IRISMarseille$ID, RFDU2011iris$CodeIris), "RFUCQ211"]
pal <- colorNumeric("Greens", IRISMarseille@data$RFUCQ211)
leaflet(data = IRISMarseille) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  addPolygons(color = ~pal(RFUCQ211), stroke = FALSE) %>% 
  addLegend(position = "bottomleft", pal = pal, values = ~RFUCQ211, title = "Revenu médian par unité de consommation", labFormat = labelFormat(suffix = "€")) %>% 
  addPolylines(data = iris_borders, weight = ~ratio, stroke = TRUE, color = "red")  %>% 
  htmlwidgets::saveWidget(file = "carte6.html")
