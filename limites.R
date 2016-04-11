library(cartography)
library(rgeos)
library(maptools)
load("./data/IRISPACA.Rdata")

load("./data/POP_2012_IRIS.Rdata")

load("./data/RFDUiris20012011.Rdata")


IRISPACAconsolide <- unionSpatialPolygons(IRISPACA, IDs = IRISPACA@data$DComIris)
IRISPACA <- SpatialPolygonsDataFrame(IRISPACAconsolide, data.frame(ID = row.names(IRISPACAconsolide), row.names = row.names(IRISPACAconsolide), stringsAsFactors = FALSE))

IRISMarseille <- IRISPACA[substr(IRISPACA@data$ID, 1, 5) %in% as.character(13201:13216),]

iris_borders <- getBorders(IRISMarseille, "ID")
IRISMarseille <- spTransform(IRISMarseille, CRSobj = CRS("+init=epsg:4326"))
iris_borders <- spTransform(iris_borders, CRSobj = CRS("+init=epsg:4326"))


POP_2012_IRIS[POP_2012_IRIS$P12_POP_ETR_pc %in% 0, "P12_POP_ETR_pc"] <- 0.001
MarseilleOSM <- getTiles(IRISMarseille, type = "stamenwaterbw", crop = TRUE)


png("discontinuites_etrangers.png", width = 1200, height = 900)

opar <- par(mar = c(0,0,1.2,0))
tilesLayer(MarseilleOSM)
#plot(IRISMarseille, border = NA, col = NA, bg = "#A6CAE0")
#plot(IRISPACA, col = "#E3DEBF", border = NA, add = TRUE)
choroLayer(spdf = IRISMarseille, df = POP_2012_IRIS, var = "P12_POP_ETR_pc", border = "transparent", col = carto.pal("orange.pal", n1 = 6, transparency = TRUE), method = "equal", nclass = 6, add = TRUE, legend.pos = "left", legend.values.rnd = 2, legend.title.txt = "Étrangers (2012)", legend.title.cex = 1.5, legend.values.cex = 1.5)


discLayer(spdf = iris_borders, df = POP_2012_IRIS, var = "P12_POP_ETR_pc", type = "abs", method = "equal", nclass = 10, threshold = 0.2, sizemin = 0.5, sizemax = 6, col = "blue", legend.values.rnd = 1, legend.pos = "n", add = TRUE)
dev.off()

png("discontinuites_cs3.png", width = 1200, height = 900)

opar <- par(mar = c(0,0,1.2,0))
tilesLayer(MarseilleOSM)
#plot(IRISMarseille, border = NA, col = NA, bg = "#A6CAE0")
#plot(IRISPACA, col = "#E3DEBF", border = NA, add = TRUE)
choroLayer(spdf = IRISMarseille, df = POP_2012_IRIS, var = "C12_POP15P_CS3_pc", border = "transparent", col = carto.pal("orange.pal", n1 = 6, transparency = TRUE), method = "equal", nclass = 6, add = TRUE, legend.pos = "left", legend.values.rnd = 2, legend.title.txt = "Cadres et PIS (2012)", legend.title.cex = 1.5, legend.values.cex = 1.5)


discLayer(spdf = iris_borders, df = POP_2012_IRIS, var = "C12_POP15P_CS3_pc", type = "abs", method = "equal", nclass = 10, threshold = 0.2, sizemin = 0.5, sizemax = 6, col = "blue", legend.pos = "n", add = TRUE)
dev.off()

png("discontinuites_cs4.png", width = 1200, height = 900)

opar <- par(mar = c(0,0,1.2,0))
tilesLayer(MarseilleOSM)
#plot(IRISMarseille, border = NA, col = NA, bg = "#A6CAE0")
#plot(IRISPACA, col = "#E3DEBF", border = NA, add = TRUE)
choroLayer(spdf = IRISMarseille, df = POP_2012_IRIS, var = "C12_POP15P_CS4_pc", border = "transparent", col = carto.pal("orange.pal", n1 = 6, transparency = TRUE), method = "equal", nclass = 6, add = TRUE, legend.pos = "left", legend.values.rnd = 2, legend.title.txt = "Professions intermédiaires (2012)", legend.title.cex = 1.5, legend.values.cex = 1.5)


discLayer(spdf = iris_borders, df = POP_2012_IRIS, var = "C12_POP15P_CS4_pc", type = "abs", method = "equal", nclass = 10, threshold = 0.2, sizemin = 0.5, sizemax = 6, col = "blue", legend.pos = "n", add = TRUE)
dev.off()

png("discontinuites_cs6.png", width = 1200, height = 900)

opar <- par(mar = c(0,0,1.2,0))
tilesLayer(MarseilleOSM)
#plot(IRISMarseille, border = NA, col = NA, bg = "#A6CAE0")
#plot(IRISPACA, col = "#E3DEBF", border = NA, add = TRUE)
choroLayer(spdf = IRISMarseille, df = POP_2012_IRIS, var = "C12_POP15P_CS6_pc", border = "transparent", col = carto.pal("orange.pal", n1 = 6, transparency = TRUE), method = "equal", nclass = 6, add = TRUE, legend.pos = "left", legend.values.rnd = 2, legend.title.txt = "Ouvriers (2012)", legend.title.cex = 1.5, legend.values.cex = 1.5)


discLayer(spdf = iris_borders, df = POP_2012_IRIS, var = "C12_POP15P_CS6_pc", type = "abs", method = "equal", nclass = 10, threshold = 0.2, sizemin = 0.5, sizemax = 6, col = "blue", legend.pos = "n", add = TRUE)
dev.off()

png("discontinuites_cs7.png", width = 1200, height = 900)

opar <- par(mar = c(0,0,1.2,0))
tilesLayer(MarseilleOSM)
#plot(IRISMarseille, border = NA, col = NA, bg = "#A6CAE0")
#plot(IRISPACA, col = "#E3DEBF", border = NA, add = TRUE)
choroLayer(spdf = IRISMarseille, df = POP_2012_IRIS, var = "C12_POP15P_CS7_pc", border = "transparent", col = carto.pal("orange.pal", n1 = 6, transparency = TRUE), method = "equal", nclass = 6, add = TRUE, legend.pos = "left", legend.values.rnd = 2, legend.title.txt = "Retraités (2012)", legend.title.cex = 1.5, legend.values.cex = 1.5)


discLayer(spdf = iris_borders, df = POP_2012_IRIS, var = "C12_POP15P_CS7_pc", type = "abs", method = "equal", nclass = 10, threshold = 0.2, sizemin = 0.5, sizemax = 6, col = "blue", legend.pos = "n", add = TRUE)
dev.off()

png("discontinuites_revenus.png", width = 1200, height = 900)

opar <- par(mar = c(0,0,1.2,0))
tilesLayer(MarseilleOSM)
#plot(IRISMarseille, border = NA, col = NA, bg = "#A6CAE0")
#plot(IRISPACA, col = "#E3DEBF", border = NA, add = TRUE)
choroLayer(spdf = IRISMarseille, df = RFDU2011iris, var = "RFUCQ211", border = "transparent", col = carto.pal("orange.pal", n1 = 6, transparency = TRUE), method = "equal", nclass = 6, add = TRUE, legend.pos = "left", legend.values.rnd = 2, legend.title.txt = "Revenu médian (2011)", legend.title.cex = 1.5, legend.values.cex = 1.5)


discLayer(spdf = iris_borders, df = RFDU2011iris, var = "RFUCQ211", type = "abs", method = "equal", nclass = 10, threshold = 0.2, sizemin = 0.5, sizemax = 6, col = "blue", legend.pos = "n", add = TRUE)
dev.off()