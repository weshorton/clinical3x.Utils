setwd("/Users/hortowe/my_tool_repos/clinical3x.Utils")
library(data.table)
library(wrh.rUtils)
library(RColorBrewer)
devtools::load_all("~/my_tool_repos/colorblindr")

###
### Make colors and other vars for project
###

### Patients
ptColors_v <- c("#7BAFDE", "#AE76A3", "#CAE0AB", "#F6C141", "#1965B0", "#E8601C")
pts_v <- c("001-001", "001-002", "001-003", "001-004", "001-005")
ptColors_v <- ptColors_v[1:length(pts_v)]
names(ptColors_v) <- pts_v

usethis::use_data(ptColors_v, overwrite = T)

### Make CB colors
# recistColors_v <- c("PR" = brewer.pal(9, "Set1")[3], # green
#                     "SD" = brewer.pal(9, "Set1")[5], # orange
#                     "PD" = brewer.pal(9, "Set1")[1]) # red

recistColors_v <- c("PR" = "#3B6432",
                    "SD" = brewer.pal(9, "Set1")[3], # lighter green
                    "PD" = brewer.pal(9, "Set1")[1]) # red

usethis::use_data(recistColors_v, overwrite = T)

### Clinical benefit colors
cbColors_v <- c("CB" = brewer.pal(9, "Set1")[3], # green
                "PD" = brewer.pal(9, "Set1")[1]) # red

usethis::use_data(cbColors_v, overwrite = T)

### Time colors
timePoints_v <- c("C1D-14", "C1D-8", "C1D1", "C1D8", "C1D15", "C1D22", "C2D1", "C2D8", "C2D15", 
                  "C2D22", "C3D1", "C3D8", "C3D15", "C4D1", "C5D1", "C6D1", "C7D1")
timePoints_v <- sortTimePoints(timePoints_v)

usethis::use_data(timePoints_v, overwrite = T)

c1_v <- brewer.pal(9, "YlOrRd")[(9-length(grep("C1", timePoints_v))):8]
# c1_v <- brewer.pal(9, "YlOrRd")[(10-length(grep("C1", timePoints_v))):9] # this one makes C1 a tad darker
c2_v <- brewer.pal(9, "YlGn")[(8-length(grep("C2", timePoints_v))):7]
c3_v <- brewer.pal(9, "BuPu")[(8-length(grep("C3", timePoints_v))):7]
cOther_v <- brewer.pal(9, "YlGnBu")[(8-length(grep("C[4-9]", timePoints_v))):7]
timeColors_v <- c(c1_v, c2_v, c3_v, cOther_v)
names(timeColors_v) <- timePoints_v

usethis::use_data(timeColors_v, overwrite = T)

### Individual time colors
area7_v <- cols4all::c4a("area7")
friendly11_v <- cols4all::c4a("friendly11")
line7_v <- cols4all::c4a("line7")
tableauCB_v <- cols4all::c4a("tableau.color_blind")
tolMuted_v <- cols4all::c4a("tol.muted")

c1Colors_v <- area7_v[1:length(grep("C1", timePoints_v))]
names(c1Colors_v) <- grep("C1", timePoints_v, value = T)

c2Colors_v <- line7_v[1:length(grep("C2", timePoints_v))]
names(c2Colors_v) <- grep("C2", timePoints_v, value = T)

c3Colors_v <- tolMuted_v[1:length(grep("C3", timePoints_v))]
names(c3Colors_v) <- grep("C3", timePoints_v, value = T)

cOtherColors_v <- tableauCB_v[1:length(grep("C[4-9]", timePoints_v))]
names(cOtherColors_v) <- grep("C[4-9]", timePoints_v, value = T)

d1Colors_v <- friendly11_v[1:length(grep("D1$", timePoints_v))]
names(d1Colors_v) <- grep("D1$", timePoints_v, value = T)

indTimeColors_lsv <- list("C1" = c1Colors_v,
                          "C2" = c2Colors_v,
                          "C3" = c3Colors_v,
                          "COther" = cOtherColors_v,
                          "D1" = d1Colors_v)

usethis::use_data(indTimeColors_lsv, overwrite = T)

### Get cycles (not used)
cycles_v <- unique(gsub("D.*", "", timePoints_v))
cycles_lsv <- list("C1" = grep("C1", timePoints_v, value = T), 
                   "C2" = grep("C2", timePoints_v, value = T), 
                   "C3" = grep("C3", timePoints_v, value = T),
                   "CHi" = grep("C[4-9]", timePoints_v, value = T))

usethis::use_data(cycles_lsv, overwrite = T)
usethis::use_data(cycles_v, overwrite = T)

dirColors_v <- c("#47AD48", "#D7222B", "#0A0B09")
names(dirColors_v) <- c("UP", "DN", "NC")

usethis::use_data(dirColors_v, overwrite = T)

### Some pops are too long for excel sheets
sheetRename_dt <- data.table("Short" = c("CD14+ CD16+ intermediary Monocy", "Lin- putative Innate Lymphoid C"),
                             "Long" = c("CD14+ CD16+ intermediary Monocytes", "Lin- putative Innate Lymphoid Cells"))

usethis::use_data(sheetRename_dt, overwrite = T)

### Load colors all colors
allColors_lsdt <- suppressMessages(readAllExcel("./data-raw/colors.xlsx"))
colorsWithClass_dt <- allColors_lsdt$readable
colors_dt <- allColors_lsdt$uniqReadable

### Make color vectors (V1)
colors_v <- colors_dt$Hex; names(colors_v) <- colors_dt$Name
shortNameColors_v <- colors_dt$Hex; names(shortNameColors_v) <- colors_dt$displayName

usethis::use_data(colors_v, overwrite = T)
usethis::use_data(shortNameColors_v, overwrite = T)

### Make color vectors (V2)
colors_v2_v <- colors_dt$Hex2; names(colors_v2_v) <- colors_dt$Name
shortNameColors_v2_v <- colors_dt$Hex2; names(shortNameColors_v2_v) <- colors_dt$displayName

usethis::use_data(colors_v2_v, overwrite = T)
usethis::use_data(shortNameColors_v2_v, overwrite = T)

### Marker groups
cd48Markers_v <- c("CD38", "CD39", "CD69", "KLRG1", "Ki67", "GZMB", "IFN g", "CD40", "CD86")
cd48Markers_lsv <- list("A" = c("CD38", "CD39", "CD69", "KLRG1"), 
                        "B" = c("Ki67", "GZMB", "IFN g"),
                        "C" = c("CD40", "CD86"))

usethis::use_data(cd48Markers_v, overwrite = T)
usethis::use_data(cd48Markers_lsv, overwrite = T)

radarMarkers_v <- c("CD279", "CD278", "CTLA-4", "LAG-3", "TIGIT", "TOX", "TIM3")

usethis::use_data(radarMarkers_v, overwrite = T)

### Treatment schema tables
### Compact table with diagonals
compactSchema_dt <- as.data.table(t(data.table("A" = paste0("Cx", c("D-8", "D1", "D8", "D15", "D21")),
                                               "B" = c("aCSF1R", "PTX+aPD1", "PTX+aCSF1R", "PTX", "aCSF1R"))))

usethis::use_data(compactSchema_dt, overwrite = T)

### Table with one row per treatment
fullSchema_dt <- as.data.table(t(data.table("A" = c("Tx", paste0("Cx", c("D-8", "D1", "D8", "D15", "D21"))),
                                            "B" = c("Axatilimab\n(aCSF1R)", "aCSF1R", "", "aCSF1R", "", "aCSF1R"),
                                            "C" = c("Retifanlimab\n(aPD1)", "", "aPD1", "", "", ""),
                                            "D" = c("Paclitaxel\n(PTX)", "", "PTX", "PTX", "PTX", ""))))

usethis::use_data(fullSchema_dt, overwrite = T)

### Color mapping table
# schemaColorMap_dt <- data.table("Tx" = c("aCSF1R", "aPD1", "PTX", "PTX+aPD1", "PTX+aCSF1R"),
#                                 "Color" = c("#DA7842", "#489CD0", "#000000", "#000000+#489CD0", "#000000+#DA7842"))

schemaColorMap_dt <- data.table("Tx" = c("none", "aCSF1R", "aPD1", "PTX", "PTX+aPD1", "PTX+aCSF1R"),
                                "Color" = c("white", "#76D6FF", "#FF3092", "#7030A0", "#7030A0+#FF3092", "#7030A0+#76D6FF"))

schemaColorMap_v <- c("none" = "white", "aCSF1R" = "#76D6FF", "aPD1" = "#FF3092", "PTX" = "#7030A0", 
                      "PTX+aPD1" = c("#7030A0+#FF3092"), "PTX+aCSF1R" = c("#7030A0+#76D6FF"))

treatDateMap_dt <- data.table("Time points" = timePoints_v,
                              "Cycle" = gsub("D.*$", "", timePoints_v),
                              "Day" = paste0("D", gsub("^.*D", "", timePoints_v)),
                              "Tx" = c("none", "aCSF1R", rep(c("PTX+aPD1", "PTX+aCSF1R", "PTX", "aCSF1R"), 2),
                                       "PTX+aPD1", "PTX+aCSF1R", "PTX", rep("PTX+aPD1", 4)))
treatDateMap_dt <- merge(treatDateMap_dt, schemaColorMap_dt, by = "Tx", sort = F)

treatAnnotColors_lsv <- list("Axatilimab" = c("none" = "white", "Axatilimab" = "#76D6FF"),
                             "Retifanlimab" = c("none"= "white", "Retifanlimab" = "#FF3092"),
                             "Paclitaxel" = c("none" = "white", "Paclitaxel" = "#7030A0"))


usethis::use_data(treatAnnotColors_lsv, overwrite = T)
usethis::use_data(schemaColorMap_dt, overwrite = T)
usethis::use_data(schemaColorMap_v, overwrite = T)
usethis::use_data(treatDateMap_dt, overwrite = T)
