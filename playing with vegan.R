library(vegan)


# [4] "Gambusia.holbrooki"        "Arenigobius.bifrenatus"    "Mugil.cephalus"             
# [7] "Acanthopagrus.butcheri"    "Limnodynastes.dorsalis"    "Pseudophryne.guentheri"     
# [10] "Anas.platyrhynchos"        "Spilopelia.chinensis"      "Porphyrio.porphyrio"        
# [13] "Porzana.tabuensis"  "Egretta.novaehollandiae"  "Trichoglossus.rubritorquis" 
# [16] "Bos.taurus"                "Ovis.sp."                "Mus.musculus"               
# [19] "Rattus.sp."                "Blackfordia.polytentaculata" "Obelia.bidentata"           
# [22] "Phyllorhiza.punctata"   "Aurelia.sp."           "Canis.lupus.familiaris"     
altnames <- c("Gambusia","Goby","Mullet","Bream","BnjoFrog","GToadlet","MlrdDuck",
              "SpotDove","SwampHen","Crake","WFHeron","Lorikeet","Cattle","Sheep",
              "Mouse","Rat","HydroidB","HydroidO","BwnJelly","MoonJely","Dog")
allSpecies <- eDNA2022[,4:24]
colnames(allSpecies) <- altnames

# detrended correspondence analysis - all species
AF_dca_all <- decorana(eDNA2022[,4:24])
AF_dca_all
plot(AF_dca_all)

# non-metric multidimensional scaling
AF_nmds_all <- metaMDS(allSpecies, distance = "jaccard")
AF_nmds_all

# nmds again with Jaccard
AF_nmds_all_B <- metaMDS(allSpecies) # uses Bray
AF_nmds_all_B

# just the aquatic species:
# Gambusia.holbrooki, Arenigobius.bifrenatus, , Acanthopagrus.butcheri, 
# Limnodynastes.dorsalis, Pseudophryne.guentheri, Blackfordia.polytentaculata, 
# Obelia.bidentata, Phyllorhiza.punctata, Aurelia.sp.

fish <- eDNA2022[,c(4:9,20:23)]
colnames(fish) <- c("MosqFish","Goby","Mullet","BlkBream","BnjoFrog","GnthToad",
                    "Hydroid1","Hydroid2","BrownJel","MoonJell")

# for interest look at the dissimilarity matrix
AF_diss_aquat <- vegdist(fish, 
                           distance = "bray")
options(max.print=2000)
print(AF_diss_aquat, digits = 2)

AF_nmds_aquat <- metaMDS(fish, trymax = 500, 
                           distance = "bray")
AF_nmds_aquat
plot(AF_nmds_aquat, type="p",display = "sites", cex = 1.2)
text(AF_nmds_aquat, display = "species", col = "dodgerblue", cex = 0.8)

# alternative to vegan plots
palette(c("black","grey80","grey92",
          rainbow(4,s=0.5,end=0.25),rainbow(4,s=0.5,start=0.45,end=0.8)))
plot(AF_nmds_aquat$points, pch=c(rep(21:25, 2),21)[eDNA2022$Site], xlim = c(-1.2,1.8), 
     bg=seq(1,11)[eDNA2022$Site], 
     cex = 1.4, main = "Just aquatic species")
text(jitter(AF_nmds_aquat$points[,1], factor = 500),
     jitter(AF_nmds_aquat$points[,2], factor = 50), labels=eDNA2022$SiteID, cex = 1., 
     col=c("black","grey40","grey70",
           rainbow(4,v=0.75,end=0.25),rainbow(4,v=0.75,start=0.45,end=0.8))[eDNA2022$Site])
text(AF_nmds_aquat, display = "species", col = "magenta", font=3, cex = 1.1)

# this plot mimics the nmmds plot in Deirdre's handout
# note that the sign of dimension MDS2 is reversed
plot(AF_nmds_all$points, pch=c(rep(21:25, 2),21)[eDNA2022$Site], 
     xlim = c(-1.6,2.2), ylim = c(1.2,-1.2), 
     bg=seq(1,11)[eDNA2022$Site], 
     cex = 1.4, main = "All species")
text(jitter(AF_nmds_all$points[,1], factor = 500),
     jitter(AF_nmds_all$points[,2], factor = 50), labels=eDNA2022$SiteID, cex = 1., 
     col=c("black","grey40","grey70",
           rainbow(4,v=0.75,end=0.25),rainbow(4,v=0.75,start=0.45,end=0.8))[eDNA2022$Site])
text(AF_nmds_all, display = "species", col = "#80008080", font=3, cex = 1.1)
