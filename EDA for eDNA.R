par(mfrow = c(2,2), mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0), 
    tcl = 0.3, ljoin = "mitre", lend = "square", font.lab = 2)
palette(c("black","darkcyan","blue3","springgreen4","bisque4","white"))
plot(as.numeric(tapply(eDNA$all_aquat, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     ylim = c(0,4), log = "x",
     pch = 19, cex = 2, type = "n",
     xlab = "Mn (mg/L)",
     ylab = "Number of aquatic species (mean)")
grid()
points(as.numeric(tapply(eDNA$all_aquat, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     pch = 19, col = 2, cex = 2)
text(as.numeric(tapply(eDNA$all_aquat, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     labels = levels(eDNA$Zone), pos = c(3,1,4,2,4,2), cex = 0.8)
mtext("All aquatic species", 3, -1.5, adj=0.05, font=2, col=2)
#
plot(as.numeric(tapply(eDNA$fish, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     ylim = c(0,4), log = "x",
     pch = 19, cex = 2, type = "n",
     xlab = "Mn (mg/L)",
     ylab = "Number of fish species (mean)")
grid()
points(as.numeric(tapply(eDNA$fish, eDNA$Sample, mean)) ~ 
         as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
       pch = 19, col = 3, cex = 2)
text(as.numeric(tapply(eDNA$fish, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     labels = levels(eDNA$Zone), pos = c(3,1,4,2,4,2), cex = 0.8)
mtext("Fish", 3, -1.5, adj = 0.95, font = 2, col = 3)
#
plot(as.numeric(tapply(eDNA$frogs, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     ylim = c(0,4), log = "x",
     pch = 19, cex = 2, type = "n",
     xlab = "Mn (mg/L)",
     ylab = "Number of frog species (mean)")
grid()
points(as.numeric(tapply(eDNA$frogs, eDNA$Sample, mean)) ~ 
         as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
       pch = 19, col = 4, cex = 2)
text(as.numeric(tapply(eDNA$frogs, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     labels = levels(eDNA$Zone), pos = c(3,1,4,2,4,2), cex = 0.8)
mtext("Frogs", 3, -1.5, adj = 0.95, font = 2, col = 4)
#
plot(as.numeric(tapply(eDNA$Chelodina_sp, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     ylim = c(0,4), log = "x",
     pch = 19, cex = 2, type = "n",
     xlab = "Mn (mg/L)",
     ylab = expression(bold(paste("Number of ", 
                                  bolditalic("Chelodina"), 
                                  " species (mean)"))))
grid()
points(as.numeric(tapply(eDNA$Chelodina_sp, eDNA$Sample, mean)) ~ 
         as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
       pch = 19, col = 5, cex = 2)
text(as.numeric(tapply(eDNA$Chelodina_sp, eDNA$Sample, mean)) ~ 
       as.numeric(tapply(eDNA$Mn, eDNA$Sample, mean, na.rm=T)),
     labels = levels(eDNA$Zone), pos = 3, offset = 3, 
     srt = 90, cex = 0.8)
mtext(expression(bold(paste(bolditalic("Chelodina"), 
                            " species (turtles)"))),
      3, -1.5, adj = 0.95, font = 2, col = 5)
