# # Code to generate MSP text file:
# 
# x <- MetaDataDolphin[, c("Compound", "SpectrumFilename")]
# names(x) <- c("compound", "filename")
# WriteMspFile(SpecDataDolphin, x, filename = "SpecLibDolphin2014.txt", 
#              comment = "Blubber from two ecotypes of common bottlenose dolphins (Tursiops truncatus) fatally stranded in the Southern California Bight from 1995 to 2010. Shaul, NJ, et al.")

LibraryReport <- function(spectra = SpecDataDolphin,
                          metadata = MetaDataDolphin,
                          pdfFile = "SpecLibDolphin2014.pdf",
                          pdfTitle = "SpecLibDolphin2014 Library",
                          xMin = 40) {
  
  pdf(file = pdfFile, width = 10.5, height = 8, title = pdfTitle, paper = "usr")
  
  
  #---------- Title Page ----------
  
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 1, heights = unit(c(0.1, 0.2, 0.5, 0.2), "npc"))))
  
  pushViewport(viewport(layout.pos.row = 1))
  
  grid.text("SpecLibDolphin2014 Mass Spectral Library", y = 0.5, gp = gpar(cex = 1.25))
  grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0,0), "npc"))
  
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2))
  
  grid.text("Pacific Bottlenose Dolphin Blubber", y = 0.5, gp = gpar(cex = 2))
  
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 3))
  
  grid.text("Nontargeted analysis reveals hundreds of unmonitored bioaccumulative compounds\nin bottlenose dolphins from Southern California.", y = 0.9, gp = gpar(cex = 1.25))
  grid.text("Authors: Nellie J. Shaul, Nathan G. Dodder, Lihini I. Aluwihare, Susan A. Mackintosh,\nKeith A. Maruya, Susan J. Chivers, Kerri Danil, David W. Weller, Eunha Hoh", y = 0.6, gp = gpar(cex = 1.25))
  grid.text("Web Reference: http://OrgMassSpec.github.io", y = 0.3, gp = gpar(cex = 1.25))
  
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 4))
  
  session.info <- sessionInfo()
  
  grid.text(paste("Prepared:", Sys.time()), y = 0.8)
  grid.text(paste("SpecLibDolphin2014 version", session.info$otherPkgs$SpecLibDolphin2014$Version), y = 0.65)
  grid.text(paste("OrgMassSpecR version", session.info$otherPkgs$OrgMassSpecR$Version), y = 0.5)
  grid.text(session.info$R.version$version.string, y = 0.35)
  
  popViewport()

  # Prepare metadata
  
  uniqueCompounds <- as.character(unique(metadata$ReferenceSpectrum))
  
  endings <- substr(uniqueCompounds, (nchar(uniqueCompounds) - 2) + 1, nchar(uniqueCompounds))
  
  uniqueSampleCompounds <- paste(uniqueCompounds, endings, sep = "_")
  
  filteredMetadata <- metadata[metadata$SpectrumFilename %in% uniqueSampleCompounds, ]
  
  filteredMetadata$page <- 1:nrow(filteredMetadata)
  
  # Draw one spectrum on each page  
  
  DrawSpectrum <- function(currentFilename) {
    
    currentSpectrum <- spectra[spectra$filename == as.character(currentFilename), ]
    currentMetadata <- metadata[metadata$SpectrumFilename == currentFilename, ]
    
    message("Making spectrum for ", currentMetadata$Compound)
    
    grid.newpage()
    
    # Write information at top of page
    
    pushViewport(viewport(layout = grid.layout(nrow = 5, 
                                               ncol = 1, 
                                               heights = unit(c(0.05, 0.1, 0.55, 0.25, 0.05), "npc"))))
    
    pushViewport(viewport(layout.pos.row = 1))
    
    grid.text(paste("Name:", currentMetadata$Compound), 
              x = 0, y = 0.5, just = c("left", "center"), gp = gpar(cex = 1.25))
    
    grid.text(paste("Class:", currentMetadata$Category2),
              x = 0.67, y = 0.5, just = c("left", "center"), gp = gpar(cex = 1.25))
    
    grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(0,0), "npc"))
    
    popViewport()
    
    
    #---------- Compound Info ----------
    
    pushViewport(viewport(layout.pos.row = 2))
    
    grid.text(paste("Sample: ", currentMetadata$Sample, ", ", currentMetadata$SampleCode, sep = ""), x = 0, y = 0.75, hjust = 0) 
    grid.text(paste("Ecotype:", currentMetadata$Ecotype), x = 0, y = 0.5, hjust = 0)                
    grid.text(paste("Instrument:", currentMetadata$Instrument), x = 0, y = 0.25, hjust = 0)    
    grid.text(paste("1D RT, 2D RT (s): ", currentMetadata$RetentionTime1D, ", ", currentMetadata$RetentionTime2D, sep = ""), x = 0.33, y = 0.75, hjust = 0)    
    grid.text(paste("Quantitative Ion m/z:", currentMetadata$QuantIon), x = 0.33, y = 0.5, hjust = 0)
    
    if(!is.na(currentMetadata$Formula)) {
      grid.text(paste("Elemental Formula:", currentMetadata$Formula), x = 0.67, y = 0.75, hjust = 0)
    } else {
      grid.text(paste("Elemental Formula:"), x = 0.67, y = 0.75, hjust = 0)
    }
    
    grid.text(paste("Source:", currentMetadata$Category1), x = 0.67, y = 0.5, hjust = 0)      
    
    if(!is.na(currentMetadata$Identification)) {
      grid.text(paste("Identification:", currentMetadata$Identification),x = 0.67, y = 0.25, hjust = 0) 
    } else {
      grid.text(paste("Identification:"),x = 0.67, y = 0.25, hjust = 0) 
    }
    
    if(!is.na(currentMetadata$Comment)) {
      grid.text(paste("Comment:", currentMetadata$Comment), x = 0, y = 0, hjust = 0)
    } else {
      grid.text(paste("Comment:"), x = 0, y = 0, hjust = 0)
    }
    
    if(!is.na(currentMetadata$AtlanticLibrary)) {
      grid.text(paste("Atlantic Lib:", currentMetadata$AtlanticLibrary), x = 0.33, y = 0.25, hjust = 0)
    } else {
      grid.text(paste("Atlantic Lib:"), x = 0.33, y = 0.25, hjust = 0)
    }
    
    popViewport()
    
    
    #---------- Draw spectrum ----------
    
    pushViewport(viewport(layout.pos.row = 3))
    
    currentSpectrum$percent.intensity <- with(currentSpectrum, intensity / max(intensity) * 100)
    
    # Calculate molecular weight to set x-axis upper limit
    
    if(!is.na(currentMetadata$Formula)) {
      
      mw <- MolecularWeight(formula = ListFormula(currentMetadata$Formula))
      xMax <- mw + (mw * 0.03)
      
    } else {
      
      m <- max(currentSpectrum$mz) 
      xMax <- m + (m * 0.03)
      
    }
    
    plot.data <-currentSpectrum[currentSpectrum$mz >= xMin & currentSpectrum$mz <= xMax, ]
    
    pushViewport(plotViewport(c(3.75, 3.5, 1.5, 1)))
    pushViewport(dataViewport(xscale = c(xMin, xMax),
                              yscale = c(0, 110)))
    
    grid.rect()
    p.ticks <- pretty(plot.data$mz, n = 10)
    x.ticks <- p.ticks[p.ticks >= xMin & p.ticks <= xMax]
    grid.xaxis(at = x.ticks)
    grid.yaxis(at = c(0, 25, 50, 75, 100))
    
    grid.segments(plot.data$mz,
                  plot.data$percent.intensity,
                  plot.data$mz,
                  rep(0, length(plot.data$intensity)),
                  default.units = "native",
                  gp = gpar(lwd = 0.75))
    
    ## print m/z values in plot
    
    display.values <- plot.data$mz[plot.data$display == TRUE]
    if (length(display.values) > 0) {
      grid.text(display.values,
                x = display.values,
                y = plot.data$percent.intensity[plot.data$display == TRUE] + 5,
                default.units = "native",
                gp = gpar(col = "blue"))
    }
    
    grid.text("intensity (%)", x = unit(-3.2, "lines"), rot = 90)
    grid.text("m/z", y = unit(-2.5, "lines"))
    
    # Automatic display of values every 25 units
    
    plotSegments <- cut(plot.data$mz, breaks = xMax/25)
    
    plot.data <- cbind(plot.data, plotSegments)
    
    for(i in 1:length(unique(plotSegments))) {
      
      segmentTmp <- plot.data[plot.data$plotSegments == unique(plotSegments)[i], ]
      
      maxPeak <- segmentTmp[segmentTmp$percent.intensity == max(segmentTmp$percent.intensity, na.rm = TRUE), ]
      
      if(maxPeak$percent.intensity[1] >= 5) {
        
        grid.text(maxPeak$mz,
                  x = maxPeak$mz,
                  y = maxPeak$percent.intensity + 5,
                  default.units = "native",
                  gp = gpar(col = "blue"))
      }
      
    }
    
    popViewport(3)
    
    # Define area below spectrum
    
    pushViewport(viewport(layout.pos.row = 4))
    
    pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 5, 
                                               widths = unit(c(0.05, 0.45, 0.05, 0.4, 0.05), "npc"))))
    
    
    #---------- Abundance Plot ----------
    
    pushViewport(viewport(layout.pos.col = 2))
    
    # Samples in which the compound was identified
    
    sampleList <- metadata[metadata$ReferenceSpectrum == currentMetadata$ReferenceSpectrum, c("Sample", "RelativeArea")]
    
    sampleList$Sample <- substr(sampleList$Sample, (nchar(as.character(sampleList$Sample)) - 2) + 1, nchar(as.character(sampleList$Sample)))
    
    xAxis = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8")
    
    pushViewport(plotViewport(c(2, 7, 1, 1), xscale = c(0.25, 8 + 0.75),
                              yscale = c(0, max(sampleList$RelativeArea))))
    
    grid.text("Detection in samples", x = unit(0.5, "npc"), y = unit(1.1, "npc"))
    
    grid.yaxis(at = c(0, 
                      signif(0.5 * max(sampleList$RelativeArea), 2), 
                      signif(max(sampleList$RelativeArea), 2)))
    
    grid.text("normalized peak area (unitless)", x = unit(-6, "lines"), rot = 90, gp = gpar(cex = 0.75))
    
    grid.xaxis(at = 1:8, label = xAxis)
    
    for (i in 1:8) {
      
      rectHeight <- sampleList$RelativeArea[sampleList$Sample == xAxis[i]]
      
      if(length(rectHeight) == 1) {
        
        grid.rect(x = unit(i, "native"), y = unit(rectHeight, "native"),
                  width = unit(0.65, "native"), height = unit(rectHeight, "native"),
                  just = "top", gp = gpar(fill = "light grey"))
        
      }
      
    }
    
    popViewport(2)
    
    
    #---------- Write fragment ion identifications ----------
    
    pushViewport(viewport(layout.pos.col = 4))
    
    grid.rect(x = unit(0, "npc"), height = unit(1, "npc"), width = unit(1, "npc"), gp = gpar(col = "black"), just = "left")
    
    grid.text("m/z [Fragment]", x = unit(0.075, "npc"), y = unit(0.9, "npc"), gp = gpar(col = "blue"), just = "left")
    
    grid.lines(x = unit(c(0, 1), "npc"), y = unit(0.8, "npc"), gp = gpar(col = "black"))
    
    if(!is.na(currentMetadata$FragmentIdentification)) {
      
      fragmentText <- gsub(pattern = "; ", replacement = "\n", x = currentMetadata$FragmentIdentification, fixed = TRUE)
      
      grid.text(fragmentText, x = unit(0.075, "npc"), y = unit(0.7, "npc"), gp = gpar(col = "black"), just = c("left", "top"))
      
    }
    
    popViewport(3) 
    
    # Write filename in corner
    
    pushViewport(viewport(layout.pos.row = 5))
    
    page <- filteredMetadata$page[filteredMetadata$SpectrumFilename == currentMetadata$SpectrumFilename] + 1
    
    grid.text(paste("Filename: ", currentMetadata$SpectrumFilename, ", Page: ", page, sep = ""),
              x = 1,
              just = c("right", "top"),
              gp = gpar(col = "dark grey"))
    
  }
  
  sapply(filteredMetadata$SpectrumFilename, 
         DrawSpectrum)
  
  graphics.off()
  
}

