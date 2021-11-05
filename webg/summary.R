source("countTF.R")

# Pick the the top GO term (lowest p-value) for each TF
# 1) p-value filter (p < 10^-5)
# 2) From those, pick the one with highest enrichment

# Histogram of -log p-values for top terms
# Histogram of enrichment ratios


ORA_output_summary <- function(interestGenFile, pValueFilter) {
  # First, parse the enrichment results file for the p-values and the enrichment ratios
  # Filter out if the p-value does not meet a certain threshold -> pick the one with the highest ER
  
  if (!dir.exists("ORA Summary")) {
    dir.create("ORA Summary")
  } else {
    unlink("ORA Summary", recursive = TRUE)
    dir.create("ORA Summary")
  }
  
  filename <- paste(getwd(), "/ORA Summary/ORA_summary.txt", sep = "")
  f = file(filename, open = 'a')
  
  for (i in 1:countTF(interestGenFile)) {
    # Read for gene set, pval, enrichment ratio, FDR, and size
    if (file.exists(paste(getwd(), "/Project_",nameTF(interestGenFile)[i], "/enrichment_results_", nameTF(interestGenFile)[i], ".txt",sep = ""))) {
      geneFile <- read.table(paste(getwd(), "/Project_",nameTF(interestGenFile)[i], "/enrichment_results_", nameTF(interestGenFile)[i], ".txt",sep = ""), sep = "\t", header = TRUE, quote = "")
      parameters <- list()
      ER <- vector()
      count <- 0
      
      # filter out the genes that do not meet the threshold
      for (j in 1:length(geneFile$geneSet)) {
        if (geneFile$pValue[j] < pValueFilter) {
          count <- count + 1
          
          # index of the description vector is the same as the ER index
          parameters[[count]] <- paste(as.character(geneFile$geneSet[j]),"\t",geneFile$pValue[j],"\t",geneFile$enrichmentRatio[j],"\t",geneFile$FDR[j],"\t",geneFile$size[j])
          ER[count] <- geneFile$enrichmentRatio[j]
          #print(er)
        }
      }
    }

    # check if there are values in ER
    # then print the gene set with the maximum enrichment ratio
    if (count != 0) {
      write(paste(nameTF(interestGenFile)[i],parameters[[match(max(ER), ER)]], sep = "\t"), file = filename, append = TRUE)
    } else {
      write(nameTF(interestGenFile)[i], file = filename, append = TRUE)
    }
  }
  
  #Output histograms to summary file
  pvals <- read.table(filename, fill = TRUE)$V3
  png(paste(getwd(), "/ORA Summary/top_pvalues.png", sep=""))
  hist(-log(pvals), breaks = 50, col = "yellow", main = "Top P-Values", xlab = "-log(p-value)")
  dev.off()
  
  enrichmentRatios <- read.table(filename, fill = TRUE)$V4
  png(paste(getwd(), "/ORA Summary/top_enrichment_ratios.png", sep=""))
  hist(log(enrichmentRatios), breaks = 50, col = "green", main = "Top Enrichment Ratio Values", xlab = "log(enrichment ratio)")
  dev.off()
  
  sizes <- read.table(filename, fill = TRUE)$V6
  png(paste(getwd(), "/ORA Summary/significant_sizes.png", sep=""))
  hist(sizes, breaks = 50, col = "purple", main = "Size Values of Significant P-Values", xlab = "gene set size")
  dev.off()
  
  close(filename)
}

ORA_output_summary("/Users/clint/Desktop/ORA/edges copy.csv", 10^-3)
