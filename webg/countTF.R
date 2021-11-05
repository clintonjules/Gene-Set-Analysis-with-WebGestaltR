countTF <- function(TFfileIn) {
  # Read in the TF file
  readIn <- read.csv(TFfileIn, header = FALSE)
  
  # Count the TF
  return (as.integer(length(unique(readIn$V1))))
}

firstTFoccurence <- function(TFfileIn, tfName) {
  readIn <- read.csv(TFfileIn, header = FALSE)$V1
  result_index <- match(tfName, readIn)
  
  if (is.na(result_index)) {
    "There is no match found"
  } else {  
    result_index
  }
  
  return (result_index)
}

nameTF <- function(TFfileIn) {
  # Read in the TF file
  readIn1 <- read.csv(TFfileIn, header = FALSE)
  
  # TF Names
  names <- unique(readIn1$V1)
  names <- c(as.character(names))
  
  return (names)
}


# Goals:
# Go throught the TF and gene file (TF- col 1, Gene - col 2)
# Go thorugh each line to retrieve the gene
# Place each gene into the correct TF file
parseInterestFile <- function(TFfileIn) {
  # Output Directory
  if (!dir.exists("Parsed TF Files")) {
    dir.create("Parsed TF Files")
  } else {
    unlink("Parsed TF Files", recursive = TRUE)
    dir.create("Parsed TF Files")
  }
  
  # Read in the TFs and the genes separately
  TFread <- read.csv(TFfileIn, header = FALSE)$V1
  geneRead <- read.csv(TFfileIn, header = FALSE)$V2
  
  # For each TF, create a file with the TF name as the file name 
  for (i in 1:length(TFread)) {
    filename <- paste("Parsed TF Files/",TFread[i],".txt",sep = "")
    writeGeneFile = file(filename, open = 'a')
    
    writeLines(as.character(geneRead[i]), writeGeneFile )
  
  # Make sure to close or the full list won't write!
  close(writeGeneFile)
  }
}