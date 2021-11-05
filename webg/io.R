source("webg_function.R")
source("countTF.R")

#Basic Function Variables
enrichMethod <- NULL
organism <- NULL

enrichDatabase <- NULL
enrichDatabaseFile <- NULL
enrichDatabaseType <- NULL
enrichDatabaseDescriptionFile <- NULL

interestGeneFile <- NULL
interestGene <- NULL
interestGeneType <- NULL

referenceGeneFile <- NULL
referenceGene <- NULL
referenceGeneType <- NULL
referenceSet <- NULL

outputDirectory <- NULL
projectName <- NULL

#Advanced Function Variables
collapseMethod <- "mean"
minNum <- 10
maxNum <- 500
sigMethod <- "fdr"
fdrMethod <- "BH"
fdrThr <- 0.05
topThr <- 10
reportNum <- 20
perNum <- 1000
gseaP <- 1
isOutput <- TRUE
dagColor <- "continuous"
saveRawGseaResult <- FALSE
gseaPlotFormat <- c("png","svg")
setCoverNum <- 10
networkConstructionMethod <- NULL
neighborNum <- 10
highlightType <- "Seeds"
highlightSeedNum <- 10
nThreads <- 1
cache <- NULL

#Helper variables
enrichMethodList <- c("ORA", "GSEA", "NTA")

#Opener
writeLines("Welcome to WebGestaltR: The R interface for enrichment analysis with WebGestalt!")

#method
writeLines("Which enrichment method do you want to use?")
writeLines(enrichMethodList)
enrichMethod <- readline()

#Organism
writeLines("Which organism are you analyzing?")
writeLines(listOrganism())
organism <- readline()

#Enrichment info
writeLines("What is your enrichment database? (Type N to use your own. Type D to use the Biological, Molecular, and Cellular databases at one time.)")
#print(listGeneSet())
enrichDatabase <- readline()

# If using own DB
if (enrichDatabase == "N") {
  enrichDatabase <- NULL
  
  writeLines("Enter the pathname.")
  enrichDatabaseFile <- readline()
  
  writeLines("Enter the ID type of the genes contained in the file.")
  enrichDatabaseType <- readline()
  
  writeLines("Do you have a description file? If so, enter the pathname. If not, type 'N'")
  enrichDatabaseDescriptionFile <- readline()
  
  if (enrichDatabaseDescriptionFile == 'N') {
    enrichDatabaseDescriptionFile <- NULL
  }
}

# If using the Big Three of GO
if (enrichDatabase == "D") {
  enrichDatabase <- c("geneontology_Biological_Process_noRedundant", "geneontology_Molecular_Function_noRedundant", "geneontology_Cellular_Component_noRedundant")
}

# Interest Gene Info
writeLines("What is the pathname of your gene set of interest?")
interestGeneFile <- readline()

# How many times to run Web Gestalt
sprintf("Number of unique transcription factors: %d", countTF(interestGeneFile))

writeLines("Do you have a specific gene of interest? Y or N")
interestGene <- readline()

if (interestGene == "N") {
  interestGene <- NULL
}

writeLines("What is the ID type of the interest gene set?")
writeLines(listIdType())
interestGeneType <- readline()

# Reference Gene File
writeLines("Do you have your own reference gene set? Y or N")
answer <- readline()

if (answer == "Y") {
  writeLines("What is the pathname of your reference gene set?")
  referenceGeneFile <- readline()
  
  writeLines("Do you have a specific reference gene? Y or N")
  referenceGene <- readline()
  
  if (referenceGene == "N") {
    referenceGene <- NULL
  }
  
  writeLines("What is the ID type of the reference gene set?")
  writeLines(listIdType())
  referenceGeneType <- readline()
} else {
  writeLines("Which WebGestalt reference gene set do you wish to use?")
  referenceSet <- readline()
}

# Place output
writeLines("Where do you want your output to be placed? (Type 'WD' for your working directory)")
outputDirectory <- readline()

if (outputDirectory == "WD") {
  outputDirectory <- getwd()
}

#writeLines("What do you want to name this output? (Type N to name each output after its TF)")
#projectName <- readline()

#if (outputDirectory == "N") {
#  projectName <- ""
#}

# Advanced Parameters
writeLines("Do you wish to use the advanced parameters? Y or N")
answer <- readline()

if (answer == "N") {
  # Create separated files
  parseInterestFile(interestGeneFile)
  
  igf <- interestGeneFile
  # Perform for each gene file
  for (i in 1:countTF(igf)) {
    # Output Example: KFLP4  _ORA_geneontology_Biological_Process_noRedundant
    # projectName <- paste(nameTF(igf)[i],"_",enrichMethod,"_",enrichDatabase, sep = "")
    projectName <- nameTF(igf)[i]
    
    # Input Example: Sliced TF File/KFLP.txt
    interestGeneFile <- paste("Parsed TF Files/", nameTF(igf)[i], ".txt",sep = "")
    
    WebGestaltR(
      enrichMethod,
      organism,
      enrichDatabase,
      enrichDatabaseFile,
      enrichDatabaseType,
      enrichDatabaseDescriptionFile,
      interestGeneFile,
      interestGene,
      interestGeneType,
      collapseMethod,
      referenceGeneFile,
      referenceGene,
      referenceGeneType,
      referenceSet,
      minNum,
      maxNum,
      sigMethod,
      fdrMethod,
      fdrThr,
      topThr,
      reportNum,
      perNum,
      gseaP,
      isOutput,
      outputDirectory,
      projectName,
      dagColor,
      saveRawGseaResult,
      gseaPlotFormat,
      setCoverNum,
      networkConstructionMethod,
      neighborNum,
      highlightType,
      highlightSeedNum,
      nThreads,
      cache,
      hostName = "http://www.webgestalt.org/"
    )
  }
} else {
  collapseMethod <- readline(prompt="collapseMethod: ")
  
  minNum <- readline(prompt="minNum: ")
  
  maxNum <- readline(prompt="maxNum: ")
  
  sigMethod <- readline(prompt="sigMethod: ")
  
  fdrMethod <- readline(prompt="fdrMethod: ")
  
  fdrThr <- readline(prompt="fdrThr: ")
  
  topThr <- readline(prompt="topThr: ")
  
  reportNum <- readline(prompt="reportNum: ")
  
  perNum <- readline(prompt="perNum: ")
  
  gseaP <- readline(prompt="gseaP: ")
  
  isOutput <- readline(prompt="isOutput: ")
  
  dagColor <- readline(prompt="dagColor: ")
  
  saveRawGseaResult <- readline(prompt="saveRawGseaResult: ")
  
  gseaPlotFormat <- readline(prompt="gseaPlotFormat: ")
  
  setCoverNum <- readline(prompt="setCoverNum: ")
  
  networkConstructionMethod <- readline(prompt="networkConstructionMethod: ")
  
  neighborNum <- readline(prompt="neighborNum: ")
  
  highlightType <- readline(prompt="highlightType: ")
  
  highlightSeedNum <- readline(prompt="highlightSeedNum: ")
  
  nThreads <- readline(prompt="nThreads: ")
  
  cache <- readline(prompt="cache: ")
  
  
  # Create separated files
  parseInterestFile(interestGeneFile)
  
  igf <- interestGeneFile
  # Perform for each gene file
  for (i in 1:countTF(igf)) {
    # Output Example: KFLP4_ORA_geneontology_Biological_Process_noRedundant.txt
    # projectName <- paste(nameTF(igf)[i],"_",enrichMethod,"_",enrichDatabase, ".txt", sep = "")
    projectName <- nameTF(igf)[i]
    
    # Input Example: Sliced TF File/KFLP.txt
    interestGeneFile <- paste("Parsed TF Files/", nameTF(igf)[i], ".txt",sep = "")
    
    WebGestaltR(
      enrichMethod,
      organism,
      enrichDatabase,
      enrichDatabaseFile,
      enrichDatabaseType,
      enrichDatabaseDescriptionFile,
      interestGeneFile,
      interestGene,
      interestGeneType,
      collapseMethod,
      referenceGeneFile,
      referenceGene,
      referenceGeneType,
      referenceSet,
      minNum,
      maxNum,
      sigMethod,
      fdrMethod,
      fdrThr,
      topThr,
      reportNum,
      perNum,
      gseaP,
      isOutput,
      outputDirectory,
      projectName,
      dagColor,
      saveRawGseaResult,
      gseaPlotFormat,
      setCoverNum,
      networkConstructionMethod,
      neighborNum,
      highlightType,
      highlightSeedNum,
      nThreads,
      cache,
      hostName = "http://www.webgestalt.org/"
    )
  }
}