library(openssl)
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
library(kinship2)
library(igraph)
library(reshape2)
library(plyr)
library(geosphere)
library(GGally)
library(network)
library(sna)
library(rethinking)
library(colorspace)
library(rstan)
library(rethinking)
library(PlvsVltra)
library(graphlayouts)
library(ggraph)
library(oaqc)
library(STRAND)

################################################ Set WD
 path = "C:\\Users\\..."
 setwd(path)

######################### Load data
 load("PublicData.RData")

######################### Descriptives
 source("Payout_Figures.R")
 source("Network_Figures.R")
 source("Descriptives.R")

######################### Run models
 source("Code/Model_BS.R")
 source("Code/Model_SC.R")
 source("Code/Model_SU.R")
 source("Code/Model_TB.R")

 source("Code/Model_TB_BySex.R")
 source("Code/Model_SU_BySex.R")
 source("Code/Model_SC_BySex.R")
 source("Code/Model_BS_BySex.R")

 source("Code/Caterpillar_Figures.R")
 source("Code/Caterpillar_Figures_M.R")
 source("Code/Caterpillar_Figures_F.R")
 source("Code/Caterpillar_Figures_BySex.R")








