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
library(PlvsVltra)    # github.com/ctross/PlvsVltra
library(graphlayouts)
library(ggraph)
library(oaqc)
library(STRAND)
library(xtable)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

################################################ Set WD
 path = "C:\\Users\\your_path\\prestige_and_dominance-main"
 setwd(path)

######################### Load data
 load("PublicData.RData")

######################### Descriptives
 source("Payout_Figures.R")
 source("Network_Figures.R")
 source("Descriptives.R")

######################### Run models
 source("Model_BS.R")
 source("Model_SC.R")
 source("Model_SU.R")
 source("Model_TB.R")

 source("Model_TB_BySex.R")
 source("Model_SU_BySex.R")
 source("Model_SC_BySex.R")
 source("Model_BS_BySex.R")

 source("Caterpillar_Figures.R")
 source("Caterpillar_Figures_M.R")
 source("Caterpillar_Figures_F.R")
 source("Caterpillar_Figures_BySex.R")








