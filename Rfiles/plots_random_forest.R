#this R file generates all plots used in the Random Forests section of the write
#up.  We draw on the files outputted from functions_random_forest.R 

library(dplyr)
library(ggplot2)
library(randomForest)
library(foreach)
require(doParallel)
require(parallel) 
library(cvTools)
library(ROCR)
library(reshape)
library(matrixStats)
library(grid)
library(gridBase)
library(gridTable)
setwd(file.path("/accounts/grad/janetlishali/Lab4"))

######################################################################
#False positive, False negative, True positive, True negative plots 

#specify below which random forest model you would like to generate plots for
  #remember that for the 12 fold validation, the Rdata files are in the form:
    #'iRF_block.Rdata", where 'i' is the fold number

filename <- "10RF_block.Rdata"
load(file=filename)
rf #this is the random forest class that we have loaded. 

image <- image3 #here, specify which image you would like to make the 
                #plots on.  Since the 10th fold choosen above lies in image 3, 
                # we choose image to be image3 for now. 

False.positive.False.negative.Plots <- function{
  #The following generates false positive false negative plots for a prediction  
  # we use the rf and image loaded above
  #outputs 3 pdf files of the raw image with NDAI plotted via geom_point, the 
    #correct expert classification plotted, and finally the predictions with 
    #factors indication whether the prediction was a false positive, true 
    #positive, false negative or true negative. 
  
  b <- predict(rf, image[4:6]) #this is a prediction generated from rf on image
  image$predicted <- b
  image <- tbl_df(image)
  image <- image %.% mutate(classification = rep(0, nrow(image)))
  image <- image %.% mutate(classification = 
                            ifelse(predicted == label & label == "1", 
                                   "true positive", classification),
                            classification = ifelse(predicted == label & 
                                                      predicted == "-1", 
                                                    "true negative", 
                                                    classification),
                            classification = ifelse(predicted !=label & 
                                                      predicted == "-1", 
                                                    "false positive", 
                                                    classification),
                            classification = ifelse(label != predicted & 
                                                      label == "-1", 
                                                    "false negative", 
                                                    classification))
  fpr <- geom_point(aes(x=x, y=y, color=classification))
  image.NDAI <- geom_point(aes(x=x, y=y, color = NDAI))
  raw.image <- geom_point(aes(x=x, y=y, color=as.factor(label)))

  im3 <- ggplot(image)

  png("classification_10.png")
  im3+fpr
  dev.off()

  png("label_10.png")
  im3 + raw.image
  dev.off()

  png("NDAI_10.png")
  im3 + image.NDAI
  dev.off()

  return(im3+fpr)
}

######################################################################
# GINI importance from 9 feature training set 

##################################################
#Gini Data Frame for Comparison

Gini.data.frame <- function{
  #the following creates a data frame with the Gini importance measure of each 
  #feature for the all 12 folds. 
  
  load("1RF_block.Rdata")
  rf #this is the random forest class that was loaded 
  
  Gini <- as.data.frame(rf$importance[,4])
  colnames(Gini)[1] <- "1st fold"
  
  compare.gini <- for (i in c(2:6, 8:12)){
    #this function generates a dataframe with all 12 folds of the Gini importance 
    #measures
    
    filename <- sprintf("%dRF_block.Rdata", i)
    load(filename)
    Forest <- sprintf("%dst fold", i)
    Gini <<- cbind(Gini, as.data.frame(rf$importance[,4]))
    colnames(Gini)[ncol(Gini)] <- Forest
  }
  return(Gini)
}

##################################################
#Gives mean and variance summary for Gini dataframe: 
Gini.reformat <- function{
  Gini$mean <- rowMeans(Gini)
  Gini$variance <- rowVars(as.matrix(Gini[,1:11]))
  Gini$sd <- sqrt(Gini$variance)
  return(Gini)
}

##################################################
#Reshape Gini

Reshape.Gini <- function{
  #reformats the Gini dataframe for plotting of Gini Importance:
  #outputs the saved plot
  
  Gini <- t(Gini)
  Gini <- as.data.frame(Gini)
  Gini <- mutate(Gini, fold <- c(1:6, 8:12)) 
  colnames(Gini)[9] <- "fold.number"
  Gini <- melt(Gini, id = "fold.number")
  colnames(Gini)[1]<- "fold"
  colnames(Gini)[2] <- "variable"
  colnames(Gini)[3] <- "GiniImportance"
  
  pdf("Gini_Importance.pdf")
  ggplot(Gini)+geom_point(aes(x=fold, y = GiniImportance, colour = variable))
    +geom_smooth(aes(group = variable, x = fold, y = GiniImportance))
  dev.off()
}

################################################################################
#AUC table
#

AUC.table <- function{
  #Get table of AUC measurements
  
  AUC <-read.csv("AUC_block1.csv")
  colnames(AUC)[2] <- "1st fold"
  AUC$X <- NULL
  
  compare.AUC <- for (i in c(2:6, 8:12)){
    #adds the rest of the AUC data using dataframe generated above
    filename <- sprintf("AUC_block%d.csv", i)
    AUC.num <- sprintf("%dst fold", i)
    file <- read.csv(filename)
    colnames(file)[2] <- AUC.num
    file$X <- NULL
    AUC <<- cbind(AUC, file)
  }
  
  rownames(AUC) <- "AUC"
  AUC <- t(AUC)
  
  png("AUC_12folds.png")
  grid.table(AUC,show.rownames=T)
  dev.off()
  return(AUC)
}


