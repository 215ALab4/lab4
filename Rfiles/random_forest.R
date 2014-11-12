# This file contains the random forest section of the analysis

library(dplyr)
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)
library(parallel) 
library(cvTools)
library(ROCR)
library(ggROC)

nCores <- 4
registerDoParallel(nCores)
setwd(file.path("/accounts/grad/janetlishali/Lab4"))

###############################################################################
# Random Forest function catered to formatted image files with fold in last 
# column
#

rf.specific <- function(train, num.tree){
  # Takes image file and returns the random forest class
  # Input:
  #   train - Image file to train on.  It contains features in 
  #           columns 4 up to the second last.  The 3rd column contains the labels
  #   num.tree - the number of trees to use in the forest
  
  rf <- try(randomForest(x = train[,4:(ncol(train)-1)], 
                         y = droplevels(as.factor(train[,3])), 
                         ntree=num.tree, 
                         confusion = T, 
                         importance = T))
  return(rf)
}

################################################################################
# Generate ROC plot data 
#

roc.data <- function(rf, test){
  # Takes random forest class and another test data set in same format as the 
  # trained dataset and returns the roc S4 class as given in ROCR package
  # Input:
  #   rf - random forest class
  #   test - test dataset
  
  rf.predict.prob <- predict(rf, test[4:(ncol(test)-1)], type = "prob")  
  preds <- rf.predict.prob[,2] #to get the percentage positive predictions
  for.roc <- prediction(preds, test[[3]])
  for.roc <- performance(for.roc, 'tpr', 'fpr')
  
  return(for.roc)
}

################################################################################
# AUC values
#

auc.value <- function(rf, test){
  # Takes in a random forest class (trained on some training set), applies 
  # random forest model to a test set in the same format as the training set 
  # and gives the AUC values for the prediction generated. 
  #
  # Input:
  #   rf - an object of class random forest
  #   test - a validation set
  # Output:
  #   auc - the auc for the roc of the model
  
  rf.predict.prob <- predict(rf, test[4:(ncol(test)-1)], type = "prob") 
  preds <- rf.predict.prob[,2] #to get the percentage positive predictions
  for.roc <- prediction(preds, test[[3]])
  auc.tmp <- performance(for.roc,"auc"); auc <- as.numeric(auc.tmp@y.values)
  return(auc)
}

################################################################################
# Confusion matrix
#

confusion.generate <- function(rf, test){
  # Takes in a random forest class (trained on some training set), applies 
  # random forest model to a test set in the same format as the training set 
  # and gives the confusion matrix of classification results.
  # Input:
  #   rf - an object of class random forest
  #   test - a validation set
  # Output:
  #   confusion.predict - a confusion matrix
  
  rf.predict <- predict(rf, test[,4:6])
  confusion.predict <- table(rf.predict, droplevels(as.factor(test[,3])))
  return(confusion.predict) 
}


################################################################################
################################################################################
##
## Analysis begins here
## 

# Here we load the three image files and set up the 12 fold validation via 
# quadrants.  The resulting file is 'combined' dataframe that contains the 
# validation folds 

image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

image1 <- tbl_df(image1)
image2 <- tbl_df(image2)
image3 <- tbl_df(image3)

# Name the columns usefully
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')

names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# Assign quadrants cross validation groups: 

image1 <- arrange(image1, (x))
half.row <- as.integer(nrow(image1)/2)
image1$block.x <- c(rep(TRUE,half.row), rep(FALSE, half.row+1))
image1 <- arrange(image1, (y))
image1$block.y <- c(rep(TRUE, half.row),rep(FALSE, half.row+1))
image1 <-  mutate(image1, 
                  fold = ifelse(block.x==TRUE & block.y==FALSE, 2, 1),
                  fold = ifelse(block.x==TRUE & block.y==TRUE, 3, fold), 
                  fold = ifelse(block.x == FALSE & block.y == TRUE, 4, fold))

image2 <- arrange(image2, (x))
half.row <- as.integer(nrow(image2)/2)
image2$block.x <- c(rep(TRUE,half.row), rep(FALSE, half.row))
image2 <- arrange(image2, (y))
image2$block.y <- c(rep(TRUE, half.row),rep(FALSE, half.row))
image2 <- mutate(image2, 
                 fold = ifelse(block.x==TRUE & block.y==FALSE, 6, 5), 
                 fold = ifelse(block.x==TRUE & block.y==TRUE, 7, fold), 
                 fold = ifelse(block.x == FALSE & block.y == TRUE, 8, fold))

image3 <- arrange(image3, (x))
half.row <- as.integer(nrow(image3)/2)
image3$block.x <- c(rep(TRUE,half.row), rep(FALSE, half.row+1))
image3 <- arrange(image3, (y))
image3$block.y <- c(rep(TRUE, half.row),rep(FALSE, half.row+1))
image3 <- mutate(image3, 
                 fold = ifelse(block.x==TRUE & block.y==FALSE, 10, 9), 
                 fold = ifelse(block.x==TRUE & block.y==TRUE, 11, fold), 
                 fold = ifelse(block.x == FALSE & block.y == TRUE, 12, fold))

combined <- rbind(image1, image2, image3) %>%  # combine the three images 
            dplyr::select(-block.x,-blocky) %>% # removes block.x and block.y
            mutate(combined, label=as.factor(label)) %>%
            filter(combined, label != 0) %>% #filter out unlabelled points.  
            mutate(combined, label = droplevels(label))

# Depending on whether we want to train on all features, or only NDAI, SD and 
# CORR, we can set set TopThree to TRUE

TopThree <- TRUE
if (TopThree){
  combined <- cbind(combined[,2:7], combined[["fold"]])
  names(combined)[7] <- "fold"
  combined <- tbl_df(combined)
}

################################################################################
# Apply Random Forest model to all 12 Folds

num.tree <- 100 # set the number of trees you would like to train random forest on
num.folds <- 12 # set to the number of folds in your CV set

Random.Forest.folds <- foreach(i = 1:num.folds)%dopar% {
  #This function applies random forest to all 12 folds using the 'combined'
    #dataset given above.  
  #This will save the random forest class generated for each fold as
    #an Rdata file to be retrieved later for plots. This file will be named
    # iRF_block.Rdata, where 'i' is the number of the fold. 
  
  #It also saves the AUC of each CV, as 'AUC_blocki.csv.  Same i as above. 
  
  #Lastly, this function saves the confusion matrix of each cross validation.
    #as ROC_blocki.csv
  
  train <- filter(combined, fold != i) #so we drop the ith fold
  test <- filter(combined, fold == i) #test on the ith fold
  rf <- try(rf.specific(train, num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <-sprintf("ROC_block%d.csv", i)
  write.csv(conf, file=filename.table)
  auc <- try(auc.value(rf, test))
  filename.auc <- sprintf("AUC_block%d.csv", i)
  write.csv(auc, file=filename.auc)
  filename <- sprintf("%sRF_block.Rdata", i)
  try(save(rf,file =filename))
  return(filename) 
}

################################################################################
# Image-wise validation

# This script applies random forest to all 2/3 of the 3 images and trains on 
# remaining one. 

# This will save the random forest class generated for each image as
# an Rdata file to be retrieved later for plots. This file will be named
# iRF_image.Rdata, where 'i' is 1, 4 or 8, representing image 1, 2, 3.. 

# It also saves the AUC of each CV, as 'AUC_imagei.csv.  Same i as above. 

# Lastly, this script saves the confusion matrix of each cross validation.
# as ROC_imagei.csv

image.cv.1.3 <- foreach(i = c(1,9))%dopar% {

  test <- filter(combined, fold %in% i:(i+4)) #so we drop the ith fold
  train <- anti_join(combined, train) #test on the ith fold
  rf <- try(rf.specific(train, num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <-sprintf("ROC_image%d.csv", i)
  write.csv(conf, file=filename.table)
  auc <- try(auc.value(rf, test))
  filename.auc <- sprintf("AUC_image%d.csv", i)
  write.csv(auc, file=filename.auc)
  filename <- sprintf("%sRF_image.Rdata", i)
  try(save(rf,file =filename))
  return(filename) 
}

image.cv.2 <- for(i in 5:5){
    
    test <- filter(combined, fold=5 | fold = 6 | fold = 8) #so we drop the ith fold
    train <- anti_join(combined, train) #test on the ith fold
    rf <- try(rf.specific(train, num.tree))
    conf <- try(confusion.generate(rf,test))
    filename.table <-sprintf("ROC_image%d.csv", i)
    write.csv(conf, file=filename.table)
    auc <- try(auc.value(rf, test))
    filename.auc <- sprintf("AUC_image%d.csv", i)
    write.csv(auc, file=filename.auc)
    filename <- sprintf("%sRF_image.Rdata", i)
    try(save(rf,file =filename))
    return(filename) 
}
################################################################################
# Convergence analysis and validation

# This script trains the random forest model on the first i folds and tests 
# the remain folds

# This will save the random forest class predicted for each test as
# an Rdata file to be retrieved later for plots. This file will be named
# iRF_converge.Rdata, where 'i' is 1-11. 

# It also saves the AUC of each CV, as 'AUC_convergei.csv.  Same i as above. 

# Lastly, this script saves the confusion matrix of each cross validation.
# as ROC_convergei.csv

convergence.cv <- foreach(i = 1:11)%dopar% {

  train <- filter(combined, fold <= i) #so we drop the ith fold
  test <- anti_join(combined, train) #test on the ith fold
  rf <- try(rf.specific(train, num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <-sprintf("ROC_converge%d.csv", i)
  write.csv(conf, file=filename.table)
  auc <- try(auc.value(rf, test))
  filename.auc <- sprintf("AUC_converge%d.csv", i)
  write.csv(auc, file=filename.auc)
  filename <- sprintf("%sRF_converge.Rdata", i)
  try(save(rf,file =filename))
  return(filename) 
}

################################################################################
# ROC curve data frame for plotting (of the 12 folds)
#

ROC.curve.data.frame.begin <- function{
  # First seed the dataframe with the ROC curve information from the 1st fold
  # gives us a dataframe with the 1st fold's ROC curve plotting info (x and y 
  # values)
  # To output ROC information for the convergence data, just substitute 'block'
  # in sprintf below with 'convergence'.  Same with 'image'
  
  load("1RF_block.Rdata")
  roc.data <- roc.data(rf, filter(combined, fold == 1) )
  x.values <- roc.data@x.values[[1]]
  col.length <- length(x.values)
  ROC.data <- as.data.frame(1:col.length)
  ROC.data[,1] <- "1st fold"
  colnames(ROC.data)[1] <- "fold.number"
  ROC.data$x.values <- x.values
  ROC.data$y.values <- roc.data@y.values[[1]]
  
  return(ROC.data)
}
################################################################################
# Create entire ROC curve dataframe

ROC.curve.data.frame <- function(ROC.curve.data.frame.begin){
 
  # Input previous function: ROC.curve.data.frame.begin
  # outputs the entire dataframe of ROC curve data for all other folds.  
  # to output ROC information for the convergence data, just substite 'block'
  # in sprintf below with 'convergence'.  Same with 'image'
  # writes to csv the ROC dataframe to be plotted in the random_forest_plots.R file
  
  filename <- sprintf("%dRF_block.Rdata", i)
  load(filename)
  rocData <- roc.data(rf, filter(combined, fold == i) )
  x.values <- rocData@x.values[[1]]
  col.length <- length(x.values)
  data <- as.data.frame(1:col.length)
  colnames(data)[1] <- "fold.number"
  fold.name <- sprintf("%d fold", i)
  data[,1] <- fold.name
  data$x.values <- x.values
  data$y.values <- rocData@y.values[[1]]
  ROC.data <<- rbind(ROC.data, data)
  write.csv(ROC.data, "ROC_convergence_comparison.csv")
  return(ROC.data)
}


################################################################################
# False positive, False negative, True positive, True negative plots


False.positive.False.negative.Plots <- function(image, rf){
    # This script generates false positive false negative plots for a prediction
    # we use the rf and image loaded above
    # outputs 3 pdf files of the raw image with NDAI plotted via geom_point, the
    # correct expert classification plotted, and finally the predictions with
    # factors indication whether the prediction was a false positive, true
    # positive, false negative or true negative.
    
    #this is a prediction generated from rf on image
    image$predicted <- predict(rf, image[4:6])
    
    image <- tbl_df(image) %>%
    mutate(classification = rep(0, nrow(image))) %>%
    mutate(classification =
    ifelse(predicted == label & label == "1",
    "true positive", classification),
    classification = ifelse(predicted == label &
    predicted == "-1",
    "true negative",
    classification),
    classification = ifelse(predicted !=label &
    predicted == "-1",
    "false negative",
    classification),
    classification = ifelse(label != predicted &
    label == "-1",
    "false positive",
    classification))
    fpr <- geom_point(aes(x=x, y=y, color=classification))
    image.NDAI <- geom_point(aes(x=x, y=y, color = NDAI))
    raw.image <- geom_point(aes(x=x, y=y, color=as.factor(label)))
    
    colour <- scale_colour_manual(values = c("true positive" = "white",
    "false positive" = "black",
    "true negative" = "#3366FF",
    "false negative" = "#FF00CC",
    "Unknown" = "#999999"))
    
    im3 <- ggplot(image)
    
    png("classification_10.png")
    im3+fpr+colour
    dev.off()
    
    png("label_10.png")
    im3 + raw.image
    dev.off()
    
    png("NDAI_10.png")
    im3 + image.NDAI
    dev.off()
    
    return(im3+fpr)
}


################################################################################
# ROC curve plot functions

#we gave an example of what riles contains the comparison csv, this is generated in the random_forest_functions.R file


plot.roc <- function(filename){
    ROC.data <- read.csv(filename)
    colnames(ROC.data)[2] <- "False.positive.rate"
    colnames(ROC.data)[3] <- "True.positive.rate"
    pdf("ROC_converge_comparison.pdf")
    ggplot(ROC.data, aes(x=False.positive.rate, y = True.positive.rate))+geom_line(aes(colour= number.of.quadrants, group = number.of.quadrants))
    dev.off()
}


################################################################################
# GINI importance from 9 feature training set

##################################################
# Gini Data Frame for Comparison

Gini.data.frame <- function{
    # This script creates a data frame with the Gini importance measure of each
    # feature for the all 12 folds.
    
    load("1RF_block.Rdata")
    rf # this is the random forest class that was loaded
    
    Gini <- as.data.frame(rf$importance[,4])
    colnames(Gini)[1] <- "1st fold"
    
    compare.gini <- for (i in c(2:6, 8:12)){
        # This function generates a dataframe with all 12 folds of the Gini
        # importance measures
        
        filename <- sprintf("%dRF_block.Rdata", i)
        load(filename)
        Forest <- sprintf("%dst fold", i)
        Gini <<- cbind(Gini, as.data.frame(rf$importance[,4]))
        colnames(Gini)[ncol(Gini)] <- Forest
    }
    return(Gini)
}

################################################################################
# Gives mean and variance summary for Gini dataframe:

Gini.reformat <- function{
    
    Gini$mean <- rowMeans(Gini)
    Gini$variance <- rowVars(as.matrix(Gini[,1:11]))
    Gini$sd <- sqrt(Gini$variance)
    return(Gini)
    
}

################################################################################
# Reshape Gini

Reshape.Gini <- function(Gini){
    # Reformats the Gini dataframe for plotting of Gini Importance:
    # Outputs the saved plot
    
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
# AUC table
#

AUC.table <- function{
    # Get table of AUC measurements
    
    AUC <-read.csv("AUC_block1.csv")
    colnames(AUC)[2] <- "1st fold"
    AUC <- dplyr::select(AUC, -X)
    
    compare.AUC <- for (i in c(2:6, 8:12)){
        # Adds the rest of the AUC data using dataframe generated above
        filename <- sprintf("AUC_block%d.csv", i)
        AUC.num <- sprintf("%dst fold", i)
        file <- read.csv(filename)
        colnames(file)[2] <- AUC.num
        file <- dplyr::select(file, -X)
        AUC <<- cbind(AUC, file)
    }
    
    rownames(AUC) <- "AUC"
    AUC <- t(AUC)
    
    png("AUC_12folds.png")
    grid.table(AUC,show.rownames=T)
    dev.off()
    return(AUC)
}


################################################################################
# - Image Files  -
#
# If you want to save the images, ImageSave to TRUE

if (ImageSave){
    
    # Image False Classification plots:
    filename <- "ROC_image1.Rdata"
    image <- image1
    False.positive.False.negative.Plots(image, rf)
    
    filename <- "ROC_image5.Rdata"
    image <- image2
    False.positive.False.negative.Plots(image, rf)
    
    filename <- "ROC_image9.Rdata"
    image <- image3
    False.positive.False.negative.Plots(image, rf)
    
    # ROC comparison plots
    filename <- "ROC_convergence_comparison.csv"
    
    plot.roc(filename)
    
    #Gini plots
    Gini <- Reshape.Gini(Gini.reformat(Gini.data.frame))
    Reshape.Gini <- function(Gini)
    
    #AUC table
    AUC.table
    
    
}




