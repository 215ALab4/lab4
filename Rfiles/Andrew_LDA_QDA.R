require(dplyr)
require(reshape2)
require(MASS)
require(ROCR)
require(ggplot2)

setwd("D:/coursework/stat215A/lab4")

###############################################################################
#  Split Image Function
#

SplitImage <- function(image, xsplit, ysplit){
  # Function takes an image and divides it with xsplit by ysplit grid.
  #   
  # Input:
  #   image - image data frame to be split
  #   xsplit - number of partitions in the x-dimension
  #   ysplit - number of partitions in the y-dimension
  # Output:
  #   A list containing xsplit*ysplit sub-data frames
  #   To call on a particular subset, use the [[]] operator
  # Example:
  #   For example, to split image1 using a 3x4 grid and save it to foo, the 
  #   syntax would read foo <- SplitImage(image1, xsplit = 3, ysplit = 4).
  #   To call on the bottom-left part of the image, we would write
  #   foo[[1]]    
  
  xmin <- min(image$x)
  ymin <- min(image$y)
  
  xmax <- max(image$x)
  ymax <- max(image$y)
  
  # Declare the list of data frames
  subimages <- replicate(xsplit*ysplit, data.frame())
  
  xstep.size <- ceiling((xmax-xmin)/xsplit)
  ystep.size <- ceiling((ymax-ymin)/ysplit)
  
  for(i in 1:xsplit){
    for(j in 1:ysplit){
      
      n <- (i-1)*ysplit + j  # n is the index within the list
      subimages[[n]] <- filter(image, 
                               x >= xmin + (i-1)*xstep.size, 
                               x < xmin + i*xstep.size,
                               y >= ymin + (j-1)*ystep.size, 
                               y < ymin + j*ystep.size)
      
    }
  }
  return(subimages)
}

###############################################################################
#  Image into labels and features function
#

LabelsFeatures <- function(image){
  # Takes image file and returns two data frames, labels and features
  # labels is single column data frame containing the expert lablels
  # features is a three-column data frame containing NDAI, SD, and CORR
  # The two can be called using $labels and $features
  labelled <- filter(image, label != 0)
  labels <- dplyr::select(labelled, label)
  features <- dplyr::select(labelled, NDAI, SD, CORR)
  
  return(list(labels=labels, features=features))
}

###############################################################################
#  Posterior Probabilities
#

MyPosterior <- function(newdata, training.object){
  # Uses training data posterior to classify new cases
  # Input:
  #   newdata - data frame of cases to be classified
  #   training.object - an LDA or QDA object using training data
  # Output:
  #   A data frame with posterior probabilities of a case being in class 1
  #   and true classification as its columns
  
  pred <- predict(training.object, type="prob",newdata=newdata)
  pred <- data.frame(x = newdata$x,
                     y = newdata$y,
                     posterior = pred$pos[,2], 
                     label = newdata$label)
  
  return(pred)
}
###############################################################################
#  ROC Function
#

MyPerformance <- function(method.object, data, labels){
  # Generates data for ROC plotting.  Uses the output of lda or qda on 
  #   training data to predict and verify on the set of validation data
  # Input:
  #   method.object - an object of class lda or qda
  #   data - a data frame of the validation data set
  #   labels - a vector containing the true labels for the validation data
  # Output:
  #   roc - data frame with the false positive rates and true positive 
  #     rates, and TP/FP counts as its columns
  #   auc - a numeric value with the auc
  #   either can be called with $ on the output
  
  # Extract the posterior probabilities that a data point has class 1
  image.pr <- predict(method.object, type="prob",newdata=data)$posterior[,2]
  
  # Produce an object of class prediction for ROCR
  image.pr <- prediction(image.pr, labels)
  
  # Use ROCR performance method to find TPR and FPR
  roc <- performance(image.pr, "tpr", "fpr")
  
  # Change from S4 class to S3 data frame
  roc <- data.frame(FPR=unlist(roc@x.values),
                    TPR=unlist(roc@y.values),
                    thresh=unlist(roc@alpha.values))
  auc <- unlist(performance(image.pr, "auc")@y.values)
  return(list(roc=roc, auc=auc))
}

###############################################################################
# Cross-Validation Function
#

KFoldCV <- function(splitimages, analysis=qda){
  # Takes the prepartitioned image data set and performs an
  # n-fold cross-validation
  # Input:
  #   splitimages - output of the SplitImage function, a list of data frames
  #     This can also be a list of data frames to be used for cross-validation
  #     e.g. list(image1,image2,image3)
  #   analysis - method to use.  currently tested only for lda and qda
  # Output:
  #   predictions - a data frame with the posterior probabilities
  #     for each fold and the predicted class of each case
  #   roc - a data frame with the roc for each fold
  #   auc - a vector specifying the auc for each roc
  
  n <- length(splitimages) #number of folds
  
  predictions <- data.frame()
  roc <- data.frame()
  auc <- c()
  
  #foreach(i = 1:n, .export=ls(envir=globalenv())) %dopar%{
  for(i in 1:n){
    
    # Build the training set
    training.set <- data.frame()
    
    for(j in which(1:n != i)){
      training.set <- rbind(training.set, splitimages[[j]])
    }
    
    fold <- factor(i, levels = 1:n)
    im <- LabelsFeatures(splitimages[[i]])
    
    # Train the method
    train <- analysis(label ~ NDAI + SD + CORR, 
                      data = filter(training.set, label != 0))
    
    # Append the posterior probabilities
    posterior <- data.frame(MyPosterior(splitimages[[i]], train), fold=fold)
    predictions <- rbind(predictions, posterior)
    
    # Append the ROC curve
    roc.curve <- data.frame(MyPerformance(train, im$features, im$label)$roc,
                            fold=fold)
    roc <- rbind(roc, roc.curve)
    
    # Add AUC to auc vector
    auc[i] <- MyPerformance(train, im$features, im$labels)$auc
    
  }
  
  return(list(predictions=predictions, roc=roc, auc=auc))
}

###############################################################################
# Confusion Function 
#

Confusion <- function(prob, true.label, thresh=.5){
  # Given a probability that a pixel is a cloud and its true label,
  # returns a confusion value
  # Input:
  #   prob - a posterior probability that a pixel is a cloud
  #   true.label - the true label for the pixel
  #   threshold - the probability cutoff for a pixel being a cloud
  # Output:
  #   A confusion value, correctly labeled (true positives and negatives
  #   return this value), false positive, false negative, or unknown
  if (prob>=thresh && true.label==1) {
    return("True Positive")
  } else if (prob<thresh && true.label==-1){
    return("True Negative")
  } else if (prob>=thresh && true.label==-1){
    return("False Positive")
  } else if (prob<thresh && true.label==1){
    return("False Negative")
  } else {
    return("Unknown")
  }
}

###############################################################################
# Image Reconstruction Function
#

ImageReconstruct <- function(deconstructed, nimages, xsplit, ysplit){
  # Reconstructs the image after cross-validation
  # Input:
  #   deconstructed - the prediction data frame output of KFoldCV
  #   nimages - the number of images used
  #   xsplit - the number of partitions of the x-dimension
  #   ysplit - the number of partitions of the y-dimension
  # Output:
  #   A list of nimages data frames with the columns
  #   x, y, posterior, label, fold, confusion
  
  partitions.per.image <- xsplit*ysplit
  output.names <- c()
  output <- list()
  
  for(image.num in 1:nimages){
    lower <- image.num - 1
    upper <- image.num * partitions.per.image
    output[[image.num]] <- filter(deconstructed, 
                                  as.numeric(fold) > lower,
                                  as.numeric(fold) <= upper) %>%
                           rowwise() %>%
                           mutate(confusion=Confusion(prob=posterior,
                                                      true.label=label))
    output.names[image.num] <- paste("image",image.num,sep="")
  }
  names(output) <- output.names
  
  return(output)
}

###############################################################################
###############################################################################
## 
##  Analysis begins here
##

# Get the data for three images
image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

###############################################################################
# -Pairwise EDA- 
# 
# This script generates the ggplot objects containing the scatterplots in our
# EDA section     

corr.ndai.eda <- ggplot() + 
                 geom_jitter(data=filter(image1, label!=0), 
                             aes(x=CORR, y=NDAI, 
                                 group=factor(label), colour=factor(label)), 
                             alpha=0.5) +
                 ggtitle("CORR vs. NDAI EDA") +
                 theme(legend.position="none", aspect.ratio=1)
  

ndai.sd.eda <- ggplot() + 
               geom_jitter(data=filter(image1, label!=0), 
                           aes(x=NDAI, y=SD, 
                               group=factor(label), colour=factor(label)), 
                           alpha=0.5) +
               ggtitle("NDAI vs. SD EDA") +
               theme(legend.position="none", aspect.ratio=1)

corr.sd.eda <- ggplot() + 
               geom_jitter(data=filter(image1, label!=0), 
                           aes(x=SD, y=CORR, 
                               group=factor(label), colour=factor(label)), 
                           alpha=0.5) +
               ggtitle("CORR vs. SD EDA") +
               theme(legend.position="none", aspect.ratio=1)

###############################################################################
# -Cross-validation-
#
# This section generates everything to do with cross-validation of LDA and QDA

# Each image is left alone and is cross-validated in a leave-one-out CV (LOOCV)
images.loo <- c(SplitImage(image1,1,1),
                SplitImage(image2,1,1),
                SplitImage(image3,1,1))

lda.CV.loo <- KFoldCV(images.loo, analysis=lda)
qda.CV.loo <- KFoldCV(images.loo, analysis=qda)

# Each image is split into 4 horizontal bands resulting in 12 folds
images.4x1 <- c(SplitImage(image1,1,4),
                SplitImage(image2,1,4),
                SplitImage(image3,1,4))

lda.CV.12folds <- KFoldCV(images.4x1, analysis=lda)
qda.CV.12folds <- KFoldCV(images.4x1, analysis=qda)

# ROC for LOOCV 
lda.ROC.loo <- ggplot(lda.CV.loo$roc) +
  geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
  scale_colour_discrete(name  ="Image Left Out") +
  ggtitle("ROC for One-Image-Left-Out CV of LDA")

qda.ROC.loo <- ggplot(qda.CV.loo$roc) +
  geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
  scale_colour_discrete(name  ="Image Left Out") +
  ggtitle("ROC for One-Image-Left-Out CV of QDA")

# ROC for 12-Fold CV
roc.aes <- list(geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)),
                scale_colour_discrete(name  ="Fold"),
                theme(aspect.ratio=1))

qda.ROC.12folds <- ggplot(qda.CV.12folds$roc) + roc.aes +
  ggtitle("ROC for QDA 12-Fold CV of QDA")

# AUC for 12-Fold CV
auc.12folds <- data.frame(LDA=lda.CV.12folds$auc,
                          QDA=qda.CV.12folds$auc,
                          Fold=1:12) %>% 
               melt(id="Fold", variable.name="Method",value.name="AUC") %>%
               ggplot() + 
                 ggtitle("AUC for 12-fold CV of LDA and QDA") + 
                 geom_point(aes(x=Fold, y=AUC, size=3, 
                                group=Method,colour=Method)) +
                 scale_y_continuous(limits=c(0, 1)) +
                 scale_x_discrete() +
                 theme(aspect.ratio=1) +
                 guides(size=FALSE)

# Comparison between the AUC for LDA and QDA at each fold
auc.comp.plot <- data.frame(diff=qda.CV.12folds$auc-lda.CV.12folds$auc,
                            fold=1:12) %>%
                 ggplot() +
                   geom_point(aes(x=fold, y=diff, size=3)) + 
                   scale_y_continuous(limits=c(-.25,.25)) +
                   scale_x_discrete() +
                   labs(x="Fold", y="Difference") +
                   ggtitle("Difference between AUC for QDA and LDA") +
                   theme(aspect.ratio=1) +
                   guides(size=FALSE)


# Reconstruct the images from their split form
qda.recon.12fold <- ImageReconstruct(deconstructed=qda.CV.12folds$predictions, 
                                     nimages=3, xsplit=1, ysplit=4)
qda.recon.loo <- ImageReconstruct(deconstructed=qda.CV.loo$predictions, 
                                  nimages=3, xsplit=1, ysplit=1)

###############################################################################
# -Probability Plots-  
# 
# Each pixel is colored based on the probability of its being a cloud
# 
prob.aes <- list(geom_point(aes(x=x, y=y, colour=posterior)),
                 theme(aspect.ratio=1),
                 scale_color_continuous(name="Probability", 
                                        low="#3366CC", 
                                        high="#FFFFFF"))

qda1.prob.plot <- ggplot(qda.recon.12fold$image1) + prob.aes +
  ggtitle("Probability of Being a Cloud Image 1")
qda2.prob.plot <- ggplot(qda.recon.12fold$image2) + prob.aes +
  ggtitle("Probability of Being a Cloud Image 2")
qda3.prob.plot <- ggplot(qda.recon.12fold$image3) + prob.aes +
  ggtitle("Probability of Being a Cloud Image 3")
  
###############################################################################
# - Confusion Plots - 
#
# This section will generate the ggplot objects that map individual pixels
# as true positive, true negative, false positive, false negative, or unknown

# Aesthetics
ggconf <- list(geom_point(aes(x=x, y=y, group=confusion, colour=confusion)),
          scale_colour_manual(values = c("True Positive" = "white",
                                         "False Positive" = "black",
                                         "True Negative" = "#3366FF", 
                                         "False Negative" = "#FF00CC",
                                         "Unknown" = "#999999")),
          theme(legend.position="none", aspect.ratio=1))

qda1.loo.conf <- ggplot(qda.recon.loo$image1) + ggconf +
  ggtitle("Confusion Plot for Image 1")
qda2.loo.conf <- ggplot(qda.recon.loo$image2) + ggconf +
  ggtitle("Confusion Plot for Image 2")
qda3.loo.conf <- ggplot(qda.recon.loo$image3) + ggconf +
  ggtitle("Confusion Plot for Image 3")

qda1.12fold.conf <- ggplot(qda.recon.12fold$image1) + ggconf +
  ggtitle("Confusion Plot for Image 1")
qda2.12fold.conf <- ggplot(qda.recon.12fold$image2) + ggconf +
  ggtitle("Confusion Plot for Image 2")
qda3.12fold.conf <- ggplot(qda.recon.12fold$image3) + ggconf +
  ggtitle("Confusion Plot for Image 3")

###############################################################################
# - Image Files  -
#
# If you want to save the images, ImageSave to TRUE

ImageSave <- FALSE

if (ImageSave){

# EDA
ggsave(filename="CORR_vs_NDAI.png", plot=corr.ndai.eda,
       height=5, width=5)
ggsave(filename="NDAI_vs_SD.png", plot=ndai.sd.eda,
       height=5, width=5)
ggsave(filename="CORR_vs_SD.png", plot=corr.sd.eda,
       height=5, width=5)

# ROC Curves + AUC
ggsave(filename="ROC_12_folds_DA.png", plot=qda.ROC.12folds,
       height=5, width=5)
ggsave(filename="AUC_12_folds_DA.png", plot=auc.12folds,
       height=5, width=5)
ggsave(filename="AUC_difference.png",plot=auc.comp.plot,
       height=5, width=5)

# Confusion Plots - Leave One Out CV
ggsave(filename="qda1_loo_conf.png", plot=qda1.loo.conf,
       height=5, width=5)
ggsave(filename="qda2_loo_conf.png", plot=qda2.loo.conf,
       height=5, width=5)
ggsave(filename="qda3_loo_conf.png", plot=qda3.loo.conf,
       height=5, width=5)

# Confusion Plots - 12-Fold CV
ggsave(filename="qda1_12fold_conf.png", plot=qda1.12fold.conf,
       height=5, width=5)
ggsave(filename="qda2_12fold_conf.png", plot=qda2.12fold.conf,
       height=5, width=5)
ggsave(filename="qda3_12fold_conf.png", plot=qda3.12fold.conf,
       height=5, width=5)

# Probability Plots
ggsave(filename="qda1_prob.png", plot=qda1.prob.plot,
       height=5, width=5)
ggsave(filename="qda2_prob.png", plot=qda2.prob.plot,
       height=5, width=5)
ggsave(filename="qda3_prob.png", plot=qda3.prob.plot,
       height=5, width=5)
}
