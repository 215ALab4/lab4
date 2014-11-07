library(ggplot2)
library(MASS)
library(ROCR)
library(xtable)
library(cvTools)


setwd("~/Desktop/Student_HS/Class/2014 Fall/215 Applied statistics/Lab4/image_data")
## READ IN DATA ##
all.image1 <- read.table('image1.txt', header = FALSE)
all.image2 <- read.table('image2.txt', header = FALSE)
all.image3 <- read.table('image3.txt', header = FALSE)

names(all.image1) <- names(all.image2) <- names(all.image3) <- 
  c('y', 'x', 'label', 'NDAI', 'SD', 'CORR', 'DF', 'CF', 'BF', 'AF', 'AN')
#    expert label (+1 = cloud, -1 = not cloud, 0 unlabeled)

# remove points that are unlabelled 
image1 <- all.image1[all.image1$label != 0, ]
image2 <- all.image2[all.image2$label != 0, ]
image3 <- all.image3[all.image3$label != 0, ]


# Divide each image into 2 by 2 grid and define "which" as a number between 1-12 that shows
# which grid the point belongs to. 

image1$which <- 1 + (image1$x < mean(all.image1$x) & image1$y > mean(all.image1$y)) + 
  3*(image1$y < mean(all.image1$y) & image1$x > mean(all.image1$x)) + 2*((image1$x > mean(all.image1$x)) & 
                                                                                     (image1$y > mean(all.image1$y)))

image2$which <- 5 + (image2$x < mean(all.image2$x) & image2$y > mean(all.image2$y)) + 
  3*(image2$y < mean(all.image2$y) & image2$x > mean(all.image2$x)) + 2*((image2$x > mean(all.image2$x)) & 
                                                                           (image2$y > mean(all.image2$y)))

image3$which <- 9 + (image3$x < mean(all.image3$x) & image3$y > mean(all.image3$y)) + 
  3*(image3$y < mean(all.image3$y) & image3$x > mean(all.image3$x)) + 2*((image3$x > mean(all.image3$x)) & 
                                                                           (image3$y > mean(all.image3$y)))
                                                                           

images <- rbind(image1, image2, image3)

# Scale 'NDAI', 'SD', 'CORR', 'DF', 'CF', 'BF', 'AF', 'AN' 
# so that all these feature has mean 0 and SD 1.
sub.images <- subset(images, select=c('NDAI', 'SD', 'CORR', 'DF', 'CF', 'BF', 'AF', 'AN'))
scale.images <- scale(sub.images)
scale.images <- cbind(subset(images,select=c('y','x','label','which')),scale.images) 


############ head(scale.images)
####y  x label which       NDAI         SD       CORR            DF        CF        BF
####1 373 65    -1     2 -0.5814336 -0.5077822 -0.7581418 -0.0008502838 0.3320268 0.6179521
####2 374 65    -1     2 -0.5655205 -0.4916902 -0.6846132  0.0150363415 0.3705504 0.6242670
####3 375 65    -1     2 -0.4070858 -0.4381379 -0.6745827  0.0684032124 0.3757591 0.6167337
####4 376 65    -1     2 -0.6340552 -0.3683732 -0.7355432  0.1157845794 0.3453642 0.5838297
####5 377 65    -1     2 -0.8957245 -0.2597136 -0.7805293  0.0913942554 0.2929308 0.5844392
####6 378 65    -1     2 -1.2729702 -0.2236030 -0.7807401  0.0266716982 0.2674589 0.5720308
####AF        AN
####1 0.7432263 0.7587438
####2 0.7630284 0.7440598
####3 0.7362139 0.7666229
####4 0.7447408 0.7901407
####5 0.7549507 0.8061971
####6 0.7631966 0.8081668


#########Check assumption under LDA:
######## QQ plot of each column to see if each feature is approximately normally distributed.
qqnorm(scale.images$NDAI); qqline(scale.images$NDAI, col=2)
qqnorm(scale.images$SD); qqline(scale.images$SD, col=2)
qqnorm(scale.images$CORR); qqline(scale.images$CORR, col=2)
qqnorm(scale.images$AN); qqline(scale.images$AN, col=2)

##########################################################################
## LINEAR DISCRIMINANT ANALYSIS(LDA) on four features NDAI, CORR, SD, AF ##
lda.model <- lda(factor(label) ~ NDAI + CORR + SD + AF, 
                 data = scale.images)

##### LDA gives results as follows.
##Coefficients of linear discriminants:
########  LD1
###NDAI  1.50147775
###CORR  0.48241495
###SD   -0.22667405
###AF    0.09046505
###
###Observation: We could see three features NDAI, CORR, SD have the largest coefficients.



#############################################################################
### LDA on four feature NDAI, CORR, SD, AN

lda.model<- lda(factor(label) ~ NDAI + CORR + SD + AN, data= scale.images)

##### LDA gives results as follows.

##Coefficients of linear discriminants:
######  LD1
###NDAI  1.5195279
###CORR  0.5457833
###SD   -0.2159440
###AN    0.1850849
###############################################################################


###################################################################################
############LDA on all eight features

lda.model<- lda(factor(label) ~ NDAI + CORR + SD + AN + AF + DF + CF +BF, data= scale.images)
########Coefficients of linear discriminants:
#####  LD1
#####NDAI  1.4569010
#####CORR  0.3604571
####SD   -0.2359463
####AN    0.8388941
####AF   -0.6214124
####DF    0.4701681
####CF   -0.1578081
####BF   -0.3738148





#################################################################################
### Cross Validation ############################################################


################################################################################

## Functions for LDA on three features NDAI, CORR, SD(NCS) with leaving one group test.num out.
ncs.lda <- function(test.num){
  lda(factor(label) ~ NDAI + CORR +SD, data = scale.images[scale.images$which!=test.num, ])
}  


## Functions for LDA on three features NDAI, CORR, AN (NCN) with leaving one group test.num out. 
ncn.lda <- function(test.num){
  lda(factor(label) ~ NDAI + CORR + AN, data=scale.images[scale.images$which!=test.num, ])
}


## Functions for LDA on three features NDAI, CORR, AF(NCF) with leaving one group test.num out.
ncf.lda <- function(test.num){
  lda(factor(label) ~ NDAI + CORR + AF, data= scale.images[scale.images$which!=test.num, ])
}

####################################################################################

## Functions for getting the predicted classication
pred.ncs <- function(test.num, cutoff){
  2 * (as.numeric(predict(ncs.lda(test.num), newdata = scale.images[scale.images$which==test.num, ])$posterior[ ,2] > 
                    cutoff)) - 1
}


pred.ncn <- function(test.num, cutoff){
  2 * (as.numeric(predict(ncn.lda(test.num), newdata = scale.images[scale.images$which==test.num, ])$posterior[ ,2] > 
                    cutoff)) - 1
}

pred.ncf <- function(test.num, cutoff){
  2 * (as.numeric(predict(ncf.lda(test.num), newdata = scale.images[scale.images$which==test.num, ])$posterior[ ,2] > 
                    cutoff)) - 1
}
##### Function for calculating a misclassification rate of LDA with prediction function pred on group (test.num)
misclass_rate <- function(pred , test.num, cutoff){
  1 - sum(scale.images[scale.images$which == test.num,]$label* pred(test.num, cutoff)== 1)/length(scale.images[scale.images$which == test.num,]$label)
}

######################################################################################
##############CV function: Print a 3 by 11 matrix that gives misclassification rates
########################## rates of NCS, NCN, NCF.  ################

do_cv <- function(cutoff) {
  misclass_rates1 <- vector(length= 13, mode = 'numeric')
  misclass_rates2 <- vector(length= 13, mode = 'numeric')
  misclass_rates3 <- vector(length= 13, mode = 'numeric')
  for (i in 1:12){
    misclass_rates1[i] <- misclass_rate(pred.ncs, i, cutoff)
  }

  misclass_rates1[13] <- mean(misclass_rates1[1:12])

  for (i in 1:12){
  misclass_rates2[i] <- misclass_rate(pred.ncn, i, cutoff)
  }

  misclass_rates2[13] <- mean(misclass_rates2[1:12])
 
  for (i in 1:12){
  misclass_rates3[i] <- misclass_rate(pred.ncf, i, cutoff)
  }

  misclass_rates3[13] <- mean(misclass_rates3[1:12])
  

  total.misclass_rates <- rbind(misclass_rates1, misclass_rates2, misclass_rates3)
  row.names(total.misclass_rates) <- c('NCS','NCN','NCF')
  colnames(total.misclass_rates) <- c('grid1','grid2','grid3','grid4','grid5'
                                      ,'grid6','grid7' ,'grid8', 'grid9','grid10','grid11','grid12','average')
  total.misclass_rates
} 

#prob.cutoffs <- seq(0, 1, by = 0.05) 
#lda.cv <- sapply(prob.cutoffs, cv <- function(x){do_cv(x)})

do_cv(0.5)
####
#RESULT OF DO_CV(0.5)
####grid1      grid2      grid3      grid4       grid5      grid6       grid7
#NCS 0.2667811 0.08698633 0.02248978 0.07247064 0.016796665 0.04520163 0.002208281
#NCN 0.3043926 0.08803767 0.01385734 0.07810461 0.003631711 0.03916062 0.002358846
#NCF 0.2931366 0.08652923 0.01357338 0.08219666 0.004333292 0.04562556 0.002358846
####grid8     grid9    grid10     grid11    grid12   average
#NCS 0.1934522 0.2190549 0.4586139 0.04831243 0.1418014 0.1311808
#NCN 0.1810575 0.2091876 0.5754218 0.05839307 0.1180251 0.1393024
#NCF 0.1821034 0.2122574 0.5541314 0.05704898 0.1233266 0.1380518
#On average, NCS gives the lowest misclassification rate. 


########Cross Validation 
# Train on two image files and test on the other one image file.


size.images <- list(1:nrow(image1), (nrow(image1)+1):(nrow(image1)+nrow(image2)), (nrow(image1)+nrow(image2)+1): (nrow(image1)+nrow(image2)+nrow(image3)))
lda.images.ncs<- function(image.num){
  lda(factor(label)~NDAI+CORR+SD, data=scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),])
} #NCS: Train on two image files except for the (image.num)th file.


pred.images.ncs <- function(image.num){
  2*as.numeric(predict(lda.images.ncs(image.num),newdata= scale.images[size.images[[image.num]],])$posterior[,2] > 0.5)-1
} #NCS: Test on the (image.num)th file and derive predicted label.

lda.images.ncn<- function(image.num){
  lda(factor(label)~NDAI+CORR+AN, data=scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),])
} #NCN: Train on two image files except for the (image.num)th file.

pred.images.ncn <- function(image.num){
  2*as.numeric(predict(lda.images.ncn(image.num),newdata= scale.images[size.images[[image.num]],])$posterior[,2] > 0.5)-1
} #NCN: Test on the (image.num)th file and derive predicted label.

lda.images.ncf<- function(image.num){
  lda(factor(label)~NDAI+CORR+AF, data=scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),])
} #NCN: Train on two image files except for the (image.num)th file.


pred.images.ncf <- function(image.num){
  2*as.numeric(predict(lda.images.ncf(image.num),newdata= scale.images[size.images[[image.num]],])$posterior[,2] > 0.5)-1
} #NCF: Test on the (image.num)th file and derive predicted label.




#################################
#Input
#pred: prediction method: one of pred.images.ncs, pred.images.ncn, and pred.images.ncf
#image.num: the integer between 1 and 3 that shows which file we test in LDA.
#Result
#returns the misclassification rate of predicted label from LDA and actual expert label.

misclass_rate.images <- function(pred , image.num){
  1 - sum(scale.images[size.images[[image.num]],]$label* pred(image.num)== 1)/length(size.images[[image.num]])
} 



#Result
#function that returns a matrix whose elements are misclassification rates 
#depending on LDA method and image file that is tested.

do_cv.images2 <- function() {
  misclass_rates1 <- vector(length= 4, mode = 'numeric')
  misclass_rates2 <- vector(length= 4, mode = 'numeric')
  misclass_rates3 <- vector(length= 4, mode = 'numeric')
  for (i in 1:3){
    misclass_rates1[i] <- misclass_rate.images(pred.images.ncs, i)
  }
  
  misclass_rates1[4] <- mean(misclass_rates1[1:3])
  
  for (i in 1:3){
    misclass_rates2[i] <- misclass_rate.images(pred.images.ncn, i)
  }
  
  misclass_rates2[4] <- mean(misclass_rates2[1:3])
  
  for (i in 1:3){
    misclass_rates3[i] <- misclass_rate.images(pred.images.ncf, i)
  }
  
  misclass_rates3[4] <- mean(misclass_rates3[1:3])
  
  
  total.misclass_rates <- rbind(misclass_rates1, misclass_rates2, misclass_rates3)
  row.names(total.misclass_rates) <- c('NCS','NCN','NCF')
  colnames(total.misclass_rates) <- c('image1','image2','image3','average')
  total.misclass_rates
} 


do_cv.images2()




##### Misclassification rates on three image files and their average
######image1     image2    image3   average
#NCS 0.1000888 0.06407947 0.1937232 0.1192972
#NCN 0.1136822 0.07269806 0.2250891 0.1371564
#NCF 0.1124695 0.07095730 0.2175976 0.1336748
################################################################################


####Cross Validation
#this time, we train on one image file, test on the other two image files,
#and calculate misclassification rate
#in order to distinguish from previous CV and clarify that we are training 
#on one image file, insert 1 at the end of every variable and function names. 



##Input
#image.num: a integer between 1 and 3 that shows which image file is trained.
lda.images.ncs1<- function(image.num){
  lda(factor(label)~NDAI+CORR+SD, data=scale.images[size.images[[image.num]],])
} #train NCS on the (image.num)th file.


pred.images.ncs1 <- function(image.num){
  2*as.numeric(predict(lda.images.ncs(image.num),newdata= scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),])$posterior[,2] > 0.5)-1
} #test NCS on the other two image files.

lda.images.ncn1<- function(image.num){
  lda(factor(label)~NDAI+CORR+AN, data=scale.images[size.images[[image.num]],])
} #train NCN on the (image.num)th file.


pred.images.ncn1 <- function(image.num){
  2*as.numeric(predict(lda.images.ncn(image.num),newdata= scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),])$posterior[,2] > 0.5)-1
} #test NCN on the other two image files.

lda.images.ncf1<- function(image.num){
  lda(factor(label)~NDAI+CORR+AF, data=scale.images[size.images[[image.num]],])
} #train NCF on the (image.num)th file.


pred.images.ncf1 <- function(image.num){
  2*as.numeric(predict(lda.images.ncf(image.num),newdata= scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),])$posterior[,2] > 0.5)-1
} #test NCF on the other two image files.



#################
#Input
#pred: prediction method: one of pred.images.ncs1, pred.images.ncn1, and pred.images.ncf1
#image.num: the integer between 1 and 3 that shows which file we test in LDA.
#Result
#returns the misclassification rate of predicted label from LDA and actual expert label.

misclass_rate.images1 <- function(pred , image.num){
  1 - sum(scale.images[setdiff(1:nrow(scale.images),size.images[[image.num]]),]$label* pred(image.num)== 1)/length(setdiff(1:nrow(scale.images),size.images[[image.num]]))
}
do_cv.images1 <- function() {
  misclass_rates1 <- vector(length= 4, mode = 'numeric')
  misclass_rates2 <- vector(length= 4, mode = 'numeric')
  misclass_rates3 <- vector(length= 4, mode = 'numeric')
  for (i in 1:3){
    misclass_rates1[i] <- misclass_rate.images1(pred.images.ncs1, i)
  }
  
  misclass_rates1[4] <- mean(misclass_rates1[1:3])
  
  for (i in 1:3){
    misclass_rates2[i] <- misclass_rate.images1(pred.images.ncn1, i)
  }
  
  misclass_rates2[4] <- mean(misclass_rates2[1:3])
  
  for (i in 1:3){
    misclass_rates3[i] <- misclass_rate.images1(pred.images.ncf1, i)
  }
  
  misclass_rates3[4] <- mean(misclass_rates3[1:3])
  
  
  total.misclass_rates <- rbind(misclass_rates1, misclass_rates2, misclass_rates3)
  row.names(total.misclass_rates) <- c('NCS','NCN','NCF')
  colnames(total.misclass_rates) <- c('image1','image2','image3','average')
  total.misclass_rates
} 




#Result
#function that returns a matrix whose elements are misclassification rates 
#depending on LDA method and image file that is tested.
do_cv.images1()

#RESULT OF DO_CV.IMAGES1
#####image1    image2     image3   average
#NCS 0.1076824 0.1305822 0.06964362 0.1026361
#NCN 0.1140553 0.1275484 0.07383791 0.1051472
#NCF 0.1130418 0.1275166 0.07266847 0.1044090
#Observation: NCS is the best.

###################################################################################
#Convergence test
#####################################################################################


