setwd('STAT215A')
library(ggplot2)
library(dplyr)
library(AUC)

# Load the images 
image1 <- read.table('image1.txt', header = F)
image2 <- read.table('image2.txt', header = F)
image3 <- read.table('image3.txt', header = F)

# Give the columns the appropriate labels
colnames(image1) <- c('y', 'x', 'label', 'NDAI', 'SD', 'CORR', 'DF', 'CF', 'BF', 'AF', 'AN')
colnames(image2) <- colnames(image1)
colnames(image3) <- colnames(image1)

# Add label to each image so we can combine them to work on one data set
image1 <- cbind(image1, 1)
image2 <- cbind(image2, 2)
image3 <- cbind(image3, 3)
# Column 12 is the column being named 'Image', so don't include its current name in the colnames
colnames(image1) <- c(colnames(image1[,-12]), 'Image')
colnames(image2) <- c(colnames(image2[,-12]), 'Image')
colnames(image3) <- c(colnames(image3[,-12]), 'Image')
# Combine
all_images <- rbind(image1, image2, image3)

# Plot unlabelled images according to AN radiance
RAW.EDA <- ggplot(all_images, aes(x=x, y=y, color = AN)) + geom_point() + facet_wrap(~ Image) +
  ggtitle('Unlabelled Images with AN Radiances')+scale_color_continuous(name='AN', low = '#3366CC', high = '#FFFFFF' )

# Plot images with their expert labels
EXPERTS.EDA <- ggplot(all_images, aes(x=x, y=y, color = factor(label))) + geom_point() + facet_wrap(~ Image) +
  ggtitle('Expertly Labelled Images')+ 
  scale_color_discrete(name='',breaks=c("-1", "0", "1"),labels=c("Clear", "Unknown", "Cloudy"))

# Plot images with NDAI
NDAI.EDA <- ggplot(all_images, aes(x=x, y=y, color = NDAI)) + geom_point() + facet_wrap(~ Image) +
  ggtitle('Mapped NDAI Readings') + scale_color_continuous(name='NDAI', low = '#3366CC', high = '#FFFFFF' )

# Plot images with SD
SD.EDA <- ggplot(all_images, aes(x=x, y=y, color = SD)) + geom_point() + facet_wrap(~ Image) + 
  ggtitle('Mapped SD Readings') + scale_color_continuous(name='SD', low = '#3366CC', high = '#FFFFFF' )

# Plot images with CORR
CORR.EDA <- ggplot(all_images, aes(x=x, y=y, color = CORR)) + geom_point() + facet_wrap(~ Image) + 
  ggtitle('Mapped CORR Readings') + scale_color_continuous(name='CORR', low = '#3366CC', high = '#FFFFFF' )

# Create the folds for CV by dividing each image into quadrants
image1$assignment <- 1 + (image1$x < mean(image1$x) & image1$y > mean(image1$y)) + 
  3*(image1$y < mean(image1$y) & image1$x > mean(image1$x)) + 
  2*((image1$x > mean(image1$x)) & (image1$y > mean(image1$y)))

image2$assignment <- 5 + (image2$x < mean(image2$x) & image2$y > mean(image2$y)) + 
  3*(image2$y < mean(image2$y) & image2$x > mean(image2$x)) + 
  2*((image2$x > mean(image2$x)) & (image2$y > mean(image2$y)))

image3$assignment <- 9 + (image3$x < mean(image3$x) & image3$y > mean(image3$y)) + 
  3*(image3$y < mean(image3$y) & image3$x > mean(image3$x)) + 
  2*((image3$x > mean(image3$x)) & (image3$y > mean(image3$y)))

# Only take expertly labelled pixels for training
image1_labelled <- filter(image1, label != 0)
image2_labelled <- filter(image2, label != 0)
image3_labelled <- filter(image3, label != 0)
labelled_data <- rbind(image1_labelled, image2_labelled, image3_labelled)

bin_data <- labelled_data
# Changes coding of -1 for clear to 0 for clear for labelled pixels. Column 3 is expert label
bin_data[bin_data$label == -1, 3] <- 0

# Set cutoff, above which pixel is considered cloudy
threshold <- .38

# Initialize storage matrices for coefficients and prediction error
coeff <- rep(0,4)
co <- matrix(0, 12, 4)
co2 <- matrix(0, 12, 4)
prediction_error <- 0
# Perform CV
for (i in 1:12){
  CV_data <- filter(bin_data, assignment != i)
  test_data <- filter(bin_data, assignment == i)
  logit <- glm(label ~ NDAI + SD + CORR, data = CV_data, family = "binomial") 
  coeff <- coeff + logit$coefficients
  co[i,] <- logit$coefficients
  co2[i,] <- coeff/i
  misclassification <- sum(abs((predict(logit, test_data, type = 'response') >= threshold) - test_data$label))
  prediction_error <- prediction_error + misclassification
}
coeff <- coeff/12
prediction_error <- prediction_error/nrow(bin_data)
logit$coefficients <- coeff

# Output prediction probabilities for expertly-labelled pixels so we can 
# calculate empirical ROC curve
prediction <- predict(logit, bin_data, type = 'response')
roc_logit <- roc(predictions = prediction, labels = factor(bin_data$label))
plot(roc_logit, main = "ROC Curve, Logit Trained via 12-Fold CV")
area_under_curve <- auc(roc_logit, 0, 1)

# Prediction probabilities and classifications for all pixels
full_predict <-  predict(logit, all_images, type ='response')
class <- factor((full_predict >= threshold)*1)
# Plot classifications
images_logit_class <- ggplot(all_images, aes(x=x, y=y, color = class)) + geom_point() + facet_wrap(~ Image) +
  ggtitle('Logit Classification')+ 
  scale_color_manual(name='', values = c('0' = '#3366CC', '1' = '#FFFFFF') ,labels=c("Clear", "Cloudy"))

# Plot probabilities
images_prob <- ggplot(all_images, aes(x=x, y=y, color = full_predict)) + geom_point() + facet_wrap(~ Image) +
  ggtitle('Logit Probabilities') + scale_color_continuous(name="P(Cloud)", low="#3366CC", high="#FFFFFF")

# Generate vector with classification outcomes
correct <- all_images$label
correct[all_images$label == 0] <- "Unknown"
correct[(all_images$label == 1 & class == 1)] <- "True Positive"
correct[(all_images$label == -1 & class == 0)] <- "True Negative"
correct[all_images$label == 1 & class == 0] <- "False Negative"
correct[all_images$label == -1 & class == 1] <- "False Positive"
misclassification.plot <- ggplot(all_images, aes(x = x, y = y, color = factor(correct))) + 
  geom_point() + ggtitle('Confusion Plot') + 
  scale_colour_manual(name = '', values = c("True Positive" = "white", "False Positive" = "black", "True Negative" = "#3366FF", 
                                 "False Negative" = "#FF00CC", "Unknown" = "#999999")) + facet_wrap(~ Image)

# Plot outcomes against feature values
plot(image1$NDAI, correct1)
plot(image1$SD, correct1)
plot(image1$CORR, correct1)

plot(image2$NDAI, correct2)
plot(image2$SD, correct2)
plot(image2$CORR, correct2)

plot(image3$NDAI, correct3)
plot(image3$SD, correct3)
plot(image3$CORR, correct3)

features <- rbind(image1[,4:6], image2[,4:6], image3[,4:6])
plot3d(features$NDAI[correct > 1], features$SD[correct > 1], features$CORR[correct > 1], 
       col = factor(correct[correct > 1]), xlab = 'NDAI', ylab = 'SD', zlab = 'CORR')

# Generate vector with misclassification rate for each threshold from .01 to 1
error_vec <- rep(0, 1, 100)
for(i in 1:100){
  error_vec[i] <- sum((prediction >= i/100)*1 != bin_data$label)
}
error_vec <- error_vec/nrow(bin_data)

# Plot misclassification rates against thresholds to find minimum
plot(c(1:100)/100, error_vec, xlab = 'Threshold', ylab = 'Misclassification Rate', main = 'Choice of Logit Threshold')
plot(c(33:43)/100, error_vec[33:43], xlab = 'Threshold', ylab = 'Misclassification Rate')

# make plots of parameter values as CV progresses
intercept.coeff <- cbind(co2[,1], 1, c(1:12))
ndai.coeff <- cbind(co2[,2], 2, c(1:12))
sd.coeff <- cbind(co2[,3], 3, c(1:12))
corr.coeff <- cbind(co2[,4], 4, c(1:12))
coeff.conv <- rbind(intercept.coeff, ndai.coeff, sd.coeff, corr.coeff)
colnames(coeff.conv) <- c('Value', 'Variable', 'Index')

parameter_convergence <- ggplot(data.frame(coeff.conv), aes(x = Index, y=Value)) + geom_point() + 
  facet_wrap(~ Variable, scales = 'free_y') + ggtitle('Parameter Convergence in CV')

