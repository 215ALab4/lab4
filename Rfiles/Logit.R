setwd('~/STAT215A')
library(ggplot2)
library(dplyr)
library(AUC)
library(gridExtra)

# Flag for whether or not we want to save images
ImageSave <- FALSE

# Load the images
image1 <- read.table('image1.txt', header = F)
image2 <- read.table('image2.txt', header = F)
image3 <- read.table('image3.txt', header = F)

# Give the columns the appropriate labels
column.labels <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- column.labels
names(image2) <- column.labels
names(image3) <- column.labels

# Add label to each image so we can combine them to work on one data set
image1 <- mutate(image1, Image=1)
image2 <- mutate(image2, Image=2)
image3 <- mutate(image3, Image=3)

# Combine
all.images <- rbind(image1, image2, image3)

# Plot unlabelled images according to AN radiance
raw.eda <- ggplot(all.images, aes(x=x, y=y, color = AN)) + 
  geom_point() + 
  facet_wrap(~ Image) +
  ggtitle('Unlabelled Images with AN Radiances') +
  scale_color_continuous(name='AN', low = '#3366CC', high = '#FFFFFF' )

# Plot images with their expert labels
experts.eda <- ggplot(all.images, aes(x=x, y=y, color = factor(label))) + 
  geom_point() + facet_wrap(~ Image) +
  ggtitle('Expertly Labelled Images')+ 
  scale_color_discrete(name='',breaks=c("-1", "0", "1"),
                       labels=c("Clear", "Unknown", "Cloudy"))

# Plot images with NDAI
ndai.eda <- ggplot(all.images, aes(x=x, y=y, color = NDAI)) + 
  geom_point() + 
  facet_wrap(~ Image) +
  ggtitle('Mapped NDAI Readings') + 
  scale_color_continuous(name='NDAI', low = '#3366CC', high = '#FFFFFF')

# Plot images with SD
sd.eda <- ggplot(all.images, aes(x=x, y=y, color = SD)) + 
  geom_point() + 
  facet_wrap(~ Image) + 
  ggtitle('Mapped SD Readings') + 
  scale_color_continuous(name='SD', low = '#3366CC', high = '#FFFFFF')

# Plot images with CORR
corr.eda <- ggplot(all.images, aes(x=x, y=y, color = CORR)) + 
  geom_point() + 
  facet_wrap(~ Image) + 
  ggtitle('Mapped CORR Readings') + 
  scale_color_continuous(name='CORR', low = '#3366CC', high = '#FFFFFF' )

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
image1.labelled <- filter(image1, label != 0)
image2.labelled <- filter(image2, label != 0)
image3.labelled <- filter(image3, label != 0)
labelled.data <- rbind(image1.labelled, image2.labelled, image3.labelled)

# Changes coding of -1 for clear to 0 for clear for labelled pixels. 
# Column 3 is expert label
bin.data <- mutate(labelled.data, label=(label+1)/2)

# Set cutoff, above which pixel is considered cloudy
threshold <- .38

# Initialize storage matrices for coefficients and prediction error
coeff <- rep(0,4)
coeff.folds <- matrix(0, 12, 4)
coeff.running.avg <- matrix(0, 12, 4)
prediction.error <- 0

# Perform CV
for (i in 1:12){  
  cv.data <- filter(bin.data, assignment != i)
  test.data <- filter(bin.data, assignment == i)
  
  logit <- glm(label ~ NDAI + SD + CORR, data = cv.data, family = "binomial") 
  coeff <- coeff + logit$coefficients
  coeff.folds[i,] <- logit$coefficients
  coeff.running.avg[i,] <- coeff/i
  misclassification <- sum(abs((predict(logit, test.data, type = 'response') >=
                                  threshold) - test.data$label))
  prediction.error <- prediction.error + misclassification
}

coeff <- coeff/12
prediction.error <- prediction.error/nrow(bin.data)
logit$coefficients <- coeff

# Output prediction probabilities for expertly-labelled pixels so we can 
# calculate empirical ROC curve
prediction <- predict(logit, bin.data, type = 'response')
roc.logit <- roc(predictions = prediction, labels = factor(bin.data$label))
plot(roc.logit, main = "ROC Curve, Logit Trained via 12-Fold CV")
area.under.curve <- auc(roc.logit, 0, 1)

# Prediction probabilities and classifications for all pixels
full.predict <-  predict(logit, all.images, type ='response')
class <- factor((full.predict >= threshold)*1)

# Plot classifications
images.logit.class <- ggplot(all.images, aes(x=x, y=y, color = class)) +
  geom_point() + facet_wrap(~ Image) +
  ggtitle('Logit Classification') + 
  scale_color_manual(name='', 
                     values = c('0' = '#3366CC', '1' = '#FFFFFF'),
                     labels=c("Clear", "Cloudy"))

# Plot probabilities
images.prob <- ggplot(all.images, aes(x=x, y=y, color = full.predict)) + 
  geom_point() + 
  facet_wrap(~ Image) +
  ggtitle('Logit Probabilities') + 
  scale_color_continuous(name="P(Cloud)", low="#3366CC", high="#FFFFFF")

# Generate vector with classification outcomes
correct <- all.images$label
correct[all.images$label == 0] <- "Unknown"
correct[(all.images$label == 1 & class == 1)] <- "True Positive"
correct[(all.images$label == -1 & class == 0)] <- "True Negative"
correct[all.images$label == 1 & class == 0] <- "False Negative"
correct[all.images$label == -1 & class == 1] <- "False Positive"

misclassification.plot <- ggplot(all.images) + 
  geom_point(aes(x = x, y = y, color = factor(correct))) + 
  ggtitle('Confusion Plot') + 
  scale_colour_manual(name = '', 
                      values = c("True Positive" = "white", 
                                 "False Positive" = "black", 
                                 "True Negative" = "#3366FF", 
                                 "False Negative" = "#FF00CC", 
                                 "Unknown" = "#999999")) + 
  facet_wrap(~ Image)

# Generate vector with misclassification rate for each threshold from .01 to 1
error.vec <- sapply(seq(.01,1, length=100),
                    function(x) sum((prediction >= x)*1 != bin.data$label)/nrow(bin.data))

# Plot misclassification rates against thresholds to find minimum
plot(c(1:100)/100, 
     error.vec, xlab = 'Threshold', 
     ylab = 'Misclassification Rate', 
     main = 'Choice of Logit Threshold')
plot(c(33:43)/100, 
     error.vec[33:43], 
     xlab = 'Threshold', 
     ylab = 'Misclassification Rate')

# Make plots of parameter values as CV progresses
intercept.coeff <- data.frame(Value=coeff.running.avg[,1], Variable=1, Index=c(1:12))
ndai.coeff <- data.frame(Value=coeff.running.avg[,2], Variable=2, Index=c(1:12))
sd.coeff <- data.frame(Value=coeff.running.avg[,3], Variable=3, Index=c(1:12))
corr.coeff <- data.frame(Value=coeff.running.avg[,4], Variable=4, Index=c(1:12))
coeff.conv <- rbind_list(intercept.coeff, ndai.coeff, sd.coeff, corr.coeff)

parameter.convergence <- ggplot(coeff.conv, aes(x = Index, y=Value)) + 
  geom_point() + 
  facet_wrap(~ Variable, scales = 'free_y') + 
  ggtitle('Parameter Convergence in CV')

# Save plots if flag was set to TRUE
if (ImageSave == TRUE){
  ggsave(filename="RAWEDA.png", plot=raw.eda,
         height=5, width=5)
  ggsave(filename="EXPERTSEDA.png", plot=experts.eda,
         height=5, width=5)
  ggsave(filename="NDAIEDA.png", plot=ndai.eda,
         height=5, width=5)
  ggsave(filename="SDEDA.png", plot=sd.ndai.eda,
         height=5, width=5)
  ggsave(filename="CORREDA.png", plot=corr.eda,
         height=5, width=5)
  ggsave(filename="LogitClassification.png", plot=images.logit.class,
         height=5, width=5)
  ggsave(filename="LogitProbabilites.png", plot=images.prob,
         height=5, width=5)
  ggsave(filename="ClassificationOutcomes.png", plot=misclassification.plot,
         height=5, width=5)
  ggsave(filename="ParameterConvergence.png", plot=parameter.convergence,
         height=5, width=5)
}

an.eda <- ggplot(image3) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)
af.eda <- ggplot(image3) + geom_density(aes(x=AF, group=factor(label), fill=factor(label)), alpha=0.5)
bf.eda <- ggplot(image3) + geom_density(aes(x=BF, group=factor(label), fill=factor(label)), alpha=0.5)
cf.eda <- ggplot(image3) + geom_density(aes(x=CF, group=factor(label), fill=factor(label)), alpha=0.5)
df.eda <- ggplot(image3) + geom_density(aes(x=DF, group=factor(label), fill=factor(label)), alpha=0.5)



