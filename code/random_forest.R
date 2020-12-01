#####
# Random Forest Classification of Mineralized Samples
# 
# version: 1.0 (2020/11/26)
#
# Last modifications:
#
# -----
# 
# 
# 
# -----
# Guilherme Ferreira, (guilherme.ferreira@cprm.gov.br)
# November, 2020
#####


#####
# Setting up the enviroment
#####

setwd('~/GitHub/jacobina/data/pphy')

set.seed(0)

#####
# Import Packages
#####

library(tidyverse) # ggplot2, tidyr, dplyr
library(readxl) # open XLSX data
library(geoquimica) # Data wrangling
library(DMwR) # SMOTE
library(caret) # Machine Learning
library(randomForest) # RF
library(randomForestExplainer) # RF
library(pROC) # ROC and AUC
library(ElemStatLearn)
library(smotefamily)

#####
# Data Preparation
#####

# pXRF data ----
xrf <- read_xlsx(path = '~/GitHub/jacobina/data/xrf/pXRF_Jacobina_SAMPLES.xlsx', sheet = 1)

# Petrophysics data ----
files <- list.files(pattern = '.xlsx$',path = '~/GitHub/jacobina/data/pphy')

phy <- lapply(files, read_xlsx, sheet = 1) %>%
  bind_rows() %>%
  mutate(HOLE = as.factor(HOLE),
         ID = as.factor(ID),
         FROM = as.numeric(FROM),
         TO = as.numeric(TO),
         LITHO = as.factor(LITHO),
         MINERALIZATION = as.factor(MINERALIZATION),
         SUSCEPTIBILITY = as.numeric(SUSCEPTIBILITY),
         CONDUCTIVITY = as.numeric(CONDUCTIVITY),
         DENSITY = as.numeric(DENSITY),
         COMMENTS = as.character(COMMENTS)) %>%
  arrange(ID)

phy <- phy %>%
  mutate(SAMPLE = paste(HOLE,formatC(x = phy$FROM,flag = '0',
                                     width = 6,
                                     digits = 2,
                                     format = 'f'),sep = '-'),
         ROCK = case_when(phy$LITHO %in% c('GRIT','LMPC','LVLPC','MLPC','MPC','MSPC',
                                    'SMPC','SPC','VSPC') ~ 'CONGLOMERATE',
                   phy$LITHO %in% c('QTO','QTO_SX','QZ_VEIN') ~ 'QUARTZITE',
                   phy$LITHO %in% c('ITV','UMF') ~ 'ULTRAMAFIC',
                   phy$LITHO %in% c('BRX') ~ 'BRECCIA',
                   phy$LITHO %in% c('XISTO') ~ 'SCHIST',
                   phy$LITHO %in% c('SOLO') ~ 'SOIL',
                   TRUE ~ as.character(phy$LITHO)))

# Merging dataset ----

df <- phy %>%
  left_join(xrf,by = 'SAMPLE') %>%
  mutate(min = factor(ifelse(test = phy$MINERALIZATION == 1 | phy$MINERALIZATION == 1000,
                             yes = 'ORE', no =  'BARREN')))

df %>%
  select(min, SUSCEPTIBILITY,CONDUCTIVITY, DENSITY, Cu, Fe, Cr, Ti, K, Al, Si, S) %>%
  elem_norm(method = 'clr') %>%
  GGally::ggpairs(mapping=ggplot2::aes(colour = min))


# Filtering for Conglomerate only ----

conglomerate <- as.data.frame(df) %>%
  na.omit() %>%
  filter(ROCK == 'CONGLOMERATE') %>%
  select(min,7:9, Cu, Fe, Cr, Ti, K, Al, Si, S)

# Split data to smote
index <- caret::createDataPartition(conglomerate$min, p =.7,
                                    list = FALSE,
                                    times = 1)

toSmote <- conglomerate[index,]

## Smote
df_smote <- DMwR::SMOTE(form = min ~ .,
                        data = as.data.frame(toSmote),
                        perc.over = 200,
                        perc.under = 150,
                        k = 5)

# On the following lines, we present an alternative code to SMOTE, called borderline-Smote
# fromSmote <- BLSMOTE(dupSize = 0, X = as.data.frame(toSmote[,-1]),K = 5,target = as.data.frame(toSmote[,'min']))
# 
# fromSmote$data %>%
#   rename(min = class) %>%
#   elem_norm(method = 'clr') %>%
#   GGally::ggpairs(mapping=ggplot2::aes(colour = min))



df_smote %>%
  elem_norm(method = 'clr') %>%
  GGally::ggpairs(mapping=ggplot2::aes(colour = min), alpha = .4)


table(conglomerate$min)
table(toSmote$min)
table(df_smote$min)

#####
# Random Forest
#####

# Imbalanced Model

splitIndex <- caret::createDataPartition(conglomerate$min, p =.7,
                                         list = FALSE,
                                         times = 1)


trainSPlit <- conglomerate[splitIndex,]
testSplit <- conglomerate[-splitIndex,]


imbal_minModel <- randomForest(min ~ .,
                         trainSPlit,
                         proximity = TRUE,
                         ntree = 500, localImp = TRUE)


print(imbal_minModel)
plot(imbal_minModel)

par(pty = 's')
plot.roc(trainSPlit$min, imbal_minModel$votes[,1],legacy.axes=TRUE,
         percent = FALSE, print.auc = TRUE,xlab="FALSE POSITIVE",
         ylab = 'TRUE POSITIVE')
par(pty = 'm')



# Balanced Model

splitIndex <- caret::createDataPartition(df_smote$min, p =.7,
                                         list = FALSE,
                                         times = 1)


trainSPlit <- df_smote[splitIndex,]
testSplit <- df_smote[-splitIndex,]

prop.table(table(trainSPlit$min))
prop.table(table(testSplit$min))


ctrl <- caret::trainControl(method = 'cv',
                            number = 10,
                            verboseIter = TRUE)

tg <- data.frame(mtry = seq(2,12, by = 1))

minModel <- randomForest(min ~ .,
                         trainSPlit,
                         proximity = TRUE,
                         ntree = 500, localImp = TRUE)


# minModel <- caret::train(min ~ ., data = trainSPlit, method = "rf",
#                          trControl = ctrl, ntree = 300, importance = TRUE,
#                          tuneGrid = tg, proximity = TRUE)

print(minModel)
plot(minModel)

par(pty = 's')
plot.roc(trainSPlit$min, minModel$votes[,1],legacy.axes=TRUE,
         percent = FALSE, print.auc = TRUE,xlab="FALSE POSITIVE",
         ylab = 'TRUE POSITIVE')
par(pty = 'm')


pred <- predict(minModel, testSplit[,-1])

confusionMatrix(as.factor(testSplit$min), pred,positive = 'ORE')


varImpPlot(minModel,sort = TRUE,scale = TRUE)
varImp(minModel,sort = TRUE,scale = TRUE)


x <- df %>%
  filter(ROCK == 'CONGLOMERATE')

y <- df %>%
  filter(ROCK == 'CONGLOMERATE') %>%
  select(min)

labs <- df %>%
  filter(ROCK == 'CONGLOMERATE') %>%
  select(ID:LITHO)

pred1 <- predict(minModel, x)

confusionMatrix(as.factor(y$min), as.factor(pred1),positive = 'ORE')

auc1 <- roc(as.double(y$min), as.double(pred1))


plot(auc1, ylim = c(0,1), print.thres = TRUE, main = paste('AUC',round(auc1$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)



## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-minModel$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status = trainSPlit$min)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_point(aes(col = Status)) +
  # theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

# Cairo::CairoPDF(file = 'pairs.PDF',width = 40,height = 40)
# df_smote %>%
#   elem_norm(method = 'clr') %>%
#   GGally::ggpairs(mapping=ggplot2::aes(colour = min), alpha = .4)
# dev.off()


min_depth_frame <- min_depth_distribution(minModel)

plot_min_depth_distribution(min_depth_frame,mean_sample = 'relevant_trees')

importance_frame <- randomForestExplainer::measure_importance(minModel)

randomForestExplainer::plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_importance_rankings(importance_frame)

plot_importance_ggpairs(importance_frame)







# # this section creates the background region red/green. It does that by the 'by' which you can think of as the steps in python, so each 0.01 is interpreted as 0 or 1 and is either green or red. The -1 and +1 give us the space around the edges so the dots are not jammed. Another way to think of the 'by' as is as the resolution of the graphing of the background
# X1 = seq(min(set[, 14]) - .01, max(set[, 14]) + .01, by = 0.1)
# X2 = seq(min(set[, 15]) - .01, max(set[, 15]) + .01, by = 0.1)
# 
# grid_set = expand_grid(X1, X2)#, X3, X4, X5)#, X6)#, X7,
#                        # X8, X9, X10, X11, X12)
# # just giving a name to the X and Y 
# colnames(grid_set) = c("PC1", "PC2")#, "DENSITY",
#                        # "Fe","Cr")
# # this is the MAGIC of the background coloring
# # here we use the classifier to predict the result of each of each of the pixel bits noted above. NOTE we need class here because we have a y_grid is a matrix!
# y_grid = predict(minModel, newdata = grid_set, type = 'class')
# # that's the end of the background
# # now we plat the actual data 
# plot(set[, 14:15],
#      main = 'Random Forest classification (Training set)',
#      xlab = 'Dim1 (44%)', ylab = 'Dim2 (25%)',
#      xlim = range(X1), ylim = range(X2)) # this bit creates the limits to the values plotted this is also a part of the MAGIC as it creates the line between green and red
# contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
# # here we run through all the y_pred data and use ifelse to color the dots
# # note the dots are the real data, the background is the pixel by pixel determination of y/n
# # graph the dots on top of the background give you the image
# points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
# points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


library(ElemStatLearn)
set <- trainSPlit %>%
  elem_norm(method = 'clr')

pca <- prcomp(set[-1],center = TRUE,scale. = FALSE)

summary(pca)

set <- set %>%
  bind_cols(as_tibble(pca$x)) %>%
  select(1, 14, 15)

minModel <- randomForest(min ~ .,
                         set,
                         proximity = TRUE,
                         ntree = 500, localImp = TRUE,
                         set.seed(42))


X1 = seq(min(set [, 2]) - 1, max(set [, 2]) + 1, by = 0.01)
X2 = seq(min(set [, 3]) - 1, max(set [, 3]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(minModel, grid_set)
plot(set[, -1],
     main = 'Random Forest Classification (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set [, 1] == 'ORE', 'green4', 'red3'))
