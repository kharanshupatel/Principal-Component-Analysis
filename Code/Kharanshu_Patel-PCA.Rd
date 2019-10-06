---
title: '[Group 2_Kharanshu]PCA - Oasis_Cross_Sectional'
author: "Kharanshu Patel"
date: "9/8/2018"
output:
  html_document:
    df_print: paged
  word_document: default
---

```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)
library(ExPosition)
library(corrplot)
library(ggplot2)
library(InPosition)
library(dplyr)
library(PTCA4CATA)
library(data4PCCAR)
library(classInt)
library(Hmisc)
library(psych)
library(TInPosition)
library(RColorBrewer)
library(plotly)
library(heatmaply)
library(TExPosition)
```
# Method: PCA
Principal component analysis (PCA) is used to analyze one table of quantitative data. PCA mixes the input variables to give new variables, called principal components. The first principal component is the line of best fit. It is the line that maximizes the inertia (similar to variance) of the cloud of data points. Subsequent components are defined as orthogonal to previous components, and maximize the remaining inertia. 

PCA gives one map for the rows (called factor scores), and one map for the columns (called loadings). These 2 maps are related, because they both are described by the same components. However, these 2 maps project different kinds of information onto the components, and so they are *interpreted differently*.  Loadings describe the column variables. Loadings are interpreted by the angle between them, and their distance from the origin. 

The distance from the origin is important in both maps, because squared distance from the mean is inertia (variance, information; see sum of squares as in ANOVA/regression). Because of the Pythagorean Theorem, the total information contributed by a data point (its squared distance to the origin) is also equal to the sum of its squared factor scores. 

# Dataset : oasis_cross-sectional

The Open Access Series of Imaging Studies (OASIS) is a project aimed at making MRI data sets of the brain freely available to the scientific community.
It is a cross-sectional MRI Data in Young, Middle Aged, Nondemented and Demented Older Adults.
This set consists of a cross-sectional collection of 216 subjects (rows) aged 33 to 96. The subjects include both men and women. 
It measures 216 subjects (rows) on 4 quanitiative variables (columns). The 5 qualitative variables have been used to interpret the data. 

Measures included in the study:

Age: Age (years)

Gender: Male or Female

Education: Years of Education - Recoded into categories from 1 (highest) to 5 (lowest)

SES: Socioeconomic status classified into categories from 1 (highest) to 5 (lowest)

MMSE: Mini-Mental State Examination score – range from 0 (worst) to 30 (best)

CDR: Clinical Dementia Rating - (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2 = moderate AD)

ASF : Atlas scaling factor (unitless). Computed scaling factor that transforms native-space brain and skull to the atlas target (i.e. the determinant of the transform matrix)

eTIV: Estimated total intracranial volume (cm3)

nWBV: Normalized whole brain volume, expressed as a percent of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process 

```{r data_set}

library(readxl)
oasis_cross_sectional <- read_excel("oasis_cross_sectional.xlsx")

head(oasis_cross_sectional)

oasis.dummy <- oasis_cross_sectional[c(6,8,9)]

# Recode Gender
oasis_cross_sectional$`M/F`[oasis_cross_sectional$`M/F`=="M"] <- "2"
oasis_cross_sectional$`M/F`[oasis_cross_sectional$`M/F`=="F"] <- "1"

oasis_cross_sectional$`M/F` <- as.numeric(as.character(oasis_cross_sectional$`M/F`))

colnames(oasis_cross_sectional)[2] <- c("Gender")

oasis.dummy2 <- oasis_cross_sectional[c(2,3,4,5,6,7,8,9,10)]

```
# Add Color
```{r COLPAL}

#We want to add different colors to the variables according to observations and group. 

#First create an empty list called COLPAL.
COLPAL <- list()

### Group variable Age 

#Create a new variable GroupAge which divides the varible Age into groups of 10.

COLPAL$rows$Age$labels <- c(paste(seq(0, 99, by = 10), seq(0 + 10 - 1, 100 - 1, by = 10), sep = "-"))
COLPAL$rows$Age$labels

oasis_cross_sectional$AgeGroup <- cut(oasis_cross_sectional$Age, breaks = c(seq(0, 99, by = 10), Inf), labels = COLPAL$rows$Age$labels, right = FALSE)

### Age
qts.age <- quantile(as.numeric(unlist(oasis_cross_sectional[,3])))[2:4]

hist(oasis_cross_sectional[,3], breaks = 10, main="Age", xlab = "Values") 
abline(v = qts.age, col = "red", lwd =2)
A1 = recordPlot()

# cut Age
Age_recode <- cut(as.numeric(unlist(oasis_cross_sectional[,3])), breaks= c(min(oasis_cross_sectional[,3])-1,qts.age, max(oasis_cross_sectional[,3])+1),labels=c(1,2,3,4))

# check correlation
cor(as.numeric(Age_recode), as.numeric(unlist(oasis_cross_sectional[,3])), method = "spearman")

table(Age_recode)
oasis_cross_sectional$Age

# Reorder Educ
table(oasis_cross_sectional$Educ)
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 1] <- "50" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 5] <- "1" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 50] <- "5" 

oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 2] <- "100" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 4] <- "2" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 100] <- "4" 

#Next step is to label the group names and to find which row belongs to which group (as a COLPAL vector, and then as a COLPAL matrix)

#Try to design colors on the rows.
#The COLPAL vector (a vector that describes which row belongs to which group) given by CDR is created.
COLPAL$rows$CDR$vec <- oasis_cross_sectional$CDR
COLPAL$rows$Educ$vec <- oasis_cross_sectional$Educ
COLPAL$rows$SES$vec <- oasis_cross_sectional$SES
COLPAL$rows$MF$vec <- oasis_cross_sectional$Gender
COLPAL$rows$Age$vec <- Age_recode

#Next Convert the vector to a matrix
COLPAL$rows$CDR$mat <- makeNominalData(as.matrix(COLPAL$rows$CDR$vec))
COLPAL$rows$Educ$mat <- makeNominalData(as.matrix(COLPAL$rows$Educ$vec))
COLPAL$rows$SES$mat <- makeNominalData(as.matrix(COLPAL$rows$SES$vec))
COLPAL$rows$MF$mat <- makeNominalData(as.matrix(COLPAL$rows$MF$vec))
COLPAL$rows$Age$mat <- makeNominalData(as.matrix(COLPAL$rows$Age$vec))


# Reorder the columns in ascending order

COLPAL$rows$Educ$mat <- COLPAL$rows$Educ$mat[,order(colnames(COLPAL$rows$Educ$mat))]

COLPAL$rows$SES$mat <- COLPAL$rows$SES$mat[,order(colnames(COLPAL$rows$SES$mat))]

COLPAL$rows$Age$mat <- COLPAL$rows$Age$mat[,order(colnames(COLPAL$rows$Age$mat))]

COLPAL$rows$CDR$mat <- COLPAL$rows$CDR$mat[,order(colnames(COLPAL$rows$CDR$mat))]

 
#The group names are labeled:
colnames(COLPAL$rows$CDR$mat) <- c("CDR 0", "CDR 0.5", "CDR 1", "CDR 2")
COLPAL$rows$CDR$labels <- colnames(COLPAL$rows$CDR$mat)

colnames(COLPAL$rows$Educ$mat) <- c("Class 1", "Class 2", "Class3", "Class 4", "Class 5")
COLPAL$rows$Educ$labels <- colnames(COLPAL$rows$Educ$mat)

colnames(COLPAL$rows$SES$mat) <- c("Class 1", "Class 2", "Class3", "Class 4", "Class 5")
COLPAL$rows$SES$labels <- colnames(COLPAL$rows$SES$mat)

colnames(COLPAL$rows$MF$mat) <- c("Female", "Male")
COLPAL$rows$MF$labels <- unique(oasis_cross_sectional$Gender)

colnames(COLPAL$rows$Age$mat) <- c("30-66", "67-73", "74-82", "83-100")
COLPAL$rows$Age$labels <- colnames(COLPAL$rows$Age$mat)
```


```{r colors_2}

###Make customized colors ###

COLPAL$rows$CDR$color_groups <- c("#e197ef","#acd12b","#687cef","#750000")
COLPAL$rows$Educ$color_groups <- c("#e4e400","#e01fdc","#3d41dc","#25dd2a","#d40108")
COLPAL$rows$SES$color_groups <- c("#cb01cc","#cbcc00","#01cccd","#7f3332","#0100fb")
COLPAL$rows$MF$color_groups <- c("#fc33e2", "#282be2")
COLPAL$rows$Age$color_groups <- c("#8f00fc", "#02c1c2", "#ebeb00", "#ff0101")

#Next we need to create a vector of the group colors.
#We'll take one group at a time and trade the group names for the color names...

#First, copy the group names
COLPAL$rows$CDR$color_obs <- as.matrix(COLPAL$rows$CDR$vec)
COLPAL$rows$Educ$color_obs <- as.matrix(COLPAL$rows$Educ$vec)
COLPAL$rows$SES$color_obs <- as.matrix(COLPAL$rows$SES$vec)
COLPAL$rows$MF$color_obs <- as.matrix(COLPAL$rows$MF$vec)
COLPAL$rows$Age$color_obs <- as.matrix(COLPAL$rows$Age$vec)


#Then, for each group, replace the group name with the respective group color

COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "0")] <- 
COLPAL$rows$CDR$color_groups[1]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "0.5")] <- COLPAL$rows$CDR$color_groups[2]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "1")] <-
COLPAL$rows$CDR$color_groups[3]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "2")] <-
COLPAL$rows$CDR$color_groups[4]

COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "1")] <- 
COLPAL$rows$Educ$color_groups[1]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "2")] <- 
COLPAL$rows$Educ$color_groups[2]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "3")] <- 
COLPAL$rows$Educ$color_groups[3]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "4")] <- 
COLPAL$rows$Educ$color_groups[4]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "5")] <- 
COLPAL$rows$Educ$color_groups[5]

COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "1")] <- 
COLPAL$rows$SES$color_groups[1]
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "2")] <- 
COLPAL$rows$SES$color_groups[2]
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "3")] <- 
COLPAL$rows$SES$color_groups[3]
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "4")] <- 
COLPAL$rows$SES$color_groups[4]
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "5")] <- 
COLPAL$rows$SES$color_groups[5]

COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "1")] <- 
COLPAL$rows$MF$color_groups[1]
COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "2")] <- 
COLPAL$rows$MF$color_groups[2]

COLPAL$rows$Age$color_obs[which(COLPAL$rows$Age$vec == "1")] <- COLPAL$rows$Age$color_groups[1]
COLPAL$rows$Age$color_obs[which(COLPAL$rows$Age$vec == "2")] <- COLPAL$rows$Age$color_groups[2]
COLPAL$rows$Age$color_obs[which(COLPAL$rows$Age$vec == "3")] <- COLPAL$rows$Age$color_groups[3]
COLPAL$rows$Age$color_obs[which(COLPAL$rows$Age$vec == "4")] <- COLPAL$rows$Age$color_groups[4]

```
# PCA

```{r analyze PCA, echo = TRUE}

oasis_pca <- epPCA(oasis.dummy,
                   center = TRUE,
                   scale = TRUE,
                   graphs = FALSE
                  )

loadings.pca <- t(oasis_pca$ExPosition.Data$cj)^2
```
### Varimax Rotation
```{r Varimax}
vari.oasis  <- data4PCCAR::epVari(oasis_pca)
```
### Inference battery 
```{r Inference Battery}

oasis.inf.CDR <- InPosition::epPCA.inference.battery(DATA = oasis.dummy,
                   scale = 'SS1', 
                   DESIGN = COLPAL$rows$CDR$color_groups,
                   graphs =  FALSE 
                   )

oasis.inf.Age <- InPosition::epPCA.inference.battery(DATA = oasis.dummy,
                   scale = 'SS1', 
                   DESIGN = COLPAL$rows$Age$color_groups,
                   graphs =  FALSE 
                   )

```
### Group Analysis 

```{r Group Analysis}

# Bootstrap for CI:
BootCube.oasis <- PTCA4CATA::Boot4Mean(oasis_pca$ExPosition.Data$fi, 
                                 design = Age_recode,
                                 niter = 100,
                                 suppressProgressBar = TRUE)
# Bootstrap ratios 
bootRatios.oasis <- boot.ratio.test(BootCube.oasis$BootCube)

# eigenvalues: MonteCarlo Approach
random.eigen <- data4PCCAR::monteCarlo.eigen(X = oasis.dummy, nIter = 100)

# eigenvalues: Bootstrap approach
bootstrap.eigen <- data4PCCAR::boot.eigen(oasis.dummy, nIter = 100)
```
## Factor Means
```{r Factor Means}

factor.means <- oasis_pca$ExPosition.Data$fi

colnames(factor.means) <- c("Component1", "Component2", "Component3") 

## Multiply all columns of factor scores by the COLPAL matrix to get the sum of all the factor scores for all groups
facCDR.sum <- t(factor.means) %*% COLPAL$rows$CDR$mat
facEduc.sum <- t(factor.means) %*% COLPAL$rows$Educ$mat
facSES.sum <- t(factor.means) %*% COLPAL$rows$SES$mat
facMF.sum <- t(factor.means) %*% COLPAL$rows$MF$mat
facAge.sum <- t(factor.means) %*% COLPAL$rows$Age$mat

# To compute the mean, divide these sums by the counts for each group.
facCDR.count <- colSums(COLPAL$rows$CDR$mat)
facEduc.count <- colSums(COLPAL$rows$Educ$mat)
facSES.count <- colSums(COLPAL$rows$SES$mat)
facMF.count <- colSums(COLPAL$rows$MF$mat)
facAge.count <- colSums(COLPAL$rows$Age$mat)


# Now compute the mean factor scores
facCDR.mean <- facCDR.sum/facCDR.count
facEduc.mean <- facEduc.sum/facEduc.count
facSES.mean <- facSES.sum/facSES.count
facMF.mean <- facMF.sum/facMF.count
facAge.mean <- facAge.sum/facAge.count
```
# Results PCA

### Correlation Plot
```{r correlation plot, echo = TRUE}

cor.oasis <- cor(oasis.dummy2)

corrplot(cor.oasis, 
         method = "circle",
         type = "upper",
         order = "FPC",
         bg = "#6e6e6f", 
         title = "Correlation Plot",
         mar=c(0,0,2,0), 
         tl.col = "#0c7315",
         tl.srt = 45,
         tl.cex = 1.3,
         na.label.col = "#fe7315",
         cl.cex = 1.2,
         cl.align.text = "l",
         )

```
## Scree Plot

```{r scree plot, echo= TRUE}

eigs <- oasis_pca$ExPosition.Data$eigs

oasis_scree <- plot(oasis_pca$ExPosition.Data$eigs,
                    ylab = "Eigenvalues",
                    xlab = "Components",
                    type = "l",
                    main = "Scree Plot",
                    ylim = c(-1, max(oasis_pca$ExPosition.Data$eigs)),
                    col = "#870404"
                    )
points(oasis_pca$ExPosition.Data$eigs, cex = 2, pch = 19, col = "#87cf04")
points(oasis_pca$ExPosition.Data$eigs, cex = 2, pch = 21, col = "#1d2649")


```
### Scree Plot + Interference

```{r Scree Plot with Inference}

# Create Function for Scree Plot
PlotScreeFix <- function (ev, p.ev = NULL, max.ev = NULL, alpha = 0.05, col.ns = "#006D2C", 
          col.sig = "#54278F", title = "Explained Variance per Dimension", 
          plotKaiser = FALSE, color4Kaiser = "darkorchid4", lwd4Kaiser = 2.5) 
{
  val.tau = (100 * ev/sum(ev))
  Top.y = ceiling(max(val.tau) * 0.1) * 10
  if (!is.null(max.ev)) {
    ev = ev * (max.ev/ev[1])
  }
  par(mar = c(5, 6, 4, 4))
  plot(x = seq(1, length(val.tau)), y = val.tau, xlab = "Dimensions", 
       ylab = "Percentage of Explained Variance", main = title, 
       type = "l", col = col.ns, lwd = 1, xlim = c(1, length(val.tau)), 
       ylim = c(0, Top.y))
  points(x = seq(1, length(val.tau)), y = val.tau, pch = 16, 
         cex = 1, col = col.ns, lwd = 2.5)
  if (!is.null(p.ev)) {
    signi.vp = which(p.ev < alpha)
    ##### These are the lines I changed #####
    lines(x = seq(1, max(signi.vp)), y = val.tau[1:max(signi.vp)], 
          type = "l", col = col.sig, lwd = 1.5)
    points(x = signi.vp, y = val.tau[signi.vp], 
           pch = 16, cex = 1.5, col = col.sig, lwd = 3.5)
    #########################################
  }
  par(new = TRUE)
  par(mar = c(5, 6, 4, 4) + 0.5)
  le.max.vp = Top.y * (ev[1]/val.tau[1])
  plot(ev, ann = FALSE, axes = FALSE, type = "n", ylim = c(0, 
                                                           le.max.vp))
  if (plotKaiser) {
    abline(h = sum(ev)/length(ev), col = color4Kaiser, lwd = lwd4Kaiser)
  }
  mtext("Inertia Extracted by the Components", side = 4, line = 3)
  axis(4)
}

# Scree + Inference----
PlotScreeFix(ev = eigs, 
       p.ev =  oasis.inf.Age$Inference.Data$components$p.vals,
       title = 'Eigenvalues Inference', plotKaiser = TRUE
       )
      
```
## Plot Mean Groups

```{r Plot Mean Groups}
Age.mean.plot <- prettyPlot(oasis_pca$ExPosition.Data$fi,
                  col = COLPAL$rows$Age$color_obs, 
                  display_names = FALSE)
prettyPlot(facAge.mean,
           col = COLPAL$rows$Age$color_groups, 
           display_names = FALSE,
           pch = 22, 
           cex = 1.5,
           dev.new = FALSE,
           new.plot = FALSE)
legend("bottomright", pch = 19, legend = COLPAL$rows$Age$labels, 
       col = COLPAL$rows$Age$color_groups, text.col = COLPAL$rows$Age$color_groups)
```

# Factor scores

Factor scores are the coordinates of the 216 subjects on the components. They are interpreted by the distances between them(shorter being more similar) and their distance from the origin. They can be color coded to interpret the components. The first plot is color coded according to the CDR ratings whereas the second plot has been color coded according to the gender.

```{r factor scores by CDR, echo=TRUE}

oasis_fac_CDR <- prettyPlot(data_matrix = oasis_pca$ExPosition.Data$fi,  
                           dev.new=FALSE,
                           main = "OASIS Row Factor Scores by CDR",
                           x_axis = 1, y_axis = 2,
                           contributionCircles = FALSE,
                           display_points = TRUE, pch = 24, cex = 1.3,  
                           display_names = FALSE,
                           fg.col = "red",
                           col = COLPAL$rows$CDR$color_obs,
                           xlab = paste0("Component 1 Inertia: ",
                           round(oasis_pca$ExPosition.Data$t[1],3), "%"),
                           ylab = paste0("Component 2 Inertia: ",
                           round(oasis_pca$ExPosition.Data$t[2],3), "%")
)

legend("topright", pch = 24, legend = COLPAL$rows$CDR$labels, 
       col = COLPAL$rows$CDR$color_groups, cex = 1, text.col = COLPAL$rows$CDR$color_groups
       )
```
## By Education

```{r Factor scores by Educ}

oasis_fac_Educ <- prettyPlot(data_matrix = oasis_pca$ExPosition.Data$fi,  
                           dev.new=FALSE,
                           main = "OASIS Row Factor Scores by Educ",
                           x_axis = 1, y_axis = 2,
                           contributionCircles = FALSE,
                           display_points = TRUE, pch = 24, cex = 0.8,  
                           display_names = FALSE,
                           fg.col = "red",
                           col = COLPAL$rows$Educ$color_obs,
                           xlab = paste0("Component 1 Inertia: ",
                           round(oasis_pca$ExPosition.Data$t[1],3), "%"),
                           ylab = paste0("Component 2 Inertia: ",
                           round(oasis_pca$ExPosition.Data$t[2],3), "%")
)
prettyPlot(facEduc.mean,
           col = COLPAL$rows$Educ$color_groups,
           display_names = FALSE,
           pch = 24, 
           cex = 1.5,
           dev.new = FALSE,
           new.plot = FALSE
)
legend("topright", pch = 23, legend = COLPAL$rows$Educ$labels, 
       col = COLPAL$rows$Educ$color_groups,
       text.col = COLPAL$rows$Educ$color_groups,
       cex = 1
)

```
## By Economic Status
```{r Factor score by SES}

oasis_fac_SES <- prettyPlot(data_matrix = oasis_pca$ExPosition.Data$fi,  
                           dev.new=FALSE,
                           main = "OASIS Row Factor Scores by SES",
                           x_axis = 1, y_axis = 2,
                           contributionCircles = FALSE,
                           display_points = TRUE, pch = 22, cex = 1.3,  
                           display_names = FALSE,
                           fg.col = "red",
                           col = COLPAL$rows$SES$color_obs,
                           xlab = paste0("Component 1 Inertia: ",
                           round(oasis_pca$ExPosition.Data$t[1],3), "%"),
                           ylab = paste0("Component 2 Inertia: ",
                           round(oasis_pca$ExPosition.Data$t[2],3), "%")
)
prettyPlot(facSES.mean,
           col = COLPAL$rows$SES$color_obs,
           display_names = FALSE,
           pch = 19, 
           cex = 2,
           dev.new = FALSE,
           new.plot = FALSE
)
legend("topright", pch = 22, legend = COLPAL$rows$SES$labels, 
       col = COLPAL$rows$SES$color_groups,
       lty = 1.2,
       cex = 1
)
```

## By Gender
```{r Factor scores by M/F}

oasis_facMF <- prettyPlot(data_matrix = oasis_pca$ExPosition.Data$fi,  
                           dev.new=FALSE,
                           main = "OASIS Row Factor Scores by Gender",
                           x_axis = 1, y_axis = 2,
                           contributionCircles = FALSE, 
                           display_points = TRUE, pch = 21, cex = 1.5,  
                           display_names = TRUE,
                           fg.col = "darkgreen",
                           col = COLPAL$rows$MF$color_obs,
                           xlab = paste0("Component 1 Inertia: ",                                     round(oasis_pca$ExPosition.Data$t[1],3), "%"),
                           ylab = paste0("Component 2 Inertia: ",                                     round(oasis_pca$ExPosition.Data$t[2],3), "%")
)
prettyPlot(facMF.mean,
           col = COLPAL$rows$MF$color_groups,
           display_names = FALSE,
           pch = 19, 
           cex = 2,
           dev.new = FALSE,
           new.plot = FALSE
)
legend("topleft", pch = 16, legend = c("MALE", "FEMALE"), 
       col = c("#282be2","#fc33e2"),
       lty = 1.2,
       cex = 0.8
)
 

```
##By Age
```{r Factor Scores by Age}

oasis_fac_Age <- prettyPlot(data_matrix = oasis_pca$ExPosition.Data$fi,
                            dev.new = FALSE,
                            main = "OASIS Row Factor Scores by Age",
                            x_axis = 1, y_axis = 2,
                            contributionCircles = FALSE,
                            display_points = TRUE, pch = 22, cex = 1.3,
                            display_names = FALSE,
                            fg.col = "gray 50",
                            col = COLPAL$rows$Age$color_obs,
                            xlab = paste0("Component 1 Inertia: ",
                            round(oasis_pca$ExPosition.Data$t[1],3), "%"),
                            ylab = paste0("Component 2 Inertia: ",
                            round(oasis_pca$ExPosition.Data$t[2],3), "%")
                            )

legend("topleft", pch = 22, legend = COLPAL$rows$Age$labels, 
       col = COLPAL$rows$Age$color_groups,
       lty = 1.2,
       cex = 1
)

```
* Component 1 : Non Demented vs Demented

* Component 2 : Male vs Female

# Loadings
Loadings describe the column variables. Loadings are interpreted by the angle between the variables and their distance from the origin.

```{r Loadings , echo=TRUE}

oasis_loading <- prettyPlot(data_matrix = oasis_pca$ExPosition.Data$fj,  
                            dev.new=FALSE,
                            main = "OASIS Column Loadings",
                            x_axis = 1, y_axis = 2, 
                            contributionCircles = TRUE, 
                            contributions = oasis_pca$ExPosition.Data$cj, 
                            display_points = TRUE, pos = 2, 
                            pch = 21, cex = 1.3, text.cex = 0.7, 
                            col = "red", 
                            display_names = TRUE,
                            fg.col = "blue",
                            bg.col = "brown",
                            flip = TRUE,
                            xlab = paste0("Component 1 Inertia: ",      
                                          round(oasis_pca$ExPosition.Data$t[1],3), "%"),
                            ylab = paste0("Component 2 Inertia: ", 
                                          round(oasis_pca$ExPosition.Data$t[2],3), "%")
)

```

# The I-set graphs

```{r I-set graphs}

# Age
oasis.ggplot1 <- PTCA4CATA::createFactorMap(oasis_pca$ExPosition.Data$fi,
col.points = COLPAL$rows$Age$color_obs,
display.points = TRUE,
display.names = FALSE,
display.labels = FALSE,
alpha.points = 0.8,
pch = 20,
cex = 2.5,
col.labels = COLPAL$rows$Age$color_obs,
text.cex = 2,
col.axes = "black",
alpha.axes = 1,
col.background = "gray50",
width.axes = 1.2,
title = "ROW Factor Scores"
)

# Labels for Inertia
label4Map.oasis <- createxyLabels.gen(1,2,
lambda =oasis_pca$ExPosition.Data$eigs,
tau = oasis_pca$ExPosition.Data$t)

c1.oasis.ggMap <- oasis.ggplot1$zeMap + label4Map.oasis +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(c1.oasis.ggMap)

# Plot Group Means

Age.Means <- PTCA4CATA::getMeans(oasis_pca$ExPosition.Data$fi, Age_recode)

col4Means <- unique(COLPAL$rows$Age$color_obs)

MapGroup <- PTCA4CATA::createFactorMap(Age.Means,
                            constraints = oasis.ggplot1$constraints,
                            display.labels = FALSE,
                            col.points = COLPAL$rows$Age$color_groups,
                            alpha.points = 0.7,
                            cex = 6.5,
                            col.labels = COLPAL$rows$Age$color_groups,
                            text.cex = 4,
                            dev.new=FALSE,
                            new.plot = FALSE)

pca.oasis.withMeans <- c1.oasis.ggMap + MapGroup$zeMap_dots + MapGroup$zeMap_text +
  ggtitle("ROW Factor Scores With Group Means") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(pca.oasis.withMeans)

# Plot Confidence Intervals using ellipses
GraphElli <- PTCA4CATA::MakeCIEllipses(BootCube.oasis$BootCube[,1:2,],
                            names.of.factors = c("Dimension 1","Dimension 2"),
                            col = COLPAL$rows$Age$color_groups,
                            p.level = .95
)

pca.oasis.withCI <-  pca.oasis.withMeans +  GraphElli + ggtitle("ROW Factor Scores with Confidence Intervals")
print(pca.oasis.withCI)

# Plot Tolerance Intervals

GraphTI.Hull.90 <- MakeToleranceIntervals(oasis_pca$ExPosition.Data$fi,
                                  as.factor(Age_recode),
                                  names.of.factors = c("Dim1","Dim2"),
                                  col = col4Means,
                                  line.size = .5, 
                                  line.type = 3,
                                  alpha.ellipse = .2,
                                  alpha.line = .4,
                                  p.level = .75,
                                  type = 'hull')

aggMap.i.withHull <- pca.oasis.withMeans  + GraphTI.Hull.90 + ggtitle("Row Factor Scores with Tolerance Intervals")

print(aggMap.i.withHull)

# Gender

oasis.ggplot2 <- PTCA4CATA::createFactorMap(oasis_pca$ExPosition.Data$fi,
col.points = COLPAL$rows$MF$color_obs,
display.points = TRUE,
display.names = FALSE,
display.labels = FALSE,
alpha.points = 0.8,
pch = 20,
cex = 2.5,
col.labels = COLPAL$rows$MF$color_obs,
text.cex = 2,
col.axes = "black",
alpha.axes = 1,
col.background = "white",
width.axes = 1.2,
title = "Gender ROW Factor Scores"
)

# Labels for Inertia
label4Map.oasis <- createxyLabels.gen(1,2,
lambda =oasis_pca$ExPosition.Data$eigs,
tau = oasis_pca$ExPosition.Data$t)

c2.oasis.ggMap <- oasis.ggplot2$zeMap + label4Map.oasis +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(c2.oasis.ggMap)

# Plot Group Means

Gender.Means <- PTCA4CATA::getMeans(oasis_pca$ExPosition.Data$fi, oasis_cross_sectional$Gender)

col4Means.gender <- unique(COLPAL$rows$MF$color_obs)

MapGroup.g <- PTCA4CATA::createFactorMap(Gender.Means,
                            constraints = oasis.ggplot2$constraints,
                            display.labels = FALSE,
                            col.points = COLPAL$rows$MF$color_groups,
                            alpha.points = 0.7,
                            cex = 6.5,
                            col.labels = COLPAL$rows$MF$color_groups,
                            text.cex = 4,
                            dev.new=FALSE,
                            new.plot = FALSE)

pca.oasis.gender.withMeans <- c2.oasis.ggMap + MapGroup.g$zeMap_dots + MapGroup.g$zeMap_text +
  ggtitle("Gender ROW Factor Scores With Group Means") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(pca.oasis.gender.withMeans)

# Gender Bootstrap for CI:
BootCube.oasis.g <- PTCA4CATA::Boot4Mean(oasis_pca$ExPosition.Data$fi, 
                                 design = oasis_cross_sectional$Gender,
                                 niter = 100,
                                 suppressProgressBar = TRUE)
# Gender Bootstrap ratios 
bootRatios.oasis.g <- boot.ratio.test(BootCube.oasis.g$BootCube)


# Plot Confidence Intervals using ellipses
GraphElli.g <- PTCA4CATA::MakeCIEllipses(BootCube.oasis.g$BootCube[,1:2,],
                            names.of.factors = c("Dimension 1","Dimension 2"),
                            col = COLPAL$rows$MF$color_groups,
                            p.level = .95
)

pca.oasis.gender.withCI <-  pca.oasis.gender.withMeans +  GraphElli.g + ggtitle("Gender ROW Factor Scores with Confidence Intervals")
print(pca.oasis.gender.withCI)

# Plot Tolerance Intervals

GraphTI.Hull.90.g <- MakeToleranceIntervals(oasis_pca$ExPosition.Data$fi,
                                  as.factor(oasis_cross_sectional$Gender),
                                  names.of.factors = c("Dim1","Dim2"),
                                  col = col4Means.gender,
                                  line.size = .5, 
                                  line.type = 3,
                                  alpha.ellipse = .2,
                                  alpha.line = .4,
                                  p.level = .75,
                                  type = 'hull')

aggMap.i.withHull.g <- pca.oasis.gender.withMeans  + GraphTI.Hull.90.g + ggtitle("Gender Row Factor Scores with Tolerance Intervals")

print(aggMap.i.withHull.g)

```
# The J-set graphs

A circle of correlation

# Circle of Correlation

```{r correlation circle}

loadings.oasis <- (cor(oasis.dummy, oasis_pca$ExPosition.Data$fi))

col4J <- prettyGraphsColorSelection(NCOL(oasis.dummy))
col4j2 <- c("darkgreen","darkblue","darkred")

mapJ.oasis <- PTCA4CATA::createFactorMap(loadings.oasis ,
col.points = col4J,
col.labels = col4J,
display.points = TRUE, pos = 2,
display.names = TRUE,
display.labels = TRUE,
alpha.points = 0.8,
pch = 20,
cex = 3.5,
text.cex = 3.5,
font.face = "bold",
col.axes = "black",
alpha.axes = 1,
col.background = "gray50",
width.axes = 1.2,
constraints = list(minx = -1, miny = -1,
maxx = 1 , maxy = 1)
)

# Circle with arrows
arrows.oasis <- addArrows(loadings.oasis, color = col4J, 
                          size = 1.2, 
                          alpha = 0.8,
                          arrowLength = 0.5
                          )

d1.jolieggMap.J <- mapJ.oasis$zeMap_background +
mapJ.oasis$zeMap_text +
  addCircleOfCor(color = "red", size = 1.5, alpha = 0.6) + 
  arrows.oasis + 
  ggtitle("COLOUMN Loadings with Correlation Cirlce") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(d1.jolieggMap.J)
```

# Contributions

```{r Contributions}

create.ctrPlot <- function(res, axis = 1,
col = NULL,
set = 'J', ...){
if (set == 'I'){
ctr.s <- res$ExPosition.Data$ci[,axis] *
sign(res$ExPosition.Data$fi[,axis])
} else {
ctr.s <- res$ExPosition.Data$cj[,axis] *
sign(res$ExPosition.Data$fj[,axis])
}
zebars <- PrettyBarPlot2(ctr.s,
threshold = 1 / length(ctr.s),
color4bar = gplots::col2hex(col), # we need hex code
main = paste0('Signed Contributions. Dimension ',axis),
ylab = 'Contributions',
ylim = c(1.2*min(ctr.s), 1.2*max(ctr.s) ), ...)
return(zebars)
} # end of function create.ctrPlot

### Dimension 1

e1.ctrJ <- create.ctrPlot(oasis_pca, axis = 1, font.size = 5,
col = col4j2, horizontal = FALSE)
print(e1.ctrJ)

### Dimension 2

e2.ctrJ <- create.ctrPlot(oasis_pca, axis = 2, font.size = 5,
col = col4j2, horizontal = FALSE)
print(e2.ctrJ)

### Dimension 3
e3.ctrJ <- create.ctrPlot(oasis_pca, axis = 3, font.size = 5,
col = col4j2, horizontal = FALSE)
print(e3.ctrJ)
```


# Permutation Test for Eigen-values

```{r Permutation Test for Eigen-values}
zeDim = 1
pH1 <- prettyHist(
  distribution = oasis.inf.Age$Inference.Data$components$eigs.perm[,zeDim], 
           observed = oasis.inf.Age$Fixed.Data$ExPosition.Data$eigs[zeDim], 
           xlim = c(0, 2), 
           breaks = 20,
           border = "white", 
           main = paste0("Permutation Test for Eigenvalue ",zeDim),
           xlab = paste0("Eigenvalue ",zeDim), 
           ylab = "", 
           counts = FALSE, 
           cutoffs = c( 0.975))

zeDim = 2
pH1 <- prettyHist(
  distribution = oasis.inf.Age$Inference.Data$components$eigs.perm[,zeDim], 
           observed = oasis.inf.Age$Fixed.Data$ExPosition.Data$eigs[zeDim], 
           xlim = c(0, 2), 
           breaks = 20,
           border = "white", 
           main = paste0("Permutation Test for Eigenvalue ",zeDim),
           xlab = paste0("Eigenvalue ",zeDim), 
           ylab = "", 
           counts = FALSE, 
           cutoffs = c( 0.975))

zeDim = 3
pH1 <- prettyHist(
  distribution = oasis.inf.Age$Inference.Data$components$eigs.perm[,zeDim], 
           observed = oasis.inf.Age$Fixed.Data$ExPosition.Data$eigs[zeDim], 
           xlim = c(0, 2),
           breaks = 20,
           border = "white", 
           main = paste0("Permutation Test for Eigenvalue ",zeDim),
           xlab = paste0("Eigenvalue ",zeDim), 
           ylab = "", 
           counts = FALSE, 
           cutoffs = c( 0.975))

```
# Monte Carlo Parallel Test for Eigen-values

```{r Monte Carlo Parallel Test for Eigen-values}

zeDim = 1
pH1.p <- prettyHist(random.eigen$rand.eigs[,zeDim], 
  observed = random.eigen$fixed.eigs[zeDim], 
  xlim = c(0, 2), 
  breaks = 20,
  border = "white", 
  main = paste0("Monte Carlo (Parallel) Test for Eigenvalue ",zeDim),
  xlab = paste0("Eigenvalue ",zeDim), 
  ylab = "", 
  counts = FALSE, 
  cutoffs = c( 0.975))

zeDim = 2
pH1.p <- prettyHist(random.eigen$rand.eigs[,zeDim], 
  observed = random.eigen$fixed.eigs[zeDim], 
  xlim = c(0, 2),
  breaks = 20,
  border = "white", 
  main = paste0("Monte Carlo (Parallel) Test for Eigenvalue ",zeDim),
  xlab = paste0("Eigenvalue ",zeDim), 
  ylab = "", 
  counts = FALSE, 
  cutoffs = c( 0.975))

zeDim = 3
pH1.p <- prettyHist(random.eigen$rand.eigs[,zeDim], 
  observed = random.eigen$fixed.eigs[zeDim], 
  xlim = c(0, 2),
  breaks = 20,
  border = "white", 
  main = paste0("Monte Carlo (Parallel) Test for Eigenvalue ",zeDim),
  xlab = paste0("Eigenvalue ",zeDim), 
  ylab = "", 
  counts = FALSE, 
  cutoffs = c( 0.975))

```
# Bootstrap Test for eigen-values
```{r Bootstrap Test for Eigen-values}

zeDim = 1
pH2.p <- prettyHist(bootstrap.eigen$boot.eigs[,zeDim], 
                    observed = random.eigen$fixed.eigs[zeDim], 
                    xlim = c(1, 2), 
                    breaks = 20,
                    border = "white", 
                    main = paste0("Bootstrapped distribution for Eigenvalue ",zeDim),
                    xlab = paste0("Eigenvalue ",zeDim), 
                    ylab = "", 
                    counts = FALSE, 
                    cutoffs = c(0.025, 0.975))

zeDim = 2
pH2.p <- prettyHist(bootstrap.eigen$boot.eigs[,zeDim], 
                    observed = random.eigen$fixed.eigs[zeDim], 
                    xlim = c(0.5, 1.5), 
                    breaks = 20,
                    border = "white", 
                    main = paste0("Bootstrapped distribution for Eigenvalue ",zeDim),
                    xlab = paste0("Eigenvalue ",zeDim), 
                    ylab = "", 
                    counts = FALSE, 
                    cutoffs = c(0.025, 0.975))

zeDim = 3
pH2.p <- prettyHist(bootstrap.eigen$boot.eigs[,zeDim], 
                    observed = random.eigen$fixed.eigs[zeDim], 
                    xlim = c(0, 1), 
                    breaks = 20,
                    border = "white", 
                    main = paste0("Bootstrapped distribution for Eigenvalue ",zeDim),
                    xlab = paste0("Eigenvalue ",zeDim), 
                    ylab = "", 
                    counts = FALSE, 
                    cutoffs = c(0.025, 0.975))
```
# Bootstrap Ratios

```{r Bootstrap Ratio}
BR <- oasis.inf.Age$Inference.Data$fj.boots$tests$boot.ratios
laDim = 1
ba001.BR1 <- PrettyBarPlot2(BR[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = gplots::col2hex(col4j2), # we need hex code
                   main = paste0(
                     'PCA on the Oasis Set: Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(ba001.BR1)

laDim = 2
ba002.BR2 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4j2), # we need hex code
                            main = paste0(
                              'PCA on the Oasis Set: Bootstrap ratio Dimension ',laDim),
                ylab = 'Bootstrap ratios'
)
print(ba002.BR2)

laDim = 3
ba003.BR3 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4j2), # we need hex code
                            main = paste0(
                              'PCA on the Oasis Set: Bootstrap ratio Dimension ',laDim),
                ylab = 'Bootstrap ratios'
)

print(ba003.BR3)

```

# Varimax graphs

```{r Varimax graphs}

labels4Vari <- PTCA4CATA::createxyLabels.gen(1,2,
                                    lambda = vari.oasis$rotated.eigs,
                                     tau = vari.oasis$rotated.t)
baseMap.j.rot <- PTCA4CATA::createFactorMap(vari.oasis$rotated.J,
                                  col.points   = col4j2,
                                   alpha.points =  .3,
                                   col.labels   = col4j2,
                                   title = 'Loadings Post Varimax')
zeArrows.rot <- addArrows(vari.oasis$rotated.J, color = col4j2)

c001.aggMap.J.rot <- baseMap.j.rot$zeMap_background + # background layer
                     baseMap.j.rot$zeMap_dots +
                     baseMap.j.rot$zeMap_text +  # dots & labels
                     zeArrows.rot + labels4Vari
print(c001.aggMap.J.rot)

# Plot the Rotated observations

oasis.Imap.rot <- PTCA4CATA::createFactorMap(
  vari.oasis$rotated.I,
  col.points = COLPAL$rows$Age$color_obs,
  col.background = "gray 50",
  col.axes = "black",
  display.labels = FALSE,
  alpha.points = .8,
  alpha.axes = 1,
  title = 'Factor Scores Post Varimax'
)
c002.Map.I.rot <- oasis.Imap.rot$zeMap + labels4Vari + theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(c002.Map.I.rot)
```

### Conclusion

* Component 1 : 

Rows : Young Subjects vs Old Subjects
Columns : High MMSE scores vs Low MMSE scores

* Component 2 : 

Rows : Male vs Female
Columns : High eTIV vs Low eTIV

Component 1 inertia : 50.997%
Component 2 inertia : 33.082%




