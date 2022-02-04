# Clear all plots
dev.off()

### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# General-purpose data wrangling
library(tidyverse)  
library(data.table)

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Eases DateTime manipulation
library(lubridate)

# Total
urlPage= "https://www.foxsports.com.au/nrl/nrl-premiership/stats/teams"

# Read webpage
webPage= read_html(urlPage)

tables= webPage %>% html_table()

summaryTeamTotal= tables[[4]]

setwd("~/Documents/NRLStats/2021")
saveRDS(summaryTeamTotal, 'summaryTeamTotal2021')

# Team average
urlPage= "https://www.foxsports.com.au/nrl/nrl-premiership/stats/teams?editiondata=none&fromakamai=true&pt=none&device=DESKTOP&wpa=BB44D82C3D7223D393F2AE47579FB5EA6791ABE4&isAvg=true"

# Read webpage
webPage= read_html(urlPage)

tables= webPage %>% html_table()

summaryTeamAvg= tables[[4]]

setwd("~/Documents/NRLStats/2021")
saveRDS(summaryTeamAvg, 'summaryTeamAvg2021')

### PCA analysis
teamName= gsub('[[:digit:]]+', '', summaryTeamTotal$Name)
teamName= paste0(1:16, '.', teamName)

# Prepare data for PCA- matrix with row names
mydata = summaryTeamTotal[,2:ncol(summaryTeamTotal)]
row.names(mydata) = teamName

mydata$`FG%`= NULL

names(mydata)
names(mydata)= metric

# Get rid of commasmydata
mydata= 
  mydata %>%
    map_df(str_replace, pattern = ",", replacement = "") %>%
    map_df(as.numeric)
row.names(mydata) = teamName

mydata <- scale(mydata) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:11) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:11, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 6) # 5 cluster solution

k2 <- kmeans(mydata, centers = 2, nstart = 25)

library(factoextra)
library(fpc)
library(useful)

km = kmeans(mydata, centers = 4)
plot(km, data= mydata)

# PCA
res.pca <- prcomp(mydata, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                pointsize = "cos2",
                col.ind = "Black",  # Individuals color
                col.var= "steelblue"
)

plot1= fviz_pca_biplot(res.pca, alpha.var="contrib",
                pointsize = "cos2",
                col.var= as.factor(legendDF$statType))
str(plot1)
plot1$labels
plot1$labels$size= 'Importance'
plot1$labels$colour= 'Metric'
plot1$labels$alpha= 'Contribution'

plot1

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

library("corrplot")
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_eig(res.pca)
