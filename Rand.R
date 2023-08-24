#RAND index for MMH clusters before and during COVID-----------------------------

install.packages('fossil')
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("qvalue")
install.packages("jaccard")
remove.packages("rlang")
install.packages("rlang")
library(rlang)
library(dplyr)
library(fossil)
library(data.table)
library(jaccard)

setwd('C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/MMH Clusters/Rand.R')'

pre_clusters <- read.csv("data/zcta_clusters_updated_2016_2019.csv")
post_clusters <- read.csv("data/zcta_clusters_updated_2020_2021.csv")

pre_clusters[is.na(pre_clusters)]<-0
post_clusters[is.na(post_clusters)]<-0

pre_clusters_filter <- pre_clusters %>%
  filter(SMI_CLU==1)

pre_clusters_filter$cluster <- 1

pre_clusters_filter <- pre_clusters_filter %>%
  select(ZCTA, cluster)

pre_clusters <- pre_clusters %>%
  left_join(pre_clusters_filter, by=c('ZCTA'))

pre_clusters[is.na(pre_clusters)]<-0

post_clusters_filter <- post_clusters %>%
  filter(SMI_CLU==1)

post_clusters_filter$cluster <- 1

post_clusters_filter <- post_clusters_filter %>%
  select(ZCTA, cluster)

post_clusters <- post_clusters %>%
  left_join(post_clusters_filter, by=c('ZCTA'))

post_clusters[is.na(post_clusters)]<-0

pre <- pre_clusters %>%
  select(ZCTA, cluster.x.x)

pre <- pre %>%
  arrange(ZCTA)

covid <- post_clusters %>%
  select(ZCTA, cluster.x.x)

covid <- covid %>%
  arrange(ZCTA)
  
covid <- as.numeric(covid$cluster.x.x)
pre <- as.numeric(pre$cluster.x.x)

rand.index(covid, pre)

#RAND index for pre- and post-COVID MMH clusters--------------------------------


setwd('C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/MMH Clusters/Rand.R')'

pre <- fread('preMDD_ForRand.csv')
pre <- as.numeric(pre$CLUSTER)

covid <- fread('MDD_ForRand.csv')
covid <- as.numeric(covid$CLUSTER)
str(covid)

pre <- pre %>%
  arrange(ZCTA)

covid <- covid %>%
  arrange(ZCTA)

rand.index(covid, pre) #should print out a decimal value between 0-1
