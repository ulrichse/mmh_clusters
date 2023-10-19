
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
library(readxl)

#read in ZCTA file with 0 or 1 for each cluster type
pre_clusters <- read.csv("data/zcta_clusters_updated_2016_2019.csv")
post_clusters <- read.csv("data/zcta_clusters_updated_2020_2021.csv")

pre_clusters <- pre_clusters %>%
  arrange(ZCTA)
pre <- as.numeric(pre_clusters$MDP_CLU) 

post_clusters <- post_clusters %>%
  arrange(ZCTA)
covid <- as.numeric(post_clusters$MDP_CLU) 

rand.index(covid, pre) #should print out a decimal value between 0-1
adj.rand.index(covid, pre) #adjusted rand index

#Multivariate clusters

multi_pre <- read_excel("data/pre_multi_clu.xls")
multi_post <- read_excel("data/post_multi_clu.xls")

multi_pre <- multi_pre %>%
  arrange(zcta5ce10)
pre <- as.numeric(multi_pre$pre_cl_multi) 

multi_post <- multi_post %>%
  arrange(zcta5ce10)
covid <- as.numeric(multi_post$post_multi_clu) 

rand.index(covid, pre) #should print out a decimal value between 0-1
adj.rand.index(covid, pre)


install.packages("clusteval")
library(clusteval)


ari <- adjustedRandIndex(covid, pre)










