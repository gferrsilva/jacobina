library(tidyverse)
library(readxl)
library(janitor)
library(compositions)
library(geoquimica)
library(corrplot)
library(factoextra)
library(mclust)
library(dbscan)

setwd('C:\\Users\\GUILHERMEFERREIRA-PC\\Desktop\\pxrf')

files <- list.files(pattern = '.xlsx')

df <- lapply(files, read_xlsx, sheet = 1, col_types = 'text') %>%
  bind_rows() %>%
  mutate(Time = excel_numeric_to_date(as.double(Time),include_time = T)) %>%
  mutate(key = paste(Time,SAMPLE,sep = '_')) %>%
  distinct(key, .keep_all = TRUE) %>%
  filter(as.double(Duration) >= 110) %>%
  filter(!LOCATION %in% c("r salitre", "rio salitre", "rio salitri")) %>%
  filter(!SAMPLE %in% c("NIST", "BLANK", "teste_inicio",
                        "RCRA-180-637",'teste01','branco01','jaime',
                        'nist','till','teste_till','testeinicio-21-11-2019',
                        'branco', 'PADRAO','teste','padrao','TILL')) %>%
  filter(!grepl('apa',SAMPLE)) %>%
  select(-ends_with(c('O','O2','O3','O5','ua'),ignore.case = F)) %>%
  mutate(Index = as.factor(Index),
         Type = as.factor(Type),
         Duration = as.double(Duration),
         Units = as.factor(Units)) %>%
  select(-`...11`)

df[,15:121] <- sapply(df[,15:121], as.double)



df_error <- df %>%
  select(ends_with('error'))

df <- df %>%
  select(-ends_with('error'))


elem_fillrate(df, sort = TRUE)

df <- elem_select(df,cut = .75)

df1 <- df %>%
  distinct(SAMPLE, .keep_all = TRUE)


df1 <- elem_imput(df1,coef = .5)

df1 %>%
  ggplot(aes(x = clr(as.double(Fe)), y = clr(as.double(S)),
             col = as.double(Cu), size = log10(as.double(Si)))) +
  geom_point() +
  scale_color_viridis_c()

df1 %>%
  ggplot(aes(x = clr(Fe))) +
  geom_density()


df_norm <- elem_norm(df1,
                     method ='clr',
                     keep = c('Duration')) %>%
  select(key,Duration, SAMPLE, 11:35)


df_norm <- df_norm %>%
  select(-Ba, -Cs, -Te, -Sb, -Sn, -Cd, -Pd, - Nd,
         -Pr, - Ce, -La, -Bal)

cor <- cor(df_norm %>% 
             select(4:16), method = "spearman")

corrplot(cor, method="color",# col=col(200),  
         order= 'original', 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=TRUE,
         # title = "Spearman's Correlogram for CLR transformed data",
         tl.cex = .8, # tamanho da fonte dos labels
         tl.offset = 1, # offset do nome das colunas em relação a matriz
         number.font = .7,
         number.digits = 1)





# Outlier detection ----

## DBscan

### detectando abaixo o raio ótimo para busca dos dados para 5 vizinhos
dbscan::kNNdistplot(df_norm[,4:16], k = 5)
abline(h = 5, lty = 2)

### density-based scannering
res.db <- dbscan::dbscan(df_norm[,4:16],eps = 5,
                         minPts = 5)

# vizualização
fviz_cluster(res.db, df_norm[,4:16], geom = 'point')

df_norm <- df_norm %>%
  bind_cols(res.db$cluster) %>%
  rename(outlier_db = '...17') %>%
  mutate(outlier_db = ifelse(outlier_db == 0,'outlier','regular data')) %>%
  filter(outlier_db != 'outlier') %>%
  select(-outlier_db)

df_pca <- prcomp(df_norm[,4:16], center = TRUE,scale. = FALSE)


fviz_pca_ind(df_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F,      # Avoid text overlapping
             axes = c(1,2), #controla quais eixos devem ser mostrados na figura
             geom = c("point")
)
fviz_pca_var(df_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# fviz_nbclust(df_pca$x[,1:5], kmeans, method = "silhouette", k.max = 25)

mclus_fil <- Mclust(df_pca$x[,1:3], G = 1:8)
summary(mclus_fil)
## MClus plot for raw data
fviz_cluster(mclus_fil,
             data = df_pca$x[,1:3],
             axes = c(1,2), stand = F, ellipse = T, 
             ellipse.type = 'convex', 
             choose.vars = c(1,2), geom = c('point'),
             main = 'MB Cluster (Raw Data)')

plot(mclus_fil, what = 'BIC')
plot(mclus_fil, what = 'uncertainty')

df_norm <- df_norm %>%
  bind_cols(mclus_fil$classification) %>%
  rename(cluster = '...17')


df2 <- df_norm %>%
  group_by(cluster)


cor <- cor(df_norm %>% 
             filter(cluster == 1) %>%
             select(4:16), method = "spearman")

corrplot(cor, method="color",# col=col(200),  
         order= 'original', 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=TRUE,
         # title = "Spearman's Correlogram for CLR transformed data",
         tl.cex = .8, # tamanho da fonte dos labels
         tl.offset = 1, # offset do nome das colunas em relação a matriz
         number.font = .7,
         number.digits = 1)

