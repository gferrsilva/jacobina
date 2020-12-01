library(tidyverse)
library(readxl)
library(janitor)
library(geoquimica)
library(GGally)


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


# writexl::write_xlsx(df, 'output/pXRF_Jacobina.xlsx')


df[,15:121] <- sapply(df[,15:121], as.double)



df_error <- df %>%
  select(ends_with('error'))

df <- df %>%
  select(-ends_with('error'))


elem_fillrate(df, sort = TRUE)

df <- elem_select(df,cut = .7)
 
df <- df %>%
  distinct(SAMPLE, .keep_all = TRUE)


df1 <- elem_imput(df,coef = .5)

writexl::write_xlsx(df1, 'output/pXRF_Jacobina_SAMPLES.xlsx')


df_norm <- elem_norm(df1,
                     method ='clr',
                     keep = c('Duration')) %>%
  select(key,12:39)


df_norm %>%
  ggplot(aes(x = Fe, y = S,
             col = Cu)) +
  geom_point() +
  scale_color_viridis_c() +
  coord_equal(xlim = c(-2.5,5),
                  ylim = c(-1.5,3))

df_norm %>%
  select(2:14,26:28) %>%
  ggpairs()


nome <- paste(names(df_norm[,2:26]), "clr", sep = "_")

names(df_norm[,2:26]) <- nome


t <- df1 %>%
  left_join(df_norm, by = 'key')




# REFERENCES ----

# GGPAIRS
## https://www.blopig.com/blog/2019/06/a-brief-introduction-to-ggpairs/
