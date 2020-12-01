library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

setwd('C:\\Users\\GUILHERMEFERREIRA-PC\\Desktop')

list.files(pattern = '.xlsx')

df <- read_xlsx("pXRF_Jacobina_OLD.xlsx",sheet = 1)

# Ferro

df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Fe), col = SAMPLE)) +
  coord_cartesian(ylim = c(20000,45000)) +
  geom_point() +
  geom_hline(yintercept = 33600) +
  geom_hline(yintercept = 31920, col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = 35280, col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = 39700) +
  geom_hline(yintercept = 37715, col = 'green',linetype = 'dashed') +
  geom_hline(yintercept = 41685, col = 'green',linetype = 'dashed')


# Zr

df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Zr), col = SAMPLE)) +
  coord_cartesian(ylim = c(75,425)) +
  geom_point() +
  geom_hline(yintercept = 385) +
  geom_hline(yintercept = 365.75, col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = 404.25, col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = 195) +
  geom_hline(yintercept = 185.25, col = 'green',linetype = 'dashed') +
  geom_hline(yintercept = 204.75, col = 'green',linetype = 'dashed')


# K
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(K), col = SAMPLE)) +
  # coord_cartesian(ylim = c(75,425)) +
  geom_point() +
  geom_hline(yintercept = 21100) +
  geom_hline(yintercept = 20045, col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = 22155, col = 'red',linetype = 'dashed')


# Cu

a <- 237
b <- 33.9
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Cu), col = SAMPLE)) +
  coord_cartesian(ylim = c(0,350)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')


# Cr

a <- 130
b <- 45
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Cr), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')

# Ba

a <- 395
b <- 979
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Ba), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')

# Sr

a <- 109
b <- 239
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Sr), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')


# Th

a <- 17.4
b <- 10.9
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Th), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')


# Ti

a <- 4840
b <- 3360
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Ti), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')


# As

a <- 111
b <- 10.5
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(As), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')

# Ca

a <- 19100
b <- 6000
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Ca), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')

# Mg

a <- 7598
b <- 14600
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'NIST') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(Mg), col = SAMPLE)) +
  # coord_cartesian(ylim = c(0,10000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')


# S

a <- 800
b <- 800
df %>%
  filter(MISC == 'STANDARD') %>%
  # filter(SAMPLE == 'BLANK') %>%
  # filter(JOB == '#018') %>%
  ggplot(aes(x = Index, y = as.double(S))) +
  # coord_cartesian(ylim = c(800,100000)) +
  geom_point() +
  geom_hline(yintercept = a) +
  geom_hline(yintercept = (a*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (a*1.2), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = b) +
  geom_hline(yintercept = (b*0.8), col = 'red',linetype = 'dashed') +
  geom_hline(yintercept = (b*1.2), col = 'red',linetype = 'dashed')
