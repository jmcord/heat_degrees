m(list = ls())


#libraries


library(M3)
library(lubridate)
require(ncdf4) #para tratamiento de datos nc
require(fields) #para visualizaci?n
require(maps) #para mapa base
require(DescTools) # some utilities
library(dplyr) # data_frame utils
library(sqldf) #df combinations
library(ggplot2) #Graphics
library(openair) #Graphics
library(stringr) #NLPP
library(data.table)
library(readr)
#library(xlsx)



#NC MAD_meteo_2015_SVN_Daily_Sin_Vegetacion_Madrid ####

PATH <- 'D:/Varios/heat_degrees/Nuevos_escenarios_24_01_2023/nuevos_escenarios'
setwd(PATH)

wdep.ioapi.file <- 'T2_BASE_ANUAL.nc'
dep.in <- nc_open(wdep.ioapi.file, write=T)
num.cols <- ncatt_get(dep.in, varid=0, attname="NCOLS")$value
num.rows <- ncatt_get(dep.in, varid=0, attname="NROWS")$value
num.layers <- ncatt_get(dep.in, varid=0, attname="NLAYS")$value
num.time.steps <- ncatt_get(dep.in, varid=0, attname="TSTEP")$value

print(paste0("The file has ",num.cols," columnas,", num.rows, " filas,", num.layers," capas, y ", num.time.steps," time_steps"))

print(dep.in)


varr <-  ncvar_get(nc=dep.in, varid='T2')

Val_col <- C
Val_row <- F

#159 columnas, 168 filas, 365 tsteps

Val <- varr[Val_col,Val_row,1:365]



df_ <- data.frame(matrix(ncol = 6, nrow = 19584))
colnames(df_) <- c('C','F','day','T2','heat','cold')

 for (k in c(1:nrow(df_))) {
 for (i in c(1:136)) {
   for (j in c(1:144)) {
     df_$C[k] = i
     df_$F[k] = j
   }
 }
   print(k)
 }

df_$C <- c(1:159)

df_$F <- rep(seq(c(1:168)), each = 136)


df_cf <- df_[rep(seq_len(nrow(df_)), each = 365), ]

row.names(df_cf) <- NULL

df_cf$day <- c(1:365)
row.names(df_cf) <- NULL

T2 <- vector()
for (j in c(1:168)) {
  for (i in c(1:159)) {
    
    T2 <- append(T2,varr[i,j,1:365])
    
  }
  print(j)}

df_cf$T2 <- T2 

#By the moment, MMT = 21.4 ?C

MMT = 21.4

for (i in c(1:nrow(df_cf))) {
  df_cf$heat[i] = max((df_cf$T2[i]-MMT),0)
  df_cf$cold[i] = max((MMT-df_cf$T2[i]),0)
  print(i)
}

df_cf$heat <- sapply(df_cf$T2, function(x) max((x-MMT),0))
df_cf$cold <- sapply(df_cf$T2, function(x) max((MMT-x),0))

#write.csv(df_cf, 'df_cf.csv') 

df_cf <- read.csv('df_cf.csv', num.rows = FALSE)


df_final_year <- data.frame(matrix(ncol = 4, nrow = 19584))
colnames(df_final_year) <- c('C','F','heat','cold')

df_final_year$C <- c(1:159)

df_final_year$F <- rep(seq(c(1:168)), each = 136)

df_final_year$C_F <- paste0(df_final_year$C,'_',df_final_year$F)
df_cf$C_F <- paste0(df_cf$C,'_',df_cf$F)


heat <- vector()
cold <- vector()
for (ref in unique(df_cf$C_F)) {
  df = df_cf[df_cf$C_F == ref, ]
  heat = append(heat,sum(df$heat)/365)
  cold = append(cold,sum(df$cold)/365)
}

df_final_year$heat <- heat
df_final_year$cold <- cold

write.csv(df_final_year, 'df__2015_SVN_Daily_Sin_Vegetacion_Madrid2.csv')
df__2015_SVN_Daily_Sin_Vegetacion_Madrid <- read.csv('df__2015_SVN_Daily_Sin_Vegetacion_Madrid.csv')


#### END MAD_meteo_2015_SVN_Daily_Sin_Vegetacion_Madrid ####

#### NC MAD_meteo_2015_SVR_Daily_Esc_BASE ####


PATH <- 'D:/Varios/heat_degrees/VEGGAP_Meteorologia'
setwd(PATH)

wdep.ioapi.file <- 'MAD_meteo_2015_SVR_Daily_Esc_BASE.nc'
dep.in <- nc_open(wdep.ioapi.file, write=T)
num.cols <- ncatt_get(dep.in, varid=0, attname="NCOLS")$value
num.rows <- ncatt_get(dep.in, varid=0, attname="NROWS")$value
num.layers <- ncatt_get(dep.in, varid=0, attname="NLAYS")$value
num.time.steps <- ncatt_get(dep.in, varid=0, attname="TSTEP")$value

print(paste0("The file has ",num.cols," columnas,", num.rows, " filas,", num.layers," capas, y ", num.time.steps," time_steps"))

print(dep.in)


varr <-  ncvar_get(nc=dep.in, varid='T2')

#136 columnas, 144 filas


df_ <- data.frame(matrix(ncol = 6, nrow = 19584))
colnames(df_) <- c('C','F','day','T2','heat','cold')


df_$C <- c(1:136)

df_$F <- rep(seq(c(1:144)), each = 136)


df_cf <- df_[rep(seq_len(nrow(df_)), each = 365), ]

df_cf$day <- c(1:365)
row.names(df_cf) <- NULL

T2 <- vector()
for (j in c(1:144)) {
  for (i in c(1:136)) {
    
    T2 <- append(T2,varr[i,j,1:365])
    
  }
  print(j)}

df_cf$T2 <- T2 

#By the moment, MMT = 21.4 ?C

MMT = 21.4


df_cf$heat <- sapply(df_cf$T2, function(x) max((x-MMT),0))
df_cf$cold <- sapply(df_cf$T2, function(x) max((MMT-x),0))

df_final_year <- data.frame(matrix(ncol = 4, nrow = 19584))
colnames(df_final_year) <- c('C','F','heat','cold')

df_final_year$C <- c(1:136)

df_final_year$F <- rep(seq(c(1:144)), each = 136)

df_final_year$C_F <- paste0(df_final_year$C,'_',df_final_year$F)
df_cf$C_F <- paste0(df_cf$C,'_',df_cf$F)


heat <- vector()
cold <- vector()
for (ref in unique(df_cf$C_F)) {
  df = df_cf[df_cf$C_F == ref, ]
  heat = append(heat,sum(df$heat)/365)
  cold = append(cold,sum(df$cold)/365)
}

df_final_year$heat <- heat
df_final_year$cold <- cold

write.csv(df_final_year, 'df__MAD_meteo_2015_SVR_Daily_Esc_BASE2.csv')
df__MAD_meteo_2015_SVR_Daily_Esc_BASE <- read.csv('df__MAD_meteo_2015_SVR_Daily_Esc_BASE.csv')


#### END MAD_meteo_2015_SVR_Daily_Esc_BASE ####


#### NC MAD_meteo_2015_SVR_Daily_Esc_Futuro ####


PATH <- 'D:/Varios/heat_degrees/VEGGAP_Meteorologia'
setwd(PATH)

wdep.ioapi.file <- 'MAD_meteo_2015_SVR_Daily_Esc_Futuro.nc'
dep.in <- nc_open(wdep.ioapi.file, write=T)
num.cols <- ncatt_get(dep.in, varid=0, attname="NCOLS")$value
num.rows <- ncatt_get(dep.in, varid=0, attname="NROWS")$value
num.layers <- ncatt_get(dep.in, varid=0, attname="NLAYS")$value
num.time.steps <- ncatt_get(dep.in, varid=0, attname="TSTEP")$value

print(paste0("The file has ",num.cols," columnas,", num.rows, " filas,", num.layers," capas, y ", num.time.steps," time_steps"))

print(dep.in)


varr <-  ncvar_get(nc=dep.in, varid='T2')

#136 columnas, 144 filas


df_ <- data.frame(matrix(ncol = 6, nrow = 19584))
colnames(df_) <- c('C','F','day','T2','heat','cold')


df_$C <- c(1:136)

df_$F <- rep(seq(c(1:144)), each = 136)


df_cf <- df_[rep(seq_len(nrow(df_)), each = 365), ]

df_cf$day <- c(1:365)
row.names(df_cf) <- NULL

T2 <- vector()
for (j in c(1:144)) {
  for (i in c(1:136)) {
    
    T2 <- append(T2,varr[i,j,1:365])
    
  }
  print(j)}

df_cf$T2 <- T2 

#By the moment, MMT = 21.4 ?C

MMT = 21.4


df_cf$heat <- sapply(df_cf$T2, function(x) max((x-MMT),0))
df_cf$cold <- sapply(df_cf$T2, function(x) max((MMT-x),0))

df_final_year <- data.frame(matrix(ncol = 4, nrow = 19584))
colnames(df_final_year) <- c('C','F','heat','cold')

df_final_year$C <- c(1:136)

df_final_year$F <- rep(seq(c(1:144)), each = 136)

df_final_year$C_F <- paste0(df_final_year$C,'_',df_final_year$F)
df_cf$C_F <- paste0(df_cf$C,'_',df_cf$F)


heat <- vector()
cold <- vector()
for (ref in unique(df_cf$C_F)) {
  df = df_cf[df_cf$C_F == ref, ]
  heat = append(heat,sum(df$heat)/365)
  cold = append(cold,sum(df$cold)/365)
}

df_final_year$heat <- heat
df_final_year$cold <- cold

write.csv(df_final_year, 'df__MAD_meteo_2015_SVR_Daily_Esc_Futuro2.csv')
df__MAD_meteo_2015_SVR_Daily_Esc_BASE <- read.csv('df__MAD_meteo_2015_SVR_Daily_Esc_Futuro.csv')
