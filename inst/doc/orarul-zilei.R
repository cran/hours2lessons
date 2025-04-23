## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hours2lessons)

## -----------------------------------------------------------------------------
str(LSS)  # un exemplu de set de lecții
LSS %>% dplyr::filter(nchar(prof)==6)  # listează cuplajele existente

## -----------------------------------------------------------------------------
Tuplaje

## -----------------------------------------------------------------------------
mount_hours(LSS, Tuplaje) %>% as.data.frame() %>%
    dplyr::filter(grepl("Fr|Gr|Ds|Mz", .$prof)) %>% 
    long2matrix() %>% as.data.frame()

