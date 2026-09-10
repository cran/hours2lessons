## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hours2lessons)

## -----------------------------------------------------------------------------
str(dayLessons)  # un exemplu de set de lecții
dayLessons %>% dplyr::filter(nchar(prof)==6)  # listează cuplajele existente

## -----------------------------------------------------------------------------
dayTuples

## -----------------------------------------------------------------------------
mount_hours(dayLessons, dayTuples) %>% as.data.frame() %>%
    dplyr::filter(grepl("Fr|Gr|Ds|Mz", .$prof)) %>% 
    long2matrix() %>% as.data.frame()

