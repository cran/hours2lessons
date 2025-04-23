#' Depistează cuplajele și dependențele de alocare pe ore, induse de acestea
#' @param LSS data.frame cu lecțiile prof|cls
#' @return NULL dacă nu există cuplaje; altfel, lista care indică bilateral,
#'     pe fiecare membru al unui cuplaj și pe fiecare cuplaj, profesorii
#'     și cuplajele de care depinde alocarea pe ore a lecțiilor sale.
#' @keywords internal
#'

get_twins <- function(LSS) {
    if(all(nchar(LSS$prof) == 3)) return(NULL)
    P36 <- split(LSS, ~ nchar(LSS$prof)) %>%
           map(function(LS) LS %>% pull(.data$prof) %>% unique() %>% sort())
    P3 <- P36[[1]]
    P6 <- P36[[2]]
    Tw1 <- map(P3, function(P) P6[grepl(P, P6)]) %>%
           setNames(P3) %>% purrr::compact()
    Tw2 <- map(P6, function(PP) {
         sdf <- "" 
         p1 <- substr(PP, 1, 3)
         if(p1 %in% P3)  # altfel, 'p1' este "extern" (nu are ore proprii)
            sdf <- c(p1, Tw1[[p1]])
         p2 <- substr(PP, 4, 6)
         if(p2 %in% P3)  # altfel, 'p2' este "extern"
            sdf <- if(length(sdf) == 1) c(p2, Tw1[[p2]]) 
                   else c(sdf, p2, Tw1[[p2]])
         sdf <- sdf[sdf != ""]
         setdiff(sdf, PP) %>% unique() 
    }) %>% setNames(P6) %>% purrr::compact()
    list(Tw1, Tw2)
}
