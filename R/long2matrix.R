#' Transformă prof|cls|ora în matricea profesorilor pe orele 1:7
#' @param ORR data.frame prof|cls|ora (format lung)
#' @return matricea orară a lecțiilor: clasele alocate pe ore fiecărui profesor
#' @export
#' 
#' @examples
#' LM <- long2matrix(as.data.frame(mount_hours(LSS, Tuplaje))) %>% 
#'       as.data.frame()
#'

long2matrix <- function(ORR) {
    orz1 <- split(ORR %>% as.data.frame(), ~ prof)
    orz <- map_df(orz1, function(K)
               pivot_wider(K, names_from="ora", values_from="cls"))
    M <- orz[, c('prof', sort(colnames(orz)[-1]))]
    M[is.na(M)] <- '-'
    M <- as.matrix(M)
    row.names(M) <- M[, 1]
    M[, 2:ncol(M)]
}


