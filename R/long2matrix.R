#' Transformă prof|cls|ora în matricea profesorilor pe orele 1:7
#'
#' "Matricea-orar" are liniile numite după profesori, fiecare linie conținând
#' clasele la care intră profesorul respectiv, în orele 1:7. Fiecare clasă
#' apare câte o singură dată pe fiecare coloană de rang mai mic sau egal cu
#' numărul de ore al clasei respective. 
#'
#' @param ORR data.frame prof|cls|ora (format lung)
#' @return matricea orară a lecțiilor: clasele alocate pe ore fiecărui profesor
#' @export
#' 
#' @examples
#' \donttest{
#'     LM <- long2matrix(as.data.frame(mount_hours(LSS, Tuplaje)))
#' }

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

#' Verifică matricea-orar
#'
#' Fiecare clasă trebuie să apară cel mult o singură dată, pe fiecare coloană.
#'
#' @param MP Matricea-orar 
#' @return TRUE dacă fiecare clasă apare cel mult o dată, pe fiecare coloană
#'     (FALSE în caz contrar)
#' @export
#'

verify_matrix <- function(MP) {
    vrf <- apply(MP, 2, table)
    tf <- sapply(1:length(vrf), function(i) any(vrf[[i]][-1] > 1))
    all(tf == FALSE)
}
