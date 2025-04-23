#' Scalează profesorii (clasele) după numărul de clase (profesori) în comun
#' @param LSS data.frame cu lecțiile prof|cls
#' @return lista de coeficienți betweenness pentru profesori și clase
#' @keywords internal
#'
scale_prof_cls <- function(LSS) {
    BTW <- list(prof=0, cls=0)
    if(is.factor(LSS$prof))
        LSS <- LSS %>% mutate(prof = as.character(.data$prof))
    cols <- colnames(LSS)
    for(q1 in cols) {
        q2 <- setdiff(cols, q1)
        FxS <- LSS %>% distinct(.data[[q1]]) %>% pull()
        SxF <- map(FxS, function(X) 
                   LSS %>% filter(.data[[q1]] == X) %>%
                   select(all_of(q2)) %>% distinct() %>% pull()) %>%
               setNames(FxS)
        Qn <- names(SxF)
        len <- length(Qn)
        adjm <- matrix(rep(0, len), nrow=len, ncol=len, byrow=TRUE, 
                       dimnames = list(Qn, Qn))
        for(K1 in Qn)
            for(K2 in Qn)
                if(K1 != K2)
                    adjm[K1, K2] <- length(intersect(SxF[[K1]], SxF[[K2]])) 
        G <- graph_from_adjacency_matrix(adjm, mode="undirected")
        BTW[[q1]] <- 0.0001 + 
                     sort(betweenness(G, directed=FALSE, normalized=TRUE))
    }
    BTW
}

