#' Adaptează setul tuplajelor în vederea alocării în câte o aceeași oră
#'     a lecțiilor dintr-un același tuplaj
#'
#' Dacă numărul de profesori este cu 1 mai mare ca al claselor, atunci
#' se înființează un cuplaj pentru primii doi din tuplajul respectiv.
#'
#' @param TPL 'data.frame' conținând tuplajele prof|cls
#'     (separate cu un spațiu, pe fiecare câmp, dacă este cazul)
#' @return NULL dacă setul TPL este defectuos; altfel, o listă conținând
#'     setul prof|cls|ora ('ora' fiind inițializată cu 0), împreună cu
#'     doi vectori: profesorii și respectiv clasele, din tuplaje
#'
#' @keywords internal
#'
on_tuples <- function(TPL) {
    Lp <- nchar(TPL$prof)  # lungimile șirurilor din coloana 'prof'
    if(any(Lp > 15)) 
        return(NULL)  # este nefiresc un tuplaj cu peste 4 profesori
    Cls <- gsub("( |\\b)([0-9]{1}[A-Z]{1})( |\\b)", "\\1i\\2\\3", 
                TPL$cls)  # prefixează cu 'i' numele 'cls' de lungime 2  
    Lc <- nchar(Cls)
    if(any(Lp - Lc) > 4) 
        return(NULL)  # un tuplaj ar putea avea cel mult, un cuplaj
    for(i in which(Lp - Lc == 4))  # înființează un cuplaj, la primii doi
        TPL$prof[i] <- sub(" ", "", TPL$prof[i])
    t_prof <- lapply(TPL$prof, function(V) strsplit(V, " ")[[1]]) %>% 
              unlist() %>% as.vector() %>% unique()
    t_cls <- lapply(TPL$cls, function(V) strsplit(V, " ")[[1]]) %>% 
             unlist() %>% as.vector() %>% unique()
    TPL <- TPL %>% mutate(ora = 0L)
    list(TPL, t_prof, t_cls)
}

