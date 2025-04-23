#' Adaugă 'ora' încât oricare două lecții prof|cls|ora să nu se suprapună
#' @param LSS data.frame cu lecțiile prof|cls, unde 'prof' este un 
#'     profesor propriu-zis, sau unul fictiv (cuplaj de doi/clasă)
#' @param TPL data.frame pentru tuplaje, dacă este cazul
#'     Un tuplaj conține 2 sau mai mulți profesori, pe 2 sau mai multe clase
#'     (numărul de profesori fiind cel mult cu 1 mai mare, ca al claselor)
#' @return Un orar prof|cls|ora pentru ziua respectivă
#' @export
#'
#' @examples
#'     STP <- as.data.frame(mount_hours(LSS, Tuplaje)) %>% 
#'            dplyr::arrange(cls, ora)
#'

mount_hours <- function(LSS, TPL = NULL) {
    P23 <- ZTP <- ZTC <- NULL
    # Dacă s-a furnizat și un set TPL de tuplaje, atunci verifică
    # dacă on_tuples(TPL) a înființat vreun cuplaj; dacă da, atunci
    # adaugă lecțiile acestuia în LSS
    if(!is.null(TPL)) {
        lTP <- on_tuples(TPL)
        P23 <- lTP[[1]]  # tuplajele, în care  eventual, s-au înființat cuplaje
        ZTP <- lTP[[2]]  # profesorii (inclusiv, noile cuplaje) din TPL
        ZTC <- lTP[[3]]  # clasele implicate în TPL
        it <- which(nchar(ZTP) == 6)  # noul cuplaj trebuie adăugat în 'LSS'
        ADF <- data.frame(prof="", cls="")
        if(length(it) > 0) {
            for(PP in ZTP[it]) {
                    DT <- P23 %>% filter(grepl(PP, .data$prof))
                    for(i in 1:nrow(DT)) {
                        pr <- strsplit(DT[i, 1], " ")[[1]]
                        cl <- strsplit(DT[i, 2], " ")[[1]]
                        ADF <- rbind(ADF, data.frame(prof = pr, cls = cl))
                    }
            } 
        } else ADF <- rbind(ADF, data.frame(prof = ZTP, cls = ZTC))
        if(nrow(ADF) > 1) 
            LSS <- rbind(LSS, ADF[2:nrow(ADF), ])
    }
    # STOP dacă un profesor sau o clasă cumulează mai mult de 7 ore
    stopifnot("you have a Teacher (prof) with more than 7 hours" = 
              all(table(LSS$prof) <= 7))
    stopifnot("you have a Class (cls) with more than 7 hours" = 
              all(table(LSS$cls) <= 7))

    # LSS conține acum și cuplajele nou înființate; get_twins(LSS) ne dă
    # vectorii care indică de care profesor/cuplaj depinde alocarea pe ore
    # a lecțiilor cuplajelor și profesorilor angajați în cuplaje
    tw <- get_twins(LSS)
    TW1 <- tw[[1]]; TW2 <- tw[[2]]
    Twinz <- union(names(TW1), names(TW2))  # toți care țin de cuplaje
    # Alocarea pe ore a lecțiilor din LSS (ținând seama și de TPL) va 
    # decurge pe clase, într-o ordine a claselor aleatorie, dar ponderată
    # de coeficienții returnați de scale_prof_cls(LSS) 
    BTW <- scale_prof_cls(LSS)
    
    # Pentru o clasă implicată în tuplaje, determină setul alocărilor
    # existente la momentul apelării, pentru lecțiile acesteia
    ph_in_tuplaj <- function(Q) {
        p23 <- z23 %>% filter(grepl(Q, .data$cls))  # z23 = P23 curent
        map_dfr(1:nrow(p23), function(nr) {
            i <- match(Q, strsplit(p23[nr, 2], " ")[[1]])
            vpr <- strsplit(p23[nr, 1], " ")[[1]]
            data.frame(prof = vpr[i], ora = p23[nr, 3])
        })
    }
    # Actualizează câmpul 'ora' din setul curent al tuplajelor 
    set_h_tuplaj <- function(Q, P, h) {
        if(h %in% z23[which(grepl(Q, z23[,2])), 3]) return(FALSE)
        z23[which(grepl(Q, z23[,2]) & grepl(P, z23[,1])), 3] <<- h
        TRUE
    }
    #       Please NOTE: 
    # '<<-' will affect (changes) the environment of the function from which
    # this internal function is called, NOT the global-environment.
    
    # Împarte LSS după clasă, în ordinea 'betweenness' a profesorilor 
    task <- LSS %>%
            mutate(prof = factor(.data$prof, 
                   levels = names(BTW$prof), ordered=TRUE)) %>%
            dplyr::arrange(.data$prof, .data$cls)  # după BTW$prof
    Z <- split(task, ~cls)
    lstCls <- names(Z)  # vectorul claselor (de ordonat aleatoriu după BTW$cls)

    # Asociem profesorilor și cuplajelor câte un octet în care 
    # vom înregistra pe biții 0:6 orele 1:7 curent alocate acestora
    hBits <- rep(0L, nlevels(task$prof))
    names(hBits) <- levels(task$prof)

    # Alocă pe orele 1..7, lecţiile unei clase 
    mountHtoCls <- function(Q) {
        mpr <- PERM[[nrow(Q)-3]]  # matricea de permutări a orelor clasei
        bhp <- bith[Q$prof]  # biţii alocaţi anterior, profesorilor clasei
        dP <- ""
        if(!is.null(pth))  # 'pth' este setat din afară (v. mai jos)
            dP <- pth$prof  # cei din tuplaje cu ore la clasa Q
        for(i in sample(ncol(mpr))) {  # parcurge aleatoriu permutările de ore
            po <- mpr[, i]  # permutare de 1:7
            bis <- bitwShiftL(1L, po - 1L)  # permutare de indecși 0:6
            if(any(bitwAnd(bhp, bis) > 0L)) 
                next  # caută o permutare care să evite biţi '1' alocaţi deja
            
            knd <- TRUE  # pentru a semnala suprapuneri de lecții
            for(k in 1:nrow(Q)) {  # vizează pe cei angajaţi în cuplaje/tuplaje
                P <- as.character(Q$prof[k])
                if(P %in% Twinz) {  # angajat în cuplaje
                    bt <- if(nchar(P) > 3) TW2[[P]] else TW1[[P]]
                    BC <- bitwAnd(bith[bt], bis[k])
                    if(any(BC > 0)) {  # suprapunere de profesori/cuplaje
                        knd <- FALSE
                        break  # caută o altă permutare de ore
                    }
                }
                if(P %in% dP) {  # angajat în tuplaje
                   h <- pth[pth[, 1]==P, 2]  # valoarea curentă din $ora
                   if(h > 0) {  # există o alocare anterioară
                        bs <- h2bin[h]
                        if(bitwAnd(bis[k], bs) == 0L) {
                            knd <- FALSE  # suprapunere cu alocarea anterioară
                            break  # caută o altă permutare de ore
                        }
                   } else {  # înscrie în câmpul 'ora' a tuplajului
                        if(!set_h_tuplaj(Q$cls[1], P, 
                                         which(h2bin == bis[k]))) {
                            knd <- FALSE  # suprapunere de lecții
                            break  # caută o altă permutare de ore
                        }
                   }    
               }
            }
            if(!knd) next  # trece la o altă permutare de ore
            
            # Pentru un profesor cu două ore la o aceeași clasă,
            # biții de alocare s-ar suprapune
            if(anyDuplicated(names(bhp)) > 0)  # profesor cu 2 ore la clasă
                for(jn in 1:(nrow(Q)-1))  # cumulează biţii asociaţi orelor
                    if(names(bhp)[jn] == names(bhp)[jn+1]) 
                        bis[jn] <- bis[jn+1] <- bis[jn] + bis[jn+1]
            blks <- bitwOr(bhp, bis)   # biţii vechilor şi noii alocări

            bith[Q$prof] <<- blks  # actualizează vectorul alocărilor
            return(Q %>% mutate(ora = po))  # orarul clasei curente
        }
        return(NULL)  # pentru clasa curentă NU s-a reuşit un orar corect
    } # END mountHtoCls()
    
    # programul "principal", pregătit de mai sus
    odf <- vector("list", length(lstCls))  # unde înscriem orarele claselor
    names(odf) <- lstCls
    # inr <- 0  # contorizează eventual, încercările nereușite
    while(TRUE) {
        succ <- TRUE
        bith <- hBits  # reiniţializează vectorul alocărilor
        lstCls <- sample(lstCls, prob = BTW$cls)  # ordine aleatorie ponderată 
        z23 <- P23  # tuplajele rezultate la început, din on_tuples(LSS)
        for(K in lstCls) {  ##  cat("*")  # ecou pe ecran (la clasa curentă)
            if(K %in% ZTC)  # clasă implicată în tuplaje;
                pth <- ph_in_tuplaj(K)  # alocarea actuală a lecțiilor acesteia
            else {pth <- NULL}
            W <- mountHtoCls(Z[[K]])  # încearcă un orar pentru clasa curentă
            if(is.null(W)) {  ## cat(" / ")  # eșuează la clasa curentă
                # inr <- inr + 1 
                succ <- FALSE 
                break  # ...ceea ce va abandona 'for', reluând 'while'
            }
            odf[[K]] <- W  # salvează orarul constituit clasei curente
        }
        if(succ) break  # s-a reușit alocarea pentru toate clasele
    }
    # cat(inr, "attempts\n")  # numărul de încercări (nereușite)
    dplyr::bind_rows(odf)  # returnează orarul lecţiilor (prof|cls|ora)
}


