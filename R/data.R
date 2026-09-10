#' Exemplu de lecții, cu 64 profesori (între care, patru cuplaje dintre care două fac parte din tuplaje) și 34 clase
#'
#' set de 205 lecții prof|cls, care trebuie să se desfășoare 
#' în orele 1:7 ale unei zile. Fiecare profesor are cel puțin una și
#' cel mult 7 ore (lecții); fiecare clasă are cel puțin 5 și cel mult 7 ore.
#' Unele lecții (indicate în setul "dayTuples") trebuie să cadă într-o aceeași
#' oră a zilei.
#'
#' @format
#' \describe{
#'     \item{prof}{Cod de 3 sau 6 caractere, reprezentând un profesor,
#'         respectiv un cuplaj (doi profesori, pe grupe ale unei clase);
#'         primele două litere abreviază disciplina pe care este încadrat
#'         profesorul, iar cifra care urmează indexează profesorii
#'         de pe aceeași disciplină}
#'     \item{cls}{Clasa (două sau trei caractere) la care va intra 
#'                profesorul/cuplajul în ziua respectivă} 
#' }
"dayLessons"
#'
#' Exemplu de tuplaje
#'
#' set de 4 tuplaje asociate setului "LSS". Un tuplaj
#' va angaja într-o aceeași oră, mai mulți profesori și mai multe clase, dar
#' un tuplaj corect trebuie să aibă același număr de profesori ca și de clase.
#' Dacă numărul de profesori este totuși mai mare decât al claselor din tuplaj,
#' utilizatorul va ști cel mai bine care dintre profesorii respectiv trebuie
#' cuplați, încât tuplajul să devină corect.
#'
#' @format
#' \describe{
#'     \item{prof}{Profesorii (eventual, cuplați doi pe o aceeași clasă) care
#' trebuie să intre într-o aceeași oră a zilei la câte una dintre clasele
#' respective (separați printr-un spațiu).}
#'     \item{cls}{Clasele care trebuie tuplate (separate printr-un spațiu).}
#'}
"dayTuples"

