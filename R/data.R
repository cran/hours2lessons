#' Exemplu de lecții, cu 62 profesori (între care, două cuplaje) și 32 clase
#'
#' set numit "LSS" de 204 lecții prof|cls, care trebuie să se desfășoare 
#' într-o zi. În ziua respectivă, fiecare profesor are cel mult 7 lecții;
#' fiecare clasă are cel puțin 4 și cel mult 7 lecții.
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
"LSS"
#'
#' Exemplu de tuplaje
#'
#' set numit "Tuplaje", conținând 4 tuplaje asociate setului "LSS". Un tuplaj 
#' va angaja într-o aceeași oră, mai mulți profesori și mai multe clase.
#' Lecțiile tuplate NU sunt înregistrate în "LSS".
#'
#' @format
#' \describe{
#'     \item{prof}{Profesorii care trebuie să intre într-o aceeași oră a zilei
#'         la câte una dintre clasele respective (separați printr-un spațiu).}
#'     \item{cls}{Clasele care trebuie tuplate (separate printr-un spațiu).}
#'}
"Tuplaje"

