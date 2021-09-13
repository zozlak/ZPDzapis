#' @title Tworzy w bazie nowa skale i zwraca jej id_skali
#' @description
#' Funkcja stworz_skale() służy do „zarejestrowania” w bazie nowej skali.
#' Po dokonaniu takiego „zarejestrowania” należy wprowadzić do bazy informacje
#' o kryteriach oceny i pseudokryteriach oceny, z których składa się dana skala
#' - służy do tego funkcja \code{\link{edytuj_skale}}.
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param opis opis skali (zwyczajowo w formacie "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub "ewd;gh_h;2012")
#' @param rodzaj rodzaj skali (ewd/zrównywanie/ktt)
#' @param doPrezentacji czy skala ma być oznaczona jako przeznaczona do prezentacji
#' @param idTestow wektor id testów, z którymi ma być powiązana skala
#' @param pominTransakcje wartość logiczna pozwalająca wywołać funkcję tak, aby nie używała
#' transakcji - co do zasady nie należy stosować (przydatne tylko, jako sposób na uniknięcie
#' błędu DBI/RPostgres związanego z brakiem obsługi zagnieżdżonych transakcji)
#' @return [numeric] id_skali utworzonej skali
#' @export
#' @importFrom stats na.exclude
stworz_skale = function(
  P,
	opis,
	rodzaj,
	doPrezentacji,
	idTestow,
  pominTransakcje
){
  stopifnot(
    is.vector(opis), is.character(opis), length(opis) == 1, opis != '',
    is.vector(rodzaj), is.character(rodzaj), length(rodzaj) == 1, !is.na(rodzaj),
    is.vector(doPrezentacji), is.logical(doPrezentacji), length(doPrezentacji) == 1, !is.na(doPrezentacji),
    is.vector(idTestow), is.numeric(idTestow),
    is.logical(pominTransakcje), length(pominTransakcje) == 1,
    pominTransakcje %in% c(TRUE, FALSE)
  )

  idTestow = na.exclude(idTestow)
  stopifnot(
    !(opis %in% .sqlQuery(P, "SELECT DISTINCT opis FROM skale")[, 1]),
    rodzaj %in% .sqlQuery(P, "SELECT rodzaj_skali FROM sl_rodzaje_skal")[, 1],
    length(idTestow) > 0,
    all(idTestow %in% .sqlQuery(P, "SELECT id_testu FROM testy")[, 1])
  )

  if (!pominTransakcje) DBI::dbBegin(P)

  idSkali = as.integer(.sqlQuery(P, "SELECT nextval('skale_id_skali_seq')")[1, 1])
  zap = "INSERT INTO skale (id_skali, opis, rodzaj_skali, do_prezentacji) VALUES ($1, $2, $3, $4)"
  .sqlQuery(P, zap, list(idSkali, opis, rodzaj, doPrezentacji))
  zap = "INSERT INTO skale_testy (id_skali, id_testu) VALUES ($1, $2)"
  .sqlQuery(P, zap, data.frame(idSkali, idTestow))

  if (!pominTransakcje) DBI::dbCommit(P)
	return(idSkali)
}
