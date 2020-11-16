#' @title Zwraca wektor pozwalajacy identyfikowac pseudokryteria
#' @description Nazwy elementow zwracanego wektora to id_pseudokryterium
#' Elementy zwracanego wektora to posortowane rosnaco id_kryterium nalezacych do
#' danego pseudokryterium, zlaczone w lancuch znakow separatorem '|'
#' @details _
#' @param P połączenie z bazą danych uzyskane z
#'   \code{DBI::dbConnect(RPostgres::Postgres())}
#' @return [character] wektor pseudokryteriow znajdujacych sie w bazie
pobierz_pseudokryteria = function(P){
	wynik = .sqlQuery(P, "SELECT id_pseudokryterium, id_kryterium FROM pseudokryteria_oceny_kryteria ORDER BY id_kryterium")
	wynik = by(wynik, wynik$id_pseudokryterium, function(x){
		return(paste(x$id_kryterium, collapse = '|'))
	})
	tmp = names(wynik)
	wynik = as.character(unlist(wynik))
	names(wynik) = tmp
	return(wynik)
}
