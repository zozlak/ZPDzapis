#' @title Zapisuje elementy skali do bazy danych
#' @description
#' Patrz http://zpd.ibe.edu.pl/doku.php?id=r_zpd_skale
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param idSkali identyfikator skali, ktora ma zostac zapisana (typowo uzyskany z funkcji "stworz_skale()")
#' @param elementy ramka danych opisujaca elementy skali - patrz http://zpd.ibe.edu.pl/doku.php?id=r_zpd_skale
#' @param nadpisz czy nadpisac skale, jesli jest juz zdefiniowana w bazie danych
#' @return [data.frame] zapisane elementy skali
#' @export
#' @importFrom stats na.exclude
edytuj_skale = function(
  P,
	idSkali,
	elementy,
	nadpisz = FALSE
){
  stopifnot(
    is.vector(idSkali), is.numeric(idSkali), length(idSkali) == 1, !is.na(idSkali),
    is.data.frame(elementy),
    !is.null(elementy$id_kryterium),
    !is.null(elementy$id_pseudokryterium),
    !is.null(elementy$id_skrotu),
    all(as.numeric(!is.na(elementy$id_kryterium)) + as.numeric(!is.na(elementy$id_pseudokryterium)) <= 1)
  )
  kryteria = as.matrix(elementy[, grep('^id_kryterium_[0-9]+$', names(elementy))])
  if (any((!is.na(elementy$id_kryterium) | !is.na(elementy$id_pseudokryterium)) & rowSums(!is.na(kryteria)) > 0)) {
    stop('dla niektorych elementow skali zdefiniowano jednoczesnie id_kryterium/id_pseudokryterium, jak i wartosci w kolumnach id_kryterium_N')
  }

  DBI::dbBegin(P)

	kolOpis = grep('^opis$', names(elementy))
	#<-- na wypadek factor-ow
	if (ncol(kryteria) > 0) {
	  for (i in seq_len(ncol(kryteria))) {
	    kryteria[, i] = as.numeric(as.character(kryteria[, i]))
	  }
	}
	elementy$id_kryterium = as.numeric(as.character(elementy$id_kryterium))
	elementy$id_pseudokryterium = as.numeric(as.character(elementy$id_pseudokryterium))
	elementy$opis = as.character(elementy$opis)
	elementy$id_skrotu = as.character(elementy$id_skrotu)
	#-->

	#<-- testy zwiazane ze skala
  stopifnot(
    idSkali %in% .sqlQuery(P, "SELECT id_skali FROM skale")[, 1],
    nadpisz | !idSkali %in% .sqlQuery(P, "SELECT DISTINCT id_skali FROM skale_elementy")[, 1]
  )
	if (any(
	  0 != .sqlQuery(P, "SELECT count(*) FROM skalowania_elementy WHERE id_skali = $1", idSkali)[1, 1],
	  0 != .sqlQuery(P, "SELECT count(*) FROM skalowania_obserwacje WHERE id_skali = $1", idSkali)[1, 1]
	)) {
	  stop('nie mozna edytowac skali - ma ona juz wpisane do bazy parametry zadan i/lub estymacje umiejetnosci uczniow')
	}
	#-->

	krytBaza = .sqlQuery(P, "SELECT id_kryterium FROM kryteria_oceny")[, 1]
	pkrytBaza = pobierz_pseudokryteria(P)
	#<-- weryfikacja elementow skali - istnienie id_kryterium/id_pseudokryteriu w bazie
	if (ncol(kryteria) > 0) {
	  for (i in seq_len(ncol(kryteria))) {
	    tmp = !is.na(kryteria[, i]) & !(kryteria[, i] %in% krytBaza)
	    if (any(tmp)) {
	      stop("kryterium o id_kryterium ", paste(kryteria[tmp, i], collapse = ', '), " nie ma w bazie")
	    }
	  }
	}
	tmp = !is.na(elementy$id_kryterium) & !(elementy$id_kryterium %in% krytBaza)
	if (any(tmp)) {
	  stop("kryterium/ow o id_kryterium ", paste(elementy$id_kryterium[tmp], collapse = ', '), " nie ma w bazie")
	}
	tmp = !is.na(elementy$id_pseudokryterium) & !(elementy$id_pseudokryterium %in% names(pkrytBaza))
	if (any(tmp)) {
	  stop("pseudokryterium/ow o id_pseudokryterium ", paste(elementy$id_pseudokryterium[tmp], collapse = ', '), " nie ma w bazie")
	}
	rm(tmp)
	#-->

	#<-- odnajdowanie id_pseudokryterium i ew. tworzenie pseudokryteriow
	tmp = znajdz_pseudokryteria(P, kryteria, elementy$opis)
	elementy$id_pseudokryterium[!is.na(tmp)] = tmp[!is.na(tmp)]
	#-->

	#<-- weryfikacja istnienia kryteriów/pseudokryteriów w testach powiązanych ze skalą
	tmp = .sqlQuery(P, "SELECT id_pseudokryterium, id_kryterium FROM pseudokryteria_oceny_kryteria")
	tmp = tmp$id_kryterium[tmp$id_pseudokryterium %in% na.exclude(elementy$id_pseudokryterium)]
	kryt = unique(na.exclude(append(elementy$id_kryterium, tmp)))
	tmp = .sqlQuery(P, "SELECT id_kryterium FROM testy_kryteria JOIN skale_testy USING (id_testu) WHERE id_skali = $1", idSkali)[, 1]
	if (any(is.na(match(kryt, tmp)))) {
	  stop("nie wszystkie (pseudo)kryteria oceny skali pochodzą z testów powiązanych ze skalą")
	}
  rm(tmp)
	#-->

	#<-- tworzenie brakujacych skrotow skal
	sprawdz_skroty_skal(elementy$id_skrotu, P)
	#-->

	#<-- duplikaty
	tmp = duplicated(elementy$id_kryterium) & !is.na(elementy$id_kryterium)
	if (any(tmp)) {
	  stop(sprintf('kryteria o id_kryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_kryterium[tmp], collapse = ',')))
	}
	tmp = duplicated(elementy$id_pseudokryterium) & !is.na(elementy$id_pseudokryterium)
	if (any(tmp)) {
	  stop(sprintf('pseudokryteria o id_pseudokryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_pseudokryterium[tmp], collapse = ',')))
	}
	rm(tmp)
	#-->

	if (nadpisz) {
	  .sqlQuery(P, "DELETE FROM skale_elementy WHERE id_skali = $1", idSkali)
	}

	#<-- zapis do bazy
  zap = "INSERT INTO skale_elementy (kolejnosc, id_skali, id_kryterium, id_pseudokryterium, id_skrotu) VALUES ($1, $2, $3, $4, $5)"
  tmp = data.frame(
    seq_len(nrow(elementy)),
    rep(idSkali, nrow(elementy)),
    elementy$id_kryterium,
    elementy$id_pseudokryterium,
    elementy$id_skrotu
  )
  .sqlQuery(P, zap, tmp)
	#-->

	DBI::dbCommit(P)
	return(elementy)
}
