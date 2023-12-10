#' @title Odnajduje i tworzy pseudokryteria na podstawie list ich elementow
#' @description
#' _
#' @param kryteria macierz id_kryterium - kazdy wiersz opisuje jedno pseudokryterium
#' @param opisy wektor opisow pseudokryteriow
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @return [numeric vector] wektor odnalezionych i/lub utworzonych id_pseudokryteriow
znajdz_pseudokryteria = function(P, kryteria, opisy){
	kryteria = as.matrix(kryteria) # na wypadek, gdyby "kryteria" byly wektorem
	opisyBaza = .sqlQuery(P, "SELECT opis FROM pseudokryteria_oceny")[, 1]
	pkrytBaza = pobierz_pseudokryteria(P)
	idPkryt = rep(NA, nrow(kryteria))
	sumy = rowSums(!is.na(kryteria))
	if (any(sumy == 1)) {
		stop(sprintf('w wierszu/ach %s zdefiniowano pseudokryterium skladajace sie z jednego kryterium oceny', paste((seq_along(sumy))[sumy], sep = ',')))
	}
	sumy = sumy == 0
	for (i in seq_len(nrow(kryteria))) {
		if (sumy[i]) {
			next
		}
		wiersz = sort(kryteria[i, ])
		tmp = match(paste(wiersz[order(wiersz)], collapse = '|'), pkrytBaza)
		if (!is.na(tmp)) {
			idPkryt[i] = as.numeric(names(pkrytBaza)[tmp])
		} else {
			if (is.null(opisy)) {
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak nie podano dla niego opisu', i))
			}
			if (is.na(opisy[i]) | 0 == nchar(gsub(' ', '', opisy[i]))) {
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak nie podano dla niego opisu', i))
			}
			if (opisy[i] %in% opisyBaza) {
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak podany dla niego opis wystepuje juz w bazie', i))
			}

			idPkryt[i] = as.integer(.sqlQuery(P, "SELECT nextval('pseudokryteria_id_pseudokryterium_seq')")[1, 1])
			zap = "INSERT INTO pseudokryteria_oceny (id_pseudokryterium, opis) VALUES ($1, $2)"
			.sqlQuery(P, zap, list(idPkryt[i], opisy[i]))
			for (j in wiersz) {
				if (is.na(j)) {
					next
				}
			  zap = "INSERT INTO pseudokryteria_oceny_kryteria (id_pseudokryterium, id_kryterium) VALUES ($1, $2)"
				.sqlQuery(P, zap, list(idPkryt[i], j))
			}
		}
	}
	return(idPkryt)
}
