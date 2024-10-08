#' @title Tworzy nowy test (także jako kompilację wskazanych testów)
#' @description
#' _
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param ewd czy będzie to test ewd [logical]
#' @param opis opis testu
#' @param data data testu
#' @param rodzajEgzaminu rodzaj egzaminu powiązany z testem (lub NA)
#' @param czescEgzaminu część egzaminu powiązana z testem (lub NA)
#' @param idTestow wektor id_testu testów, których kryteria oceny mają zostać skopiowane do tworzonego testu
#' @return [numeric] id_testu utworzonego testu
#' @export
#' @importFrom stats na.exclude
stworz_test = function(
  P,
	ewd,
	opis,
	data,
  rodzajEgzaminu = NA_character_,
	czescEgzaminu = NA_character_,
	idTestow = NULL
){
  stopifnot(
    is.vector(ewd), is.logical(ewd), length(ewd) == 1, !is.na(ewd),
    is.vector(opis), is.character(opis), length(opis) == 1, !is.na(opis),
    is.vector(data), length(data) == 1, all(!is.na(data)), class(as.Date(data)) == 'Date',
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    is.vector(czescEgzaminu), is.character(czescEgzaminu), length(czescEgzaminu) == 1,
    is.null(idTestow) | is.vector(idTestow) & is.numeric(idTestow)
  )

  DBI::dbBegin(P)

	idTestu = as.integer(.sqlQuery(P, "SELECT nextval('testy_id_testu_seq')")[1, 1])
	.sqlQuery(P,
		"INSERT INTO testy (id_testu, opis, data, ewd, rodzaj_egzaminu, czesc_egzaminu) VALUES ($1, $2, $3, $4, $5, $6)",
		list(idTestu, opis, data, ewd, rodzajEgzaminu, czescEgzaminu)
	)

	if(!is.null(idTestow)){
		idTestow = na.exclude(idTestow)
		if(length(idTestow) > 0){
			.sqlQuery(P, "CREATE TEMPORARY SEQUENCE tmp_kolejnosc")
		  zap = sprintf(
		    "INSERT INTO testy_kryteria (id_testu, id_kryterium, kolejnosc)
					SELECT %d, t.*, nextval('tmp_kolejnosc')
					FROM
						(
							SELECT DISTINCT id_kryterium
							FROM testy_kryteria
							WHERE id_testu IN (%s)
              ORDER BY 1
						) AS t",
		    idTestu,
		    paste0(idTestow, collapse = ', ')
		  )
			.sqlQuery(P, zap)
		}
	}

	DBI::dbCommit(P)

	return(idTestu)
}
