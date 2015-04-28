#' @title Tworzy w bazie nowa skale i zwraca jej id_skali
#' @description
#' _
#' @param opis opis skali (zwyczajowo w formacie "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub "ewd;gh_h;2012")
#' @param rodzaj rodzaj skali (ewd/zrównywanie/ktt)
#' @param doPrezentacji czy skala ma być oznaczona jako przeznaczona do prezentacji
#' @param idTestow wektor id testów, z którymi ma być powiązana skala
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [numeric] id_skali utworzonej skali
#' @export
#' @import RODBCext
stworz_skale = function(
	opis, 
	rodzaj,
	doPrezentacji,
	idTestow, 
	zrodloDanychODBC = 'EWD'
){
  stopifnot(
    is.vector(opis), is.character(opis), length(opis) == 1, !is.na(nazwa), opis != '', 
    is.vector(rodzaj), is.character(rodzaj), length(rodzaj) == 1, !is.na(rodzaj),
    is.vector(doPrezentacji), is.logical(doPrezentacji), length(doPrezentacji) == 1, !is.na(doPrezentacji),
    is.vector(idTestow), is.numeric(idTestow),
    is.vector(zrodloDanychODBC), is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  P = odbcConnect(zrodloDanychODBC)
	on.exit({
	  odbcClose(P)
	})
  
  idTestow = na.exclude(idTestow)
  stopifnot(
    ! opis %in% .sqlQuery(P, "SELECT DISTINCT opis FROM skale")[, 1],
    rodzaj %in% .sqlQuery(P, "SELECT rodzaj_skali FROM sl_rodzaje_skal")[, 1],
    length(idTestow) > 0,
    all(idTestow %in% .sqlQuery(P, "SELECT id_testu FROM testy")[, 1])
  )
  
	odbcSetAutoCommit(P, FALSE)
	.sqlQuery(P, "BEGIN")
		
  idSkali = .sqlQuery(P, "SELECT nextval('skale_id_skali_seq')")[1, 1]
  zap = "INSERT INTO skale (id_skali, opis, rodzaj_skali, do_prezentacji) VALUES (?, ?, ?, ?, ?)"
  .sqlQuery(P, zap, list(idSkali, opis, rodzaj, doPrezentacji))
  zap = "INSERT INTO skale_testy (id_skali, id_testu) VALUES (?, ?)"
  .sqlQuery(P, zap, list(rep(idSkali, length(idTestow)), idTestow))
  
  odbcEndTran(P, TRUE)
	return(idSkali)	
}
