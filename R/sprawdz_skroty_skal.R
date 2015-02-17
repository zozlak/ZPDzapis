#' @title Sprawdza poprawnosc i tworzy w bazie brakujace skroty skal
#' @description
#' _
#' @param skroty wektor wartosci id_skrotu
#' @param P obiekt otwartego polaczenie ODBC
#' @return [logical] TRUE w wypadku powodzenia funkcji
sprawdz_skroty_skal = function(skroty, P){
	skrotyBaza = .sqlQuery(P, "SELECT id_skrotu FROM skroty_skal")[, 1]
	skroty = gsub(',', '.', skroty)
	tmp = unique(skroty[!(skroty %in% skrotyBaza) & !is.na(skroty)])
	tmp = strsplit(tmp, '[|]')
	for(i in tmp){
		idSkrotu = paste(i, collapse = '|')
		i = strsplit(i, '[;]')
		if(length(i) != 2){
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - nie podano wartosci wyjsciowych', idSkrotu))
		}
		we = suppressWarnings(as.numeric(i[[1]]))
		wy = suppressWarnings(as.numeric(i[[2]]))
		if(length(we) != length(wy)){
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - rozne liczby wartosci wejsciowych i wyjsciowych', idSkrotu))
		}
		if(any(is.na(we) | is.na(wy))){
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - co najmniej jedna z wartosci wejsciowych lub wyjsciowych nie jest liczba', idSkrotu))
		}
		.sqlQuery(P, "INSERT INTO skroty_skal (id_skrotu) VALUES (?)", idSkrotu)
	  zap = "INSERT INTO skroty_skal_mapowania (id_skrotu, wartosc, nowa_wartosc) VALUES (?, ?, ?)"
		.sqlQuery(P, zap, list(rep(idSkrotu, length(we)), we, wy))
	}
	return(TRUE)
}
