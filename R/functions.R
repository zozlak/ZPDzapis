#' @title Wyparsowuje znaki konca stringu
#' @description
#' Funkcja do wyparsowywania niebezpiecznych znakow przy budowaniu zapytan SQL
#' @details
#' _
#' @param str lancuch znakow do wyparsowania
#' @return character
.e = function(str){
	return(gsub("'", "''", str))
}

#' @title Wykonuje zapytanie sql i obsluguje bledy
#' @description
#' _
#' @details
#' _
#' @param P otwarte polaczenie ODBC
#' @param sql polecenie SQL do wykonania
#' @return data.frame
#' @import RODBCext
.sqlQuery = function(P, sql, dane = NULL){
	odbcClearError(P)
	tmp = sqlExecute(P, sql, dane, fetch = T, errors = F, stringsAsFactors = F, dec = '.')
	if(!is.data.frame(tmp)){
		if(tmp[1] == -2){
			return(NULL) # brak danych
		}
	}
	blad = odbcGetErrMsg(P)
	if(length(blad) > 0){
		stop(paste0(blad, collapse = '\n'))
	}
	return(tmp)
}

#' @title Ponownie rzuca przekazanym wyjatkiem zachowujac jego pierwotne wywolanie
#' @description
#' _
#' @details
#' _
#' @param e wyjatek przechwycony funkcja tryCatch()
#' @return void
.stop = function(e){
	stop(paste(deparse(conditionCall(e)), conditionMessage(e), sep = '\n'), call. = F)
}
