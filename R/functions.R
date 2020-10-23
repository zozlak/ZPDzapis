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
#' @description _
#' @details _
#' @param P połączenie z bazą danych uzyskane z
#'   \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param sql polecenie SQL do wykonania
#' @param dane ramka danych z danymi, które mają zostać wykorzystane w zapytaniu
#' @return data.frame
.sqlQuery = function(P, sql, dane = NULL){
  zap = DBI::dbSendQuery(P, sql)
  if (!is.null(dane)) {
    if (!is.null(nrow(dane))) {
      res = vector('list', nrow(dane))
      for (i in seq_len(nrow(dane))) {
        DBI::dbBind(zap, unname(as.list(dane[i, ])))
        res[[i]] = suppressWarnings(DBI::dbFetch(zap))
      }
      return(dplyr::bind_rows(res))
    } else {
      DBI::dbBind(zap, unname(as.list(dane)))
      return(suppressWarnings(DBI::dbFetch(zap)))
    }
  } else {
    return(suppressWarnings(DBI::dbFetch(zap)))
  }
}

#' @title Zwraca placeholdery parametrów zapytania SQL
#' @description _
#' @param dane cokolwiek, na czym zadziała \code{length()}
#' @param start numer pierwszego placeholdera
#' @returns character
.sqlPlaceholders = function(dane, start = 1) {
  n = seq(start, start + length(dane) - 1)
  return(paste0('$', n, collapse = ', '))
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
