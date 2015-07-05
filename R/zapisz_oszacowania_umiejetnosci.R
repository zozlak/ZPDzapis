#' @title Zapisuje oszacowania umiejętności do bazy danych
#' @description
#' Funkcja zapisuje oszacowania umiejętności uczniów do bazy danych.
#' @param oszacowania ramka danych zawierająca oszacowania, błędy standardowe oraz id
#' testu lub testów poszczególnych uczniów.
#' @param skrotCzesci skrócona nazwa częsci egzaminu, np.: gh_p, gh.
#' @param rEAP rzetelność empiryczna.
#' @param rodzajEgzaminu rodzaj egzaminu.
#' @param rokEgzaminu rok egzaminu.
#' @param nrSkalowania numer skalowania dla skali.
#' @param idSkali id skali, z której usuwane są kryteria.
#' @param rodzajEstymacji ciąg znakowy opisujący rodzaj estymacji. Domyślnie przyjmuje
#' wartość 'EAP'.
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @param maskaTestowa wektor liczb określający, które wiersze ramki 'oszacowania' mają
#' być zapisane.
#' @return Funkcja nic nie zwraca
#' @importFrom RODBC odbcConnect odbcClose odbcSetAutoCommit odbcEndTran
#' @import RODBCext
#' @export
zapisz_oszacowania_umiejetnosci = function(oszacowania, skrotCzesci, rEAP, rodzajEgzaminu,
                                           rokEgzaminu, nrSkalowania, idSkali,
                                           rodzajEstymacji = 'EAP',
                                           zrodloDanychODBC = "EWD", maskaTestowa=NULL){
  stopifnot(is.list(oszacowania), is.character(skrotCzesci),
            is.numeric(rEAP) | is.null(rEAP),
            is.character(rodzajEgzaminu),
            is.numeric(rokEgzaminu), is.numeric(nrSkalowania), is.numeric(idSkali),
            is.character(rodzajEstymacji), is.character(zrodloDanychODBC),
            is.numeric(maskaTestowa) | is.null(maskaTestowa))
  
  testy = which(grepl("^id_testu", names(oszacowania)))
  # eliminacja "pustych" kolumn z testami
  for(test in names(oszacowania)[testy]){
    if(all(is.na(oszacowania[[test]]))){
      message("Usunięto kolumnę z testami: ", test)
      oszacowania[[test]] = NULL
    }
  }
  testy = which(grepl("^id_testu", names(oszacowania)))
  
  if(is.null(rEAP)){
    rEAP = 1
  }
  
  tryCatch({
    P = odbcConnect(as.character(zrodloDanychODBC))
    rodzajeEstymacji = sqlExecute(P, "select estymacja from sl_estymacje_obserwacji", fetch = TRUE, stringsAsFactors = FALSE)[[1]]
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )
  
  stopifnot(rodzajEstymacji %in% rodzajEstymacji)
  
  if(length(testy)>1){
    idTestow = unlist(lapply(testy, function(x) { as.vector(na.omit(unique(oszacowania[[x]])))}))
    
    zapytanie = paste0("select distinct arkusze.czesc_egzaminu from testy
                       join arkusze using(arkusz)
                       where id_testu in (", paste0(rep("?", length(idTestow)), collapse =",") ,")")
    
    tryCatch({
      P = odbcConnect(as.character(zrodloDanychODBC))
      czesciEgzaminu = sqlExecute(P, zapytanie, data = data.frame(t(idTestow)), fetch = TRUE, stringsAsFactors = FALSE)[[1]]
      odbcClose(P)
    },
    error=function(e) {
      odbcClose(P)
      stop(e)
    }
    )
    
    if( sum(grepl("^id_testu_gh", names(oszacowania)[testy])) == length(testy) ){
      nazwaEgz = "humanistyczna"
    } else if ( sum(grepl("^id_testu_gm", names(oszacowania)[testy])) == length(testy) ) {
      nazwaEgz = "matematyczno-przyrodnicza"
    } else if(sum(grepl("^id_testu_jpo", names(oszacowania)[testy])) == length(testy)){
      nazwaEgz = "polski"
    } else if(sum(grepl("^id_testu_mat", names(oszacowania)[testy])) == length(testy)){
      nazwaEgz = "matematyka"
    } else if(sum(grepl("^id_testu_[mat|bio|che|fiz|geo|inf]", names(oszacowania)[testy])) == length(testy)){
      nazwaEgz = "matematyczno-przyrodnicza"
    } else if(sum(grepl("^id_testu_[jpo|wos|his]", names(oszacowania)[testy])) == length(testy)){
      nazwaEgz = "humanistyczna"
    } else {
      stop("Nie wszystkie nazwy testów są poprawne: ", paste(names(oszacowania)[testy], collapse = ", "))
    }
    
    opisEgzaminu = paste(rodzajEgzaminu, nazwaEgz, rokEgzaminu, sep=";")
    
    idTestu = suppressWarnings(stworz_test_z_wielu_czesci(rodzajEgzaminu, czesciEgzaminu, rokEgzaminu, czyEwd = TRUE, opis = opisEgzaminu, czescEgzaminuZapisz = NA, zrodloDanychODBC = zrodloDanychODBC))
  } else {
    idTestu = oszacowania[[4]]
  }
  
  czyStdErr = paste0(skrotCzesci,"_se") %in% names(oszacowania)
  
  zapytanie = paste0("INSERT INTO skalowania_obserwacje (grupa, id_testu, id_obserwacji, id_skali, skalowanie, estymacja,  wynik,", ifelse(czyStdErr, " bs,", ""), " nr_pv)
                     VALUES (?, ?, ?, ?, ?, ?, ?,", ifelse(czyStdErr, " ?,", ""), " -1)")
  
  #   insertGrupa = "INSERT INTO skalowania_grupy (id_skali, skalowanie, grupa)  VALUES (? , ? , ?)"
  
  doWstawienia = data.frame(oszacowania$grupa, idTestu, oszacowania$id_obserwacji, idSkali, nrSkalowania,
                            rodzajEstymacji, oszacowania[[skrotCzesci]]/rEAP)
  
  if(czyStdErr){
    doWstawienia = cbind(doWstawienia, oszacowania[[paste0(skrotCzesci,"_se")]]/rEAP)
  }
  
  if(!is.null(maskaTestowa)){
    doWstawienia = doWstawienia[maskaTestowa, ]
  }
  
  tryCatch({
    P = odbcConnect(as.character(zrodloDanychODBC))
    odbcSetAutoCommit(P, FALSE)
    #     sqlExecute(P, insertGrupa, data = list(idSkali, nrSkalowania, grupa), fetch = TRUE, stringsAsFactors = FALSE)[[1]]
    czesciEgzaminu = sqlExecute(P, zapytanie, data = doWstawienia, fetch = TRUE, stringsAsFactors = FALSE)[[1]]
    odbcEndTran(P, TRUE)
    odbcClose(P)
  },
  error=function(e) {
    odbcEndTran(P, FALSE)
    odbcClose(P)
    stop(e)
  }
  )
  return(invisible(NULL))
}
