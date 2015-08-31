#' @title Zapisz obiekt skalowania
#' @description
#' Zapis skalowania do bazy danych
#' @param skalowanie wynik działania fukcji skaluj z dodanym parametrem id_skali
#' @param opisSkalowania lista zawierająca dane skalowania
#' @param nadpisz czy elementy skali powinny być nadpisane
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @return funkcja nic nie zwraca
#' @export
zapisz_skalowanie = function(skalowanie, opisSkalowania, nadpisz = FALSE,
                             zrodloDanychODBC = "EWD_grzes"){
  if(opisSkalowania$egzamin == "sprawdzian"){
    message("Rozpoczęto zapis skalowania: ", opisSkalowania$egzamin, " ", opisSkalowania$rok, " - ", opisSkalowania$opis)
    opisSkalowania$skrot = "s"
    zapisz_pojedyncze_skalowanie(skalowanie, opisSkalowania, nadpisz, zrodloDanychODBC)
    message("Zapisano skalowanie: ", opisSkalowania$egzamin, " ", opisSkalowania$rok, " - ", opisSkalowania$opis)
  } else if(opisSkalowania$egzamin == "egzamin gimnazjalny"){
    for(skrot in names(skalowanie)){
      message("Rozpoczęto zapis skalowania: ", opisSkalowania$egzamin, " ", skrot,
              " ", opisSkalowania$rok, " - ", opisSkalowania$opis)
      opisSkalowania$skrot = skrot
      zapisz_pojedyncze_skalowanie(skalowanie[[skrot]], opisSkalowania, nadpisz, zrodloDanychODBC)
      message("Zapisano skalowanie: ", opisSkalowania$egzamin, " ", skrot,
              " ", opisSkalowania$rok, " - ", opisSkalowania$opis)
    }
  } else if(opisSkalowania$egzamin == "matura"){
    for(skrot in names(skalowanie)){
      for(rodzajSzkoly in names(skalowanie[[skrot]])  ){
        message("Rozpoczęto zapis skalowania: ", opisSkalowania$egzamin, " ", skrot, " ", rodzajSzkoly,
                " ", opisSkalowania$rok, " - ", opisSkalowania$opis)
        opisSkalowania$skrot = gsub("_p$", "_jp", gsub("^m", "m_", skrot))
        zapisz_pojedyncze_skalowanie(skalowanie[[skrot]][[rodzajSzkoly]], opisSkalowania, nadpisz, zrodloDanychODBC)
        message("Zapisano skalowanie: ", opisSkalowania$egzamin, " ", skrot, " ", rodzajSzkoly,
                " ", opisSkalowania$rok, " - ", opisSkalowania$opis)
      }
    }
  }
}
#' @title Zapis pojedynczego skalowania do bazy danych
#' @description
#' Funkcja zapisuje pojedyncze skalowanie do bazy danych
#' @param skalowanie wynik działania fukcji skaluj z dodanym parametrem id_skali
#' @param opisSkalowania lista zawierająca dane skalowania
#' @param nadpisz czy elementy skali powinny być nadpisane
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @return funkcja nic nie zwraca
#' @export
zapisz_pojedyncze_skalowanie = function(skalowanie, opisSkalowania, nadpisz = FALSE,
                                        zrodloDanychODBC = "EWD_grzes"){
  
  stopifnot(is.numeric(opisSkalowania$skalowanie),
            is.numeric(skalowanie$id_skali))
  
  if(is.null(skalowanie$id_skali)){
    warning("Brak id skali - pominięto skalowanie", immediate. = TRUE)
    return(invisible(NULL))
  }
  
  tryCatch({
    P = odbcConnect(as.character(zrodloDanychODBC))
    
    skalBaza  = sqlExecute(P, "SELECT count(*) FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
                           list(skalowanie$id_skali, opisSkalowania$skalowanie), TRUE)[1]
    elemBaza  = sqlExecute(P, "SELECT count(*) FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
                           list(skalowanie$id_skali, opisSkalowania$skalowanie), TRUE)[1]
    obserBaza = sqlExecute(P, "SELECT count(*) FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
                           list(skalowanie$id_skali, opisSkalowania$skalowanie), TRUE)[1]
    grupyBaza = sqlExecute(P, "SELECT count(*)  FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
                           list(skalowanie$id_skali, opisSkalowania$skalowanie), TRUE)[1]
    
    idSkal = sqlExecute(P, "SELECT DISTINCT id_skali FROM skale_elementy", fetch = TRUE, stringsAsFactors = FALSE)[[1]]
    
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )
  
  P = odbcConnect(as.character(zrodloDanychODBC))
  # Tutaj przydałby się początek transakcji.
  
  # Jeżeli skalowanie istnieje to zostanie ono usunięte wraz z parametrami i oszacowaniami.
  tryCatch({
    if(skalBaza==1 & nadpisz){
      
      if(elemBaza!=0){
        sqlExecute(P, "DELETE FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
                   list(skalowanie$id_skali, opisSkalowania$skalowanie))
        message("Usunięto rekordy z tablicy skalowania_elementy.")
      }
      if(obserBaza!=0){
        sqlExecute(P, "DELETE FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
                   list(skalowanie$id_skali, opisSkalowania$skalowanie))
        message("Usunięto rekordy z tablicy skalowania_obserwacje.")
      }
      if(grupyBaza !=0){
        sqlExecute(P, "DELETE FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
                   list(skalowanie$id_skali, opisSkalowania$skalowanie))
        message("Usunięto rekordy z tablicy skalowania_grupy.")
      }
      sqlExecute(P, "DELETE FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
                 list(skalowanie$id_skali, opisSkalowania$skalowanie))
      message("Usunięto rekord z tablicy skalowania.")
    } else if(skalBaza==1){
      stop("Skalowanie już istnieje. Jeżeli chcesz je nadpisać ustaw parametr nadpisz na TRUE.")
    }
    
    czyEdytowacSkale = all(
      0 == sqlExecute(P, "SELECT count(*) FROM skalowania_elementy WHERE id_skali = ?",
                      skalowanie$id_skali, fetch = TRUE)[1, 1],
      0 == sqlExecute(P, "SELECT count(*) FROM skalowania_obserwacje WHERE id_skali = ?",
                      skalowanie$id_skali, fetch = TRUE)[1, 1]
    )
    
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )
  
  # edycja elementów skali
  if(skalowanie$id_skali %in% idSkal & czyEdytowacSkale){
    elementy_skali = przygotuj_kryteria_do_edycji_skali(skalowanie)
    edytuj_skale(skalowanie$id_skali, elementy_skali, nadpisz = TRUE, zrodloDanychODBC)
    message("Edytowano skalę.")
  } else if(czyEdytowacSkale){
    elementy_skali = przygotuj_kryteria_do_edycji_skali(skalowanie)
    edytuj_skale(skalowanie$id_skali, elementy_skali, nadpisz = FALSE, zrodloDanychODBC)
    message("Edytowano skalę.")
  } else{
    message("Nie edytowano skali, ponieważ ta posiada przypisane obserwacje i/lub parametry z innych skalowań.")
  }
  
  grupy = unique(c(skalowanie$oszacowania$grupa, skalowanie$parametry$grupa))
  P = odbcConnect(as.character(zrodloDanychODBC))
  tryCatch({
    # wstawienie skalowania
    if(is.null(opisSkalowania$data))
    {
      insert = "INSERT INTO skalowania (skalowanie, opis , estymacja, id_skali, do_prezentacji, data)
      VALUES (? , ? , ?, ?, ?, CURRENT_DATE)"
      sqlExecute(P, insert , data=data.frame(opisSkalowania$skalowanie, opisSkalowania$opis,
                                             opisSkalowania$estymacja, skalowanie$id_skali,
                                             opisSkalowania$do_prezentacji))
    } else{
      insert = "INSERT INTO skalowania (skalowanie, opis , estymacja, id_skali, do_prezentacji, data)
      VALUES (? , ? , ?, ?, ?, ?)"
      sqlExecute(P, insert , data=data.frame(opisSkalowania$skalowanie, opisSkalowania$opis,
                                             opisSkalowania$estymacja, skalowanie$id_skali,
                                             opisSkalowania$do_prezentacji, opisSkalowania$data))
    }
    
    # wstawienie grup do skalowania
    insertGrupa = "INSERT INTO skalowania_grupy (id_skali, skalowanie, grupa)  VALUES (? , ? , ?)"
    for(gr in grupy){
      sqlExecute(P, insertGrupa, data = list(skalowanie$id_skali, opisSkalowania$skalowanie, gr), fetch = TRUE)
    }
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )
  
  rEAP = skalowanie$rzetelnoscEmpiryczna
  for(gr in grupy){
    zapisz_parametry_skalowania(skalowanie$id_skali, nazwa_skali=NULL, id_testu=NULL,
                                skalowanie$parametry, rEAP,
                                opisSkalowania$skalowanie, gr, zrodloDanychODBC)
  }
  
  skrot = opisSkalowania$skrot
  if(opisSkalowania$egzamin=="matura"){
    skrot = gsub("^m_", "m", gsub("_jp$", "_p", skrot))
  }
  
  zapisz_oszacowania_umiejetnosci(skalowanie$oszacowania, skrot, rEAP,
                                  opisSkalowania$egzamin,
                                  opisSkalowania$rok, opisSkalowania$skalowanie, skalowanie$id_skali,
                                  opisSkalowania$rodzajEstymacji, zrodloDanychODBC)
  
  # Tutaj przydałby się koniec transakcji.
}
#' @title Edycja skali
#' @description
#' Zapis skalowania do bazy danych
#' @param skalowanie wynik działania fukcji skaluj z dodanym parametrem id_skali
#' @return data frame w formacie parametru funkcji \code{\link{edytuj_skale}}
#' @import dplyr
przygotuj_kryteria_do_edycji_skali <- function(skalowanie){
  kryteria = unique(c(skalowanie$parametry$zmienna1,skalowanie$parametry$zmienna2))
  kryteria = kryteria[grepl("^k_|p_[[:digit:]]+$", kryteria)]
  kryteria = sapply(strsplit((kryteria), "_"), c)
  kryteria = data.frame(rodzaj = kryteria[1, ], numer = as.numeric(kryteria[2, ]))
  
  kryteriaDoBazy = kryteria$numer[kryteria$rodzaj == "k"]
  pseudoDoBazy = kryteria$numer[ kryteria$rodzaj == "p"]
  
  ret = data.frame(opis = character(0), id_skrotu = numeric(0),
                   id_pseudokryterium = numeric(0), id_kryterium = numeric(0))
  ret = rbind_list(ret, data.frame(id_kryterium = kryteriaDoBazy))
  ret = rbind_list(ret, data.frame(id_pseudokryterium = pseudoDoBazy)) %>% as.data.frame()
  return(ret)
}
