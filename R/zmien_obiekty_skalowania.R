#' @title Dodanie do obiektu skalowania grupy
#' @description
#' Funkcja dodaje kolumnę grupa oraz id skali do odpowiednich elementów listy - wyniku skalowania.
#' @param skalowanie lista opisująca wynik skalowania
#' @param rokEgzaminu rok egzaminu
#' @param rodzajEgzaminu rodzaj egzaminu ("sprawdzian", "egzamin gimnazjalny", "matura")
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @param maskaTestowa wektor całkowitoliczbowy określający, które obserwacje mają być umieszczone w docelowym obiekcie.
#' Jeżeli NULL to wszystkie obserwacje są umieszczane
#' @return data frame zawierający parametry zadań z dodaną kolumną opisującą grupę
#' @export
zmien_obiekty_skalowania <- function(skalowanie, rokEgzaminu, rodzajEgzaminu,
                                     zrodloDanychODBC = "EWD_grzes", maskaTestowa = NULL)
{
  stopifnot(is.character(rodzajEgzaminu) &  rodzajEgzaminu %in% c("matura", "egzamin gimnazjalny", "sprawdzian"))
  
  if(rodzajEgzaminu == "sprawdzian"){
    
    if(all( c("oszacowania", "parametry") %in% names(skalowanie) ))
    {
      if(is.null(skalowanie[["id_skali"]]))
        skalowanie[["id_skali"]] = daj_id_skali(rokEgzaminu, rodzajEgzaminu, NULL, zrodloDanychODBC);
      skalowanie$oszacowania = przypisz_grupy_do_oszacowan(skalowanie, NULL, maskaTestowa)
      
      grupy = unique(skalowanie$oszacowania$grupa)
      skalowanie$parametry = przypisz_grupy_do_parametrow(skalowanie, grupy)
    } else stop("Błędny obiekt skalowania sprawdzianu.")
    
  } else if(rodzajEgzaminu == "egzamin gimnazjalny"){
    
    for(skrot in names(skalowanie)){
      if(all( c("oszacowania", "parametry") %in% names(skalowanie[[skrot]]) )){
        if(is.null(skalowanie[[skrot]][["id_skali"]]))
          skalowanie[[skrot]][["id_skali"]] = daj_id_skali(rokEgzaminu, rodzajEgzaminu, skrot, zrodloDanychODBC)
        skalowanie[[skrot]]$oszacowania = przypisz_grupy_do_oszacowan(skalowanie[[skrot]], NULL, maskaTestowa)
        grupy = unique(skalowanie[[skrot]]$oszacowania$grupa)
        skalowanie[[skrot]]$parametry = przypisz_grupy_do_parametrow(skalowanie[[skrot]], grupy)
      } else stop("Błędny obiekt skalowania egzaminu gimnazjalnego")
    }
    
  } else if(rodzajEgzaminu == "matura"){
    
    for(skrot in names(skalowanie)){
      for(rodzajSzkoly in names(skalowanie[[skrot]])  ){
        if(all( c("oszacowania", "parametry") %in% names(skalowanie[[skrot]][[rodzajSzkoly]]) )){
          if(is.null(skalowanie[[skrot]][[rodzajSzkoly]][["id_skali"]]))
            skalowanie[[skrot]][[rodzajSzkoly]][["id_skali"]] =
            daj_id_skali(rokEgzaminu, rodzajEgzaminu, skrot, zrodloDanychODBC)
          skalowanie[[skrot]][[rodzajSzkoly]]$oszacowania =
            przypisz_grupy_do_oszacowan(skalowanie[[skrot]][[rodzajSzkoly]], rodzajSzkoly, maskaTestowa)
          grupy = unique(skalowanie[[skrot]][[rodzajSzkoly]]$oszacowania$grupa)
          skalowanie[[skrot]][[rodzajSzkoly]]$parametry =
            przypisz_grupy_do_parametrow(skalowanie[[skrot]][[rodzajSzkoly]], grupy)
          #           gsub("^m", "m_", skrot)
        } else stop("Błędny obiekt skalowania matury.")
      }
    }
  }
  return(skalowanie)
}
#' @title Dodanie grupy do parametrów zadań
#' @description
#' Funkcja dodaje kolumnę grupa do elementu listy skalowania opisującego parametry zadań
#' @param skalowanieWynik lista opisująca wynik pojedynczego skalowania
#' @param grupy wektor stringów - nazwy grup, które należy dodać
#' @return data frame zawierający parametry zadań z dodaną kolumną opisującą grupę
przypisz_grupy_do_parametrow <- function(skalowanieWynik, grupy)
{
  if("grupa" %in% names(skalowanieWynik$parametry))
    return(skalowanieWynik$parametry)
  ret = NULL
  for(grupa in grupy){
    ret = rbind(ret, cbind(skalowanieWynik$parametry, grupa = grupa, stringsAsFactors = FALSE))
  }
  return(ret)
}
#' @title Dodanie grupy do oszacowań umiejętności
#' @description
#' Funkcja dodaje kolumnę grupa do elementu listy skalowania opisującego umiejętności uczniów
#' @param skalowanieWynik lista opisująca wynik pojedynczego skalowania
#' @param rodzaj nazwa równa "LO" lub "T" dla matury albo domyśnie NULL dla sprawdzianu i egzaminu gimnazjalnego
#' @param maskaTestowa wektor całkowitoliczbowy określający, które obserwacje mają być umieszczone w docelowym obiekcie.
#' Jeżeli NULL to wszystkie obserwacje są umieszczane
#' @return data frame zawierający umiejętności z dodaną kolumną grupy
przypisz_grupy_do_oszacowan <- function(skalowanieWynik, rodzaj = NULL, maskaTestowa = NULL)
{
  stopifnot((is.character(rodzaj) & rodzaj %in% c("LO", "T")) | is.null(rodzaj))
  
  if(is.null(maskaTestowa)){
    maskaTestowa = seq_len(nrow(skalowanieWynik$oszacowania))
  }
  skalowanieWynik$oszacowania = skalowanieWynik$oszacowania[maskaTestowa, ]
  
  if("grupa" %in% names(skalowanieWynik$oszacowania))
    return(skalowanieWynik$oszacowania)
  
  if(is.null(rodzaj)){
    ret = cbind(skalowanieWynik$oszacowania, grupa = "", stringsAsFactors = FALSE)
    return(ret)
  }
  
  testyPodst = grepl("^id_testu[_|[:alnum:]]*_p$", names(skalowanieWynik$oszacowania))
  testyRozsz = grepl("^id_testu[_|[:alnum:]]*_r$", names(skalowanieWynik$oszacowania))
  
  pisalPodst = apply(skalowanieWynik$oszacowania[, testyPodst, drop=FALSE], 1, function(x) any(is.na(x) %in% FALSE))
  pisalRozsz = apply(skalowanieWynik$oszacowania[, testyRozsz, drop=FALSE], 1, function(x) any(is.na(x) %in% FALSE))
  
  if( sum( pisalPodst | pisalRozsz ) != nrow(skalowanieWynik$oszacowania)  ) {
    stop("Błędne rozpoznanie kolumn opisujących id testów.")
  }
  
  grupa = character(nrow(skalowanieWynik$oszacowania))
  if(rodzaj == "LO"){
    grupa[pisalPodst & !pisalRozsz ] = "LO tylko PP"
    grupa[pisalRozsz ] = "LO PP i PR"
  } else if(rodzaj == "T"){
    grupa[pisalPodst & !pisalRozsz ] = "T tylko PP"
    grupa[pisalRozsz ] = "T PP i PR"
  }
  
  ret = cbind(skalowanieWynik$oszacowania, grupa = grupa, stringsAsFactors = FALSE)
  return(ret)
}
#' @title Pobiernie id skali
#' @description
#' Funkcja na podstawie ogulnych informacji o egzaminie pobiera jego id skali.
#' @param rokEgzaminu rok egzaminu powiązany ze skalą
#' @param rodzajEgzaminu rodzaj egzaminu"
#' @param skrot skrótowa nazwa egzaminu lub jego części (np: "s", "gh" lub "m_mp")
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @return typ numeryczny
daj_id_skali <- function(rokEgzaminu, rodzajEgzaminu, skrot = NULL, zrodloDanychODBC = "EWD_grzes")
{
  stopifnot(is.character(skrot) | is.null(skrot))
  stopifnot(is.character(rodzajEgzaminu), rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura"))
  
  if(rodzajEgzaminu == "sprawdzian"){
    nazwa_skali = paste("ewd", "s", rokEgzaminu, sep = ";")
    skrot = "s"
  } else if(rodzajEgzaminu == "egzamin gimnazjalny"){
    stopifnot( skrot %in% c("gh_h", "gh_p", "gm_m", "gm_p", "gh","gm" ))
    nazwa_skali = paste("ewd", skrot, rokEgzaminu, sep = ";")
    
  } else if(rodzajEgzaminu == "matura"){
    #     if(skrot == "mp"){
    #       czesc_nazwy = "m_jp"
    #     } else if(skrot == "mm"){
    #       czesc_nazwy = "m_m"
    #     } else if(skrot == "mh"){
    #       czesc_nazwy = "m_h"
    #     } else if(skrot == "mmp"){
    #       czesc_nazwy = "m_mp"
    #     } else{
    #       stop("Nieznany skrót: ", skrot)
    #     }
    
    czesc_nazwy = gsub("_p$", "_jp", gsub("^m", "m_", skrot))
    nazwa_skali = paste("ewd", czesc_nazwy, rokEgzaminu, sep = ";")
  }
  
  zapytanie = "select id_skali from skale where opis = ?"
  
  tryCatch({
    P = odbcConnect(as.character(zrodloDanychODBC))
    
    idSkali = sqlExecute(P, zapytanie, data = data.frame(nazwa_skali), fetch = TRUE, stringsAsFactors = FALSE)[[1]]
    
    cat("\n", nazwa_skali, idSkali)
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )
  
  return(idSkali)
}
