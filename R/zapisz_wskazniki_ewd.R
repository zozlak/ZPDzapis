#' @title Zapis wskaznikow EWD do bazy
#' @description
#' Funkcja zapisuje do bazy wyniki skalowania: wartości parametrów, oszacowania
#' umiejętności uczniów, ew. normy (skale raschowe). W miarę potrzeby tworzy
#' nowe skalowanie w bazie, lub dopisuje do/nadpisuje już istniejące(go).
#' @param nazwaPliku ciąg znaków - nazwa pliku .RData zawierającego obiekt
#' klasy \code{listaWskaznikowEWD}, będący wynikiem działania funkcji
#' \code{\link[EWDwskazniki]{przygotuj_wsk_ewd}})
#' @param sufiks ciąg znaków - sufiks określający typ wskaźnika, dopisywany
#' do części wynikającej z nazwy konstruktu
#' @param doPrezentacji wartość logiczna - czy oznaczyć zapisywany wskaźnik
#' (w danym roku) jako 'do prezentacji'?
#' @param nadpisz wartość logiczna - czy nadpisać wartości zapisane w bazie?
#' (jeśli istnieją)
#' @param zrodloDanychODBC opcjonalnie ciąg znaków - nazwa źródła danych ODBC,
#' dającego dostęp do bazy (domyślnie "ewd")
#' @return funkcja nic nie zwraca
#' @details
#' Wskaźniki, których wartości mają być wczytywane do bazy muszą już być
#' wcześniej zarejestrowane w tablicach \code{sl_wskazniki},
#' \code{sl_wskazniki_typy_szkol} i \code{sl_kategorie_lu}. Pomocna może być
#' w tym funkcja \code{\link{stworz_nowe_wskazniki_ewd}}
#'
#' Parametr \code{doPrezentacji} wpływa na ustawienia na poziomie wskaźnika-roku,
#' ale o tym, czy wskaźniki będą publicznie dostępne decyduje koniunkcja
#' z wartością kolumny \code{do_prezentacji} w tablicy \code{sl_wskazniki},
#' na którą wywołanie tej funkcji nie oddziałuje (trzeba ją modyfikować w bazie
#' "ręcznie").
#' @import RODBCext
#' @export
zapisz_wskazniki_ewd = function(nazwaPliku, sufiks, doPrezentacji = FALSE,
                                nadpisz = FALSE, zrodloDanychODBC = "ewd") {

  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1,
            is.character(sufiks), length(sufiks) == 1,
            is.logical(nadpisz), length(nadpisz) == 1,
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1)
  stopifnot(file.exists(nazwaPliku), nadpisz %in% c(TRUE, FALSE),
            doPrezentacji %in% c(TRUE, FALSE))
  sufiks = paste0("_", sub("^_", "", sufiks))

  P = odbcConnect(zrodloDanychODBC)
  on.exit(odbcClose(P))
  odbcSetAutoCommit(P, FALSE)

  obiekty = load(nazwaPliku)
  message("Wczytano plik '", nazwaPliku, "'.")
  for (i in obiekty) {
    x = get(i)
    rm(list = i)
    if (!("listaWskaznikowEWD" %in% class(x))) {
      next
    }
    wskazniki = attributes(x)$wskazniki
    wskazniki$do_prezentacji = doPrezentacji
    wskazniki_skalowania = attributes(x)$wskazniki_skalowania
    wskazniki_parametry = attributes(x)$wskazniki_parametry
    liczba_zdajacych = attributes(x)$liczba_zdajacych
    names(liczba_zdajacych) = sub("^id_szkoly(.*)$", "id_szkoly",
                                  names(liczba_zdajacych))
    # przygotowywanie wsadu do 'wartosc_wskaznikow'
    message(" Przygotowywanie danych do wczytania.",
            format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    wartosci_wskaznikow = data.frame()
    for (i in 1:length(x)) {
      x[[i]] = cbind(id_ww = NA, rodzaj_wsk = "ewd",
                     wskaznik = names(x)[i],
                     x[[i]][, !grepl("^lu", names(x[[i]]))],
                     polska = FALSE,
                     stringsAsFactors = FALSE)
      names(x[[i]]) = sub("^id_szkoly(.*)$", "id_szkoly", names(x[[i]]))
      names(x[[i]]) = sub("^(|bs_)ewd(.*)$", "\\1ewd", names(x[[i]]))
      names(x[[i]]) = sub("^kor(.*)$", "korelacja", names(x[[i]]))
      names(x[[i]]) = sub("^(|bs_)sr_we(.*)$", "\\1srednia_we", names(x[[i]]))
      names(x[[i]]) = sub(paste0("^", names(x)[i], "$"), "srednia", names(x[[i]]))
      names(x[[i]]) = sub(paste0("^bs_", names(x)[i], "$"), "bs", names(x[[i]]))
      wartosci_wskaznikow = rbind(wartosci_wskaznikow, x[[i]])
    }
    # dopisywanie sufiksu do nazw wskazników
    wskazniki$wskaznik = paste0(wskazniki$wskaznik, sufiks)
    wskazniki_skalowania$wskaznik = paste0(wskazniki_skalowania$wskaznik, sufiks)
    wskazniki_parametry$wskaznik = paste0(wskazniki_parametry$wskaznik, sufiks)
    wartosci_wskaznikow$wskaznik = paste0(wartosci_wskaznikow$wskaznik, sufiks)
    liczba_zdajacych$wskaznik = paste0(liczba_zdajacych$wskaznik, sufiks)
    rozneWskazniki = unique(wartosci_wskaznikow[, c("rodzaj_wsk", "wskaznik", "rok_do")])

    # kasowanie
    if (nadpisz) {
      message(" Kasowanie danych w bazie dla wskazników:")
      print(rozneWskazniki, row.names = FALSE)
      # jeśli powyżej nie wybuchło, to kasujemy szerokim frontem
      kasowanie = list(
        liczba_zdajacych =
          sqlExecute(P, "DELETE FROM liczba_zdajacych WHERE id_ww IN (SELECT id_ww FROM wartosci_wskaznikow WHERE rodzaj_wsk = ? AND wskaznik = ? AND rok_do = ?)",
                     rozneWskazniki, errors = TRUE),
        wartosci_wskaznikow =
          sqlExecute(P, "DELETE FROM wartosci_wskaznikow WHERE rodzaj_wsk = ? AND wskaznik = ? AND rok_do = ?",
                     rozneWskazniki, errors = TRUE),
        wskazniki_parametry =
          sqlExecute(P, "DELETE FROM wskazniki_parametry WHERE rodzaj_wsk = ? AND wskaznik = ? AND rok_do = ?",
                     rozneWskazniki, errors = TRUE),
        wskazniki_skalowania =
          sqlExecute(P, "DELETE FROM wskazniki_skalowania WHERE rodzaj_wsk = ? AND wskaznik = ? AND rok_do = ?",
                     rozneWskazniki, errors = TRUE),
        wskazniki =
          sqlExecute(P, "DELETE FROM wskazniki WHERE rodzaj_wsk = ? AND wskaznik = ? AND rok_do = ?",
                     rozneWskazniki, errors = TRUE)
      )
    }
    # wczytujemy
    message(" Wczytywanie do tablicy 'wskazniki'.",
            format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    w = sqlExecute(P, uloz_insert_z_ramki("wskazniki", wskazniki),
                   wskazniki, errors = TRUE)
    message(" Wczytywanie do tablic 'wskazniki_skalowania'.",  #i 'wskazniki_parametry'.",
            format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    w = sqlExecute(P, uloz_insert_z_ramki("wskazniki_skalowania", wskazniki_skalowania),
                   wskazniki_skalowania, errors = TRUE)
    #w = sqlExecute(P, uloz_insert_z_ramki("wskazniki_parametry", wskazniki_parametry),
    #               wskazniki_parametry, errors = TRUE)
    message(" Wczytywanie do tablic 'wartosci_wskaznikow'.",
            format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    idWw = sqlExecute(P, "SELECT max(id_ww) FROM wartosci_wskaznikow",
                            fetch = TRUE, errors = TRUE)[1, 1]
    wartosci_wskaznikow$id_ww = 1:nrow(wartosci_wskaznikow) + idWw
    w = sqlExecute(P, uloz_insert_z_ramki("wartosci_wskaznikow", wartosci_wskaznikow),
                   wartosci_wskaznikow, errors = TRUE)
    message(" Wczytywanie do tablic 'liczba_zdajacych'.",
            format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    lPrzed = nrow(liczba_zdajacych)
    liczba_zdajacych =
      merge(wartosci_wskaznikow[, intersect(names(liczba_zdajacych),
                                            names(wartosci_wskaznikow))],
            liczba_zdajacych[, names(liczba_zdajacych) != "id_ww"])
    if (lPrzed != nrow(liczba_zdajacych)) {
      stop("Część danych w elemencie 'liczba_zdajacych' nie ma odpowiednikow ",
           "w wartościach wskaźników.")
    }
    liczba_zdajacych = liczba_zdajacych[, names(liczba_zdajacych) != "id_szkoly"]
    w = sqlExecute(P, uloz_insert_z_ramki("liczba_zdajacych", liczba_zdajacych),
                   liczba_zdajacych, errors = TRUE)
  }
  # koniec
  odbcEndTran(P, TRUE)
  message(" Zapis zakończony.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"), "\n")
  invisible(NULL)
}
