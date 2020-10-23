#' @title Zapis wskaznikow EWD do bazy
#' @description
#' Funkcja zapisuje do bazy oszacowania wskaźników EWD: wartości samych
#' wskaźników, liczbę zdających, parametry warstwic, wartości parametrów modeli
#' regresji oraz informacje o powiązaniu ze skalami wyników (oszacowań
#' umiejętności uczniów).
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param nazwaPliku ciąg znaków - nazwa pliku .RData zawierającego obiekt
#' klasy \code{listaWskaznikowEWD}, będący wynikiem działania funkcji
#' \code{\link[EWDwskazniki]{przygotuj_wsk_ewd}})
#' @param sufiks ciąg znaków - sufiks określający typ wskaźnika, dopisywany
#' do części wynikającej z nazwy konstruktu
#' @param doPrezentacji wartość logiczna - czy oznaczyć zapisywany wskaźnik
#' (w danym roku) jako 'do prezentacji'?
#' @param nadpisz wartość logiczna - czy nadpisać wartości zapisane w bazie?
#' (jeśli istnieją)
#' @param korekta wartość logiczna - ustawić na \code{TRUE}, jeśli wczytywane
#' są korekty wartości wskaźników dla poszczególnych szkół, obliczone przy
#' pomocy funkcji \code{\link[EWDwskazniki]{koryguj_wsk_ewd}} (wartość
#' parametrów \code{doPrezentacji} i \code{nadpisz} zostaną wtedy zignorowane)
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
#' @export
zapisz_wskazniki_ewd = function(
  P,
  nazwaPliku,
  sufiks,
  doPrezentacji = FALSE,
  nadpisz = FALSE,
  korekta = FALSE
) {

  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1,
            is.character(sufiks), length(sufiks) == 1,
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.logical(korekta), length(korekta) == 1,
  )
  stopifnot(file.exists(nazwaPliku), nadpisz %in% c(TRUE, FALSE),
            doPrezentacji %in% c(TRUE, FALSE), korekta %in% c(TRUE, FALSE)
  )
  sufiks = paste0("_", sub("^_", "", sufiks))
  if (korekta) {
    nadpisz = FALSE
  }

  DBI::dbBegin(P)

  obiekty = load(nazwaPliku)
  message("Wczytano plik '", nazwaPliku, "'.")
  for (i in obiekty) {
    x = get(i)
    rm(list = i)
    if (!("listaWskaznikowEWD" %in% class(x))) {
      next
    }
    if (!korekta) {
      wskazniki = attributes(x)$wskazniki
      wskazniki$do_prezentacji = doPrezentacji
      wskazniki_skalowania = attributes(x)$wskazniki_skalowania
      wskazniki_parametry = attributes(x)$wskazniki_parametry
      # dopisywanie sufiksu do nazw wskazników
      wskazniki$wskaznik = paste0(wskazniki$wskaznik, sufiks)
      wskazniki_skalowania$wskaznik = paste0(wskazniki_skalowania$wskaznik, sufiks)
      wskazniki_parametry$wskaznik = paste0(wskazniki_parametry$wskaznik, sufiks)
    }
    liczba_zdajacych = attributes(x)$liczba_zdajacych
    names(liczba_zdajacych) = sub("^id_szkoly(.*)$", "id_szkoly",
                                  names(liczba_zdajacych))
    liczba_zdajacych$wskaznik = paste0(liczba_zdajacych$wskaznik, sufiks)
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
    wartosci_wskaznikow =
      wartosci_wskaznikow[, names(wartosci_wskaznikow) != "matura_miedzynarodowa"]
    wartosci_wskaznikow$wskaznik = paste0(wartosci_wskaznikow$wskaznik, sufiks)

    # kasowanie
    if (nadpisz) {
      rozneWskazniki = unique(wartosci_wskaznikow[, c("rodzaj_wsk", "wskaznik", "rok_do")])
      message(" Kasowanie danych w bazie dla wskazników:")
      print(rozneWskazniki, row.names = FALSE)
      # jeśli powyżej nie wybuchło, to kasujemy szerokim frontem
      kasowanie = list(
        liczba_zdajacych = .sqlQuery(P,
          "DELETE FROM liczba_zdajacych WHERE id_ww IN (SELECT id_ww FROM wartosci_wskaznikow WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3)",
          rozneWskazniki
        ),
        wartosci_wskaznikow = .sqlQuery(P,
          "DELETE FROM wartosci_wskaznikow WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3",
          rozneWskazniki
        ),
        wskazniki_parametry = .sqlQuery(P,
          "DELETE FROM wskazniki_parametry WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3",
          rozneWskazniki
        ),
        wskazniki_skalowania = .sqlQuery(P,
          "DELETE FROM wskazniki_skalowania WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3",
          rozneWskazniki
        ),
        wskazniki = .sqlQuery(P,
          "DELETE FROM wskazniki WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3",
          rozneWskazniki
        )
      )
    }
    if (korekta) {
      rozneWskazniki = unique(wartosci_wskaznikow[, c("rodzaj_wsk", "wskaznik",
                                                      "rok_do", "id_szkoly")])
      message(" Kasowanie danych w bazie dla wskazników w szkołach:")
      print(rozneWskazniki, row.names = FALSE)
      kasowanie = list(
        liczba_zdajacych = .sqlQuery(P,
          "DELETE FROM liczba_zdajacych WHERE id_ww IN (SELECT id_ww FROM wartosci_wskaznikow WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3 AND id_szkoly = $4)",
          rozneWskazniki
        ),
        wartosci_wskaznikow = .sqlQuery(P,
          "DELETE FROM wartosci_wskaznikow WHERE rodzaj_wsk = $1 AND wskaznik = $2 AND rok_do = $3 AND id_szkoly = $4",
          rozneWskazniki
        )
      )
    }
    # wczytujemy
    if (!korekta) {
      message(" Wczytywanie do tablicy 'wskazniki'.",
              format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
      w = .sqlQuery(P, uloz_insert_z_ramki("wskazniki", wskazniki), wskazniki)
      message(" Wczytywanie do tablicy 'wskazniki_skalowania'.",  #i 'wskazniki_parametry'.",
              format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
      w = .sqlQuery(P, uloz_insert_z_ramki("wskazniki_skalowania", wskazniki_skalowania),
                     wskazniki_skalowania)
      #w = .sqlQuery(P, uloz_insert_z_ramki("wskazniki_parametry", wskazniki_parametry),
      #               wskazniki_parametry)
    }
    message(" Wczytywanie do tablicy 'wartosci_wskaznikow'.",
            format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    idWw = .sqlQuery(P, "SELECT max(id_ww) FROM wartosci_wskaznikow")[1, 1]
    wartosci_wskaznikow$id_ww = 1:nrow(wartosci_wskaznikow) + idWw
    w = .sqlQuery(P, uloz_insert_z_ramki("wartosci_wskaznikow", wartosci_wskaznikow),
                   wartosci_wskaznikow)
    message(" Wczytywanie do tablicy 'liczba_zdajacych'.",
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
    w = .sqlQuery(P, uloz_insert_z_ramki("liczba_zdajacych", liczba_zdajacych),
                   liczba_zdajacych)
  }
  # koniec
  DBI::dbCommit(P)
  message(" Zapis zakończony.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"), "\n")
  invisible(NULL)
}
