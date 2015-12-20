#' @title Zapis wskaznikow EWD do bazy
#' @description
#' Funkcja pozwala utworzyć w bazie (w tablicy \code{sl_wskazniki}) nowe
#' wskaźniki, na podobieństwo już istniejących. Z podanych wskaźników skopiowane
#' zostaną wszystkie ich właściwości (w tym wpisy w tablicach
#' \code{sl_wskazniki_typy_szkol} i \code{sl_kategorie_lu}), a zmienione
#' zostaną identyfikatory wskaźników (tj. wartość kolumny \code{wskaznik}).
#' Dodatkowe argumenty pozwalają też zmodyfikować wartości kolumn \code{nazwa},
#' \code{opis}, \code{okres} i \code{do_prezentacji}.
#' @param wskaznikiWzorce wektor ciągów znaków - identyfikatory wskaźników,
#' które już istnieją w bazie i mają posłużyć za wzorce dla nowotworzonych;
#' @param wyrazeniaZmienWskaznik dwuelementowy wektor ciągów znaków: pierwszy
#' element zostanie podany jako argument \code{pattern}, a drugi jako argument
#' \code{replace} do funkcji \code{\link{sub}} wywołanej na wartościach
#' argumentu \code{wskaznikiWzorce}, w celu utworzenia identyfikatorów
#' nowych wskaźników
#' @param wyrazeniaZmienNazwy opcjonalnie dwuelementowy wektor ciągów znaków:
#' pierwszy element zostanie podany jako argument \code{pattern}, a drugi jako
#' argument \code{replace} do funkcji \code{\link{sub}} wywołanej na nazwach
#' "wzorcowych" wskaźników, w celu uzyskania nazw nowych wskaźników
#' @param wyrazeniaZmienOpisy opcjonalnie dwuelementowy wektor ciągów znaków:
#' pierwszy element zostanie podany jako argument \code{pattern}, a drugi jako
#' argument \code{replace} do funkcji \code{\link{sub}} wywołanej na opisach
#' "wzorcowych" wskaźników, w celu uzyskania opisów nowych wskaźników
#' @param okres opcjonalnie liczba całkowita, zostanie przypisana nowym
#' wskaźnikom w kolumnie \code{okres} tablicy \code{sl_wskazniki}
#' @param doPrezentacji wartość logiczna, zostanie przypisana nowym wskaźnikom
#' w kolumnie \code{do_prezentacji} tablicy \code{sl_wskazniki}
#' @param zrodloDanychODBC opcjonalnie ciąg znaków - nazwa źródła danych ODBC,
#' dającego dostęp do bazy (domyślnie "ewd")
#' @return funkcja nic nie zwraca
#' @import RODBCext
#' @export
stworz_nowe_wskazniki_ewd = function(wskaznikiWzorce, wyrazeniaZmienWskaznik,
                                     wyrazeniaZmienNazwy = NULL,
                                     wyrazeniaZmienOpisy = NULL, okres = NULL,
                                     doPrezentacji = FALSE,
                                     zrodloDanychODBC = "ewd") {
  stopifnot(is.character(wskaznikiWzorce), length(wskaznikiWzorce) > 0,
            is.character(wyrazeniaZmienWskaznik),
            length(wyrazeniaZmienWskaznik) == 2,
            is.character(wyrazeniaZmienNazwy) | is.null(wyrazeniaZmienNazwy),
            is.character(wyrazeniaZmienOpisy) | is.null(wyrazeniaZmienOpisy),
            is.numeric(okres) | is.null(okres),
            is.logical(doPrezentacji) | is.null(doPrezentacji))
  if (!is.null(wyrazeniaZmienNazwy)) {
    stopifnot(length(wyrazeniaZmienNazwy) == 2)
  }
  if (!is.null(wyrazeniaZmienOpisy)) {
    stopifnot(length(wyrazeniaZmienOpisy) == 2)
  }
  if (!is.null(okres)) {
    stopifnot(length(okres) == 1, okres %in% 1:5)
  }
  if (!is.null(doPrezentacji)) {
    stopifnot(length(doPrezentacji) == 1, doPrezentacji %in% c(TRUE, FALSE))
  }

  P = odbcConnect(zrodloDanychODBC)
  on.exit(odbcClose(P))
  odbcSetAutoCommit(P, FALSE)

  # pobieranie
  wskaznikiWzor = list(
    sl_wskazniki =
      sqlExecute(P, "SELECT * FROM sl_wskazniki WHERE rodzaj_wsk = 'ewd' AND wskaznik = ?",
                              list(wskaznikiWzorce), fetch = TRUE, errors = TRUE),
    sl_wskazniki_typy_szkol =
      sqlExecute(P, "SELECT * FROM sl_wskazniki_typy_szkol WHERE rodzaj_wsk = 'ewd' AND wskaznik = ?",
                 list(wskaznikiWzorce), fetch = TRUE, errors = TRUE),
    sl_kategorie_lu =
      sqlExecute(P, "SELECT * FROM sl_kategorie_lu WHERE rodzaj_wsk = 'ewd' AND wskaznik = ?",
                 list(wskaznikiWzorce), fetch = TRUE, errors = TRUE)
  )
  if (nrow(wskaznikiWzor$sl_wskazniki) < length(wskaznikiWzorce)) {
    stop("W bazie nie istnieją wskaźniki: '",
         paste0(setdiff(wskaznikiWzorce, wskaznikiWzor$wskaznik), collapse = "', '"),
         "'.")
  }
  # modyfikacje
  if (!is.null(wyrazeniaZmienNazwy)) {
    for (i in wyrazeniaZmienNazwy) {
      wskaznikiWzor$sl_wskazniki$nazwa = sub(wyrazeniaZmienNazwy[1],
                                             wyrazeniaZmienNazwy[2],
                                             wskaznikiWzor$sl_wskazniki$nazwa)
    }
  }
  if (!is.null(wyrazeniaZmienOpisy)) {
    for (i in wyrazeniaZmienOpisy) {
      wskaznikiWzor$sl_wskazniki$opis = sub(wyrazeniaZmienOpisy[1],
                                            wyrazeniaZmienOpisy[2],
                                            wskaznikiWzor$sl_wskazniki$opis)
    }
  }
  if (!is.null(okres)) {
    wskaznikiWzor$sl_wskazniki$okres = okres
  }
  if (!is.null(doPrezentacji)) {
    wskaznikiWzor$sl_wskazniki$do_prezentacji = doPrezentacji
  }
  wskaznikiWzor = lapply(wskaznikiWzor,
                         function(x) {
                           x$wskaznik = sub(wyrazeniaZmienWskaznik[1],
                                            wyrazeniaZmienWskaznik[2],
                                            x$wskaznik)
                           return(x)
                         })
  # zapis
  wskaznikiNowe = list(
    sl_wskazniki =
      sqlExecute(P, uloz_insert_z_ramki("sl_wskazniki",
                                        wskaznikiWzor$sl_wskazniki),
                 wskaznikiWzor$sl_wskazniki, errors = TRUE),
    sl_wskazniki_typy_szkol =
      sqlExecute(P, uloz_insert_z_ramki("sl_wskazniki_typy_szkol",
                                        wskaznikiWzor$sl_wskazniki_typy_szkol),
                 wskaznikiWzor$sl_wskazniki_typy_szkol, errors = TRUE),
    sl_kategorie_lu =
      sqlExecute(P, uloz_insert_z_ramki("sl_kategorie_lu",
                                        wskaznikiWzor$sl_kategorie_lu),
                 wskaznikiWzor$sl_kategorie_lu, errors = TRUE)
  )
  # koniec
  odbcEndTran(P, TRUE)
  message(" Zapis ", nrow(wskaznikiWzor$sl_wskazniki),
          " wskaźników, zakończony powodzeniem.")
  invisible(NULL)
}
