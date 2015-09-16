#' @title Zapis wynikow skalowania do bazy
#' @description
#' Funkcja zapisuje do bazy wyniki skalowania: wartości parametrów, oszacowania
#' umiejętności uczniów, ew. normy (skale raschowe). W miarę potrzeby tworzy
#' nowe skalowanie w bazie, lub dopisuje do/nadpisuje już istniejące(go).
#' @param nazwaPliku ciąg znaków - nazwa pliku .RData, zawierającego wyniki
#' skalowania (typowo zapisanego przez funkcję \code{\link[EWDskale]{skaluj_spr}},
#' \code{\link[EWDskale]{skaluj_egz_gimn}},
#' \code{\link[EWDskale]{skaluj_egz_gimn_rasch}},
#' \code{\link[EWDskale]{skaluj_matura}} lub
#' \code{\link[EWDskale]{skaluj_matura_rasch}})
#' @param doPrezentacji wartość logiczna - jeśli tworzone będzie nowe skalowanie,
#' to czy ma ono zostać oznaczone jako 'do przezentacji'?
#' @param nadpisz wartość logiczna - czy elementy skali powinny być nadpisane?
#' @param oszacowaniaDoCopy wartość logiczna - czy zamiast wczytywać oszacowania
#' umiejętności do tablicy \code{skalowania_obserwacje} przy pomocy poleceń
#' \code{INSERT} przez ODBC (jak wszystko inne), wygenerować plik csv, który
#' będzie można wczytać do bazy komendą \code{COPY} przez \code{psql}?
#' @param zrodloDanychODBC opcjonalnie ciąg znaków - nazwa źródła danych ODBC,
#' dającego dostęp do bazy (domyślnie "ewd")
#' @details
#' W kwestii sposobu działania parametru \code{nadpisz}, patrz sekcja
#' description pomocy do funkcji \code{\link{zapisz_pojedyncze_skalowanie}}.
#'
#' Oszacowania umiejętności domyślnie (\code{oszacowaniaDoCopy = TRUE}) nie są
#' wczytywane do bazy, gdyż trwałoby to koszmarnie długo (po 4-6 h na część
#' egzaminu). Zamiast tego zapisywany jest na dysku skompresowany (w formacie
#' zip) plik csv (w formacie zgodnym z wynikiem działania
#' \code{\link{write.csv}}), który należy przenieść na Odrę i stamtąd wczytać go
#' do bazy komendą \code{COPY} programu \code{psql}.
#'
#' Uwaga! Przy zapisie do pliku csv R potrafi perfidnie zastosować notację
#' naukową do \code{id_obserwacji}, na szczęścietylko wtedy, gdy nie powoduje to
#' utraty precyzji. Jeśli takie zdarzenie nastąpi, \code{psql} rzuci błędem
#' przy próbie wczytania danych. Póki co jedynym rozwiązaniem jest ręczne
#' poprawienie w pliku csv takiej wartości na legalną liczbę całkowitą
#' i odpalenie \code{psql} ponownie.
#'
#' Uwaga! Jeśli \code{oszacowaniaDoCopy = TRUE} i \code{nadpisz = TRUE}, to
#' w ramach wywołania funkcji usunięte zostaną dotychczasowe wartości tablicy
#' \code{skalowania_obserwacje} powiązane z danymi skalami-skalowaniami, ale
#' nowe nie zostaną wczytane automatycznie.
#' @return funkcja nic nie zwraca
#' @export
zapisz_skalowanie = function(nazwaPliku, doPrezentacji = FALSE, nadpisz = FALSE,
                             oszacowaniaDoCopy = TRUE, zrodloDanychODBC = "ewd"){
  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1,
            is.logical(nadpisz), length(nadpisz) == 1,
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.logical(oszacowaniaDoCopy), length(oszacowaniaDoCopy) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1)
  stopifnot(file.exists(nazwaPliku), nadpisz %in% c(TRUE, FALSE),
            doPrezentacji %in% c(TRUE, FALSE),
            oszacowaniaDoCopy %in% c(TRUE, FALSE))

  obiekty = load(nazwaPliku)
  message("Wczytano plik '", nazwaPliku, "'.")
  for (i in obiekty) {
    x = get(i)
    rm(list = i)
    if (!("listaWynikowSkalowania" %in% class(x))) {
      next
    }
    message(" Rozpoczęto zapis wyników skalowania konstruktu '", i, "'.")
    lapply(x, zapisz_pojedyncze_skalowanie, doPrezentacji = doPrezentacji,
           nadpisz = nadpisz, oszacowaniaDoCopy = oszacowaniaDoCopy,
           zrodloDanychODBC = zrodloDanychODBC)
  }
  # koniec
  invisible(NULL)
}
#' @title Zapis wynikow skalowania do bazy
#' @description
#' Funkcja zapisuje do bazy wyniki pojedynczego skalowania
#' @param x obiekt klasy \code{wynikiSkalowania}
#' @param doPrezentacji wartość logiczna - jeśli tworzone będzie nowe skalowanie,
#' to czy ma ono zostać oznaczone jako 'do przezentacji'?
#' @param nadpisz wartość logiczna - czy elementy skali powinny być nadpisane?
#' @param oszacowaniaDoCopy wartość logiczna - czy zamiast wczytywać oszacowania
#' umiejętności do tablicy \code{skalowania_obserwacje} przy pomocy poleceń
#' \code{INSERT} przez ODBC (jak wszystko inne), wygenerować plik csv, który
#' będzie można wczytać do bazy komendą \code{COPY} przez \code{psql}?
#' @param zrodloDanychODBC opcjonalnie ciąg znaków - nazwa źródła danych ODBC,
#' dającego dostęp do bazy (domyślnie "ewd")
#' @param proba opcjonalnie liczba natrualna - wielkość próby, jaka ma być
#' wylosowana z elementu \code{skalowanie_obserwacje} przed dokonaniem zapisu;
#' przydatne (tylko) do testów działania funkcji
#' @details
#' Jeśli argument \code{nadpisz} ma wartość \code{TRUE} i w bazie jest już
#' zapisane dane skalowanie, to funkcja wymaga, aby obiekt \code{x} miał
#' wszystkie elementy, które już są zapisane w bazie. Np. jeśli w bazie zapisano
#' powiązane z danym skalowaniem parametry, \code{x} musi zawierać element
#' \code{skalowania_elementy} itd. Wszystkie wpisy w bazie, powiązane z danym
#' skalowaniem zostaną w takiej sytuacji usunięte i nadpisane nowymi.
#'
#' Jeśli argument \code{nadpisz} ma wartość \code{FALSE} i w bazie jest już
#' zapisane dane skalowanie, funkcja najpierw sprawdzi, czy dane przekazywane do
#' zapisania, które odnoszą się do tabeli \code{skalowania_grupy} są dokładnie
#' takie same, jak dane już zapisane w bazie. Jeśli tak, spróbuje dopisać dane
#' odnoszące się do tablic \code{skalowania_elementy},
#' \code{skalowania_obserwcje}, i \code{normy} do danych już istniejących
#' w bazie. Jeśli napotka przy tym jakieś konflikty, zaniecha zapisu
#' jakichkolwiek danych.
#'
#' Oszacowania umiejętności domyślnie (\code{oszacowaniaDoCopy = TRUE}) nie są
#' wczytywane do bazy, gdyż trwałoby to koszmarnie długo (po 4-6 h na część
#' egzaminu). Zamiast tego zapisywany jest na dysku skompresowany (w formacie
#' zip) plik csv (w formacie zgodnym z wynikiem działania
#' \code{\link{write.csv}}), który należy przenieść na Odrę i stamtąd wczytać go
#' do bazy komendą \code{COPY} programu \code{psql}.
#'
#' Uwaga! Jeśli \code{oszacowaniaDoCopy = TRUE} i \code{nadpisz = TRUE}, to
#' w ramach wywołania funkcji usunięte zostaną dotychczasowe wartości tablicy
#' \code{skalowania_obserwacje} powiązane z danymi skalami-skalowaniami, ale
#' nowe nie zostaną wczytane automatycznie.
#' @return funkcja nic nie zwraca
#' @export
zapisz_pojedyncze_skalowanie = function(x, doPrezentacji = FALSE,
                                        nadpisz = FALSE, oszacowaniaDoCopy = TRUE,
                                        zrodloDanychODBC = "ewd", proba = -1){
  stopifnot(is.list(x), "wynikiSkalowania" %in% class(x),
            is.logical(nadpisz), length(nadpisz) == 1,
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.logical(oszacowaniaDoCopy), length(oszacowaniaDoCopy) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1,
            is.numeric(proba), length(proba) == 1)
  stopifnot("skalowania" %in% names(x),
            "skalowania_grupy" %in% names(x),
            any(c("skalowania_elementy", "skalowania_obserwacje") %in% names(x)),
            nadpisz %in% c(TRUE, FALSE), doPrezentacji %in% c(TRUE, FALSE),
            oszacowaniaDoCopy %in% c(TRUE, FALSE),
            as.integer(proba) == proba, proba == -1 | proba > 0)
  stopifnot(is.data.frame(x$skalowania))
  stopifnot(nrow(x$skalowania) == 1)

  P = odbcConnect(zrodloDanychODBC)
  on.exit(odbcClose(P))
  odbcSetAutoCommit(P, FALSE)

  idSkali = x$skalowania$id_skali
  skalowanie = x$skalowania$skalowanie
  x$skalowania$do_prezentacji = doPrezentacji
  message(" id_skali: ", idSkali, ", skalowanie ", skalowanie, ".",
          format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))

  # sprawdzanie, czy we wszystkich elementach mamy to samo skalowanie
  message("  Kontrola poprawności argumentów.")
  for (i in 1:length(x)) {
    if (names(x)[i] == "usunieteKryteria" | is.null(x[[i]])) {
      next
    }
    czy_to_samo_skalowanie(x[[i]], idSkali, skalowanie, names(x)[i])
  }

  # pobieranie danych z bazy
  # umożliwia łatwiejszą i bardziej przyjazną użytkownikowi obsługę błędów
  baza = list(
    skalowania =
      sqlExecute(P, "SELECT * FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
                 list(idSkali, skalowanie), fetch = TRUE, errors = TRUE, stringsAsFactors = FALSE),
    skalowania_grupy =
      sqlExecute(P, "SELECT * FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
                 list(idSkali, skalowanie), fetch = TRUE, errors = TRUE, stringsAsFactors = FALSE),
    skalowania_elementy =
      sqlExecute(P, "SELECT kolejnosc, parametr, uwagi, grupa FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
                 list(idSkali, skalowanie), fetch = TRUE, errors = TRUE, stringsAsFactors = FALSE),
    skalowania_obserwacje =
      sqlExecute(P, "SELECT id_obserwacji FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
                 list(idSkali, skalowanie), fetch = TRUE, errors = TRUE, stringsAsFactors = FALSE),
    normy =
      sqlExecute(P, "SELECT grupa, wartosc FROM normy WHERE id_skali = ? AND skalowanie = ?",
                 list(idSkali, skalowanie), fetch = TRUE, errors = TRUE, stringsAsFactors = FALSE)
  )
  wBazie = lapply(baza, function(x) {return(nrow(x) > 0)})
  wBazie = names(baza)[unlist(wBazie)]
  wX = lapply(x, function(x) {
    if (!is.data.frame(x)) {
      return(FALSE)
    } else {
      return(nrow(x) > 0)
    }
  })
  wX = names(x)[unlist(wX)]

  # obsługa nadpisz = TRUE
  if (nadpisz) {
    if (!(all(wBazie %in% wX))) {
      stop("Jeśli argument nadpisz=TRUE i W bazie istnieją już wpisy dotyczące ",
           "danego skalowania, zapisywane dane muszą obejmować wszystkie tablice, ",
           "w których istnieją już wpisy dla tego skalowania. Tablice, dla których ",
           "brak danych:\n-", paste0(setdiff(wBazie, wX), collapse = ",\n- "), ".")
    }
    # jeśli powyżej nie wybuchło, to kasujemy szerokim frontem
    kasowanie = list(
      skalowania_obserwacje =
        sqlExecute(P, "DELETE FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
                   list(idSkali, skalowanie), errors = TRUE),
      skalowania_elementy =
        sqlExecute(P, "DELETE FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
                   list(idSkali, skalowanie), errors = TRUE),
      normy =
        sqlExecute(P, "DELETE FROM normy WHERE id_skali = ? AND skalowanie = ?",
                   list(idSkali, skalowanie), errors = TRUE),
      skalowania_grupy =
        sqlExecute(P, "DELETE FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
                   list(idSkali, skalowanie), errors = TRUE),
      skalowania =
        sqlExecute(P, "DELETE FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
                   list(idSkali, skalowanie), errors = TRUE)
    )
    baza = lapply(baza, function(x) {return(x[0, ])})
    wBazie = wBazie[0]
  }

  # szukanie konfliktow przy nadpisz=FALSE
  if (!nadpisz) {
    wspolne = setdiff(intersect(wBazie, wX), "skalowania")
    if ("skalowania_grupy" %in% wspolne) {
      baza$skalowania_grupy$grupa[is.na(baza$skalowania_grupy$grupa)] = ""  # obchodzenie narowów RODBC
      temp = merge(baza$skalowania_grupy, x$skalowania_grupy)
      if (nrow(temp) != nrow(baza$skalowania_grupy)) {
        cat("skale w bazie\n")
        print(baza$skalowania_grupy)
        cat("skale do zapisania\n")
        print(x$skalowania_grupy)
        stop("W bazie istnieją już zapisane definicje grup dla tej skali ",
             "i nie są one zgodne z przekazanymi do zapisania.")
      }
    }
    wspolne = setdiff(wspolne, "skalowania_grupy")
    mapply(
      function(x, y, z) {
        if (!("grupa" %in% names(y))) {
          stop("Element '", z, "' musi zawierać kolumnę 'grupa'.")
        }
        y$grupa[y$grupa == ""] = NA
        temp = merge(x, y)
        if (nrow(temp) != (nrow(x) + nrow(y))) {
          stop("W elemencie '", z, "' wykryto konflikty z danymi, które są ",
               "już zapisane w bazie.")
        } else {
          invisible(NULL)
        }
      },
      baza[wspolne],
      x[wspolne],
      as.list(wspolne)
    )
  }

  # usuwanie kryteriów
  if (!is.null(x$usunieteKryteria) & nadpisz) {
    message("  Usuwanie ze skali (peudo)kryteriów o zbyt niskich wartościach dyskryminacji:")
    kryteria = x$usunieteKryteria[grep("^k_", x$usunieteKryteria)]
    kryteria = as.numeric(sub("^k_", "", kryteria))
    if (length(kryteria) > 0) {
      w = try(sqlExecute(P, "DELETE FROM skale_elementy WHERE id_skali = ? AND id_kryterium = ?",
                          list(idSkali, kryteria), errors = FALSE))
      if (w > 0) {
        message("   Usunięto ze skali kryteria/um o id_kryterium:\n   - ",
                paste0(kryteria, collapse = ",\n   - "), ".\n")
      }
    }
    pseudokryteria = x$usunieteKryteria[grep("^p_", x$usunieteKryteria)]
    pseudokryteria = as.numeric(sub("^p_", "", pseudokryteria))
    if (length(pseudokryteria) > 0) {
      w = try(sqlExecute(P, "DELETE FROM skale_elementy WHERE id_skali = ? AND id_pseudokryterium = ?",
                     list(idSkali, pseudokryteria), errors = FALSE))
      if (w > 0) {
        message("   Usunięto ze skali pseudokryteria/um o id_pseudokryterium:\n   - ",
                paste0(pseudokryteria, collapse = ",\n   - "), ".\n")
      }
    }
  } else if (!is.null(x$usunieteKryteria)) {
    if (length(x$usunieteKryteria) > 0) {
      warning("Wyniki skalowania zawierają listę kryteriów do usunięcia ze skali, ",
              "ale ponieważ argument 'nadpisz' ma wartość FALSE, ",
              "nie zostanie to wykonane.", immediate. = TRUE)
    }
  }

  # tworzenie nowego skalowania i grup
  if (!("skalowania" %in% wBazie)) {
    message("  Tworzenie w bazie nowego skalowania:")
    w = sqlExecute(P, uloz_insert_z_ramki("skalowania", x$skalowania),
                   x$skalowania, errors = TRUE)
    message("    Utworzono skalowanie nr ", skalowanie, ".")
  }
  if (!("skalowania_grupy" %in% wBazie)) {
    message("  Tworzenie w bazie nowych grup powiązanych ze skalowaniem:")
    w = sqlExecute(P, uloz_insert_z_ramki("skalowania_grupy", x$skalowania_grupy),
                   x$skalowania_grupy, errors = TRUE)
    message("    Utworzono ", nrow(x$skalowania_grupy), " grup(ę/y).")
  }

  # zapis parametrów modelu
  if (is.data.frame(x$skalowania_elementy)) {
    message("  Zapis wartości parametrów modelu.")
    idElementu = sqlExecute(P, "SELECT max(id_elementu) FROM skalowania_elementy",
                            fetch = TRUE, errors = TRUE)[1, 1]
    x$skalowania_elementy$id_elementu = 1:nrow(x$skalowania_elementy) + idElementu

    x$skalowania_elementy$grupowy[x$skalowania_elementy$grupowy %in% FALSE] = NA
    x$skalowania_elementy$parametr[x$skalowania_elementy$parametr %in% "dyskryminacja"] = "a"
    w = sqlExecute(P, uloz_insert_z_ramki("skalowania_elementy", x$skalowania_elementy),
                 x$skalowania_elementy, errors = TRUE)
    message("   Zapisano wartości ", nrow(x$skalowania_elementy), " parametrów.")
  }

  # zapis norm
  if (is.data.frame(x$normy)) {
    message("  Zapis normalizacji wyników surowych.")
    w = sqlExecute(P, uloz_insert_z_ramki("normy", x$normy),
                   x$normy, errors = TRUE)
    message("   Zapisano ", nrow(x$normy), " rekordów.")
  }

  # zapis oszacowań umiejętności
  if (is.data.frame(x$skalowania_obserwacje)) {
    message("  Zapis oszacowań umiejętności.")
    if (proba > 0) {
      x$skalowania_obserwacje =
        x$skalowania_obserwacje[sample(1:nrow(x$skalowania_obserwacje),
                                       min(proba, nrow(x$skalowania_obserwacje))), ]
    }
    if (oszacowaniaDoCopy) {
      nazwaPliku = paste0("oszacowania_", x$skalowania$id_skali, "_",
                          x$skalowania$skalowanie, ".csv")
      write.csv(x$skalowania_obserwacje, nazwaPliku, row.names = FALSE, na = "null")
      zip(sub("csv$", "zip", nazwaPliku), nazwaPliku)
      file.remove(nazwaPliku)
    } else {
      w = sqlExecute(P, uloz_insert_z_ramki("skalowania_obserwacje",
                                            x$skalowania_obserwacje),
                     x$skalowania_obserwacje, errors = TRUE)
      message("   Zapisano oszacowania ", nrow(x$skalowania_obserwacje), " zdających.")
    }
  }

  # koniec
  odbcEndTran(P, TRUE)
  message("  Zapis zakończony.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"), "\n")
  invisible(1)
}
# mała funkcja pomocnicza
czy_to_samo_skalowanie = function(x, idSkali, skalowanie, nazwa) {
  if (!all(c("id_skali", "skalowanie") %in% names(x))) {
    stop("W elemencie '", nazwa, "' brak kolumny 'id_skali' lub kolumny 'skalowanie'.")
  }
  if (!all(x$id_skali == idSkali) | !all(x$skalowanie == skalowanie)) {
    stop("W elemencie '", nazwa, "' dane mają przypisane inne id_skali lub nr skalowania.")
  } else {
    invisible(NULL)
  }
}
#' @title Zapis wynikow skalowania do bazy
#' @description
#' Funkcja przygotowuje składnię polecenie INSERT dla zapytania
#' parametryzowanego, na podstawie struktury ramki danych.
#' @param nazwa ciąg znaków - nazwa tablicy w bazie, do której dane mają być
#' zapisywane
#' @param ramka data frame, którego struktura odpowiada strukturze tablicy
#' podanej argumentem \code{nazwa}
#' @return ciąg znaków
uloz_insert_z_ramki = function(nazwa, ramka) {
  stopifnot(is.character(nazwa), length(nazwa) == 1,
            is.data.frame(ramka) | is.list(ramka))
  return(paste0("INSERT INTO ", nazwa, " (",
                paste0(names(ramka), collapse = ", "), ") VALUES (",
                paste0(rep("?", ncol(ramka)), collapse = ", "), ")"))
}
