#' @title Tworzy w bazie skale zwiazane z obliczaniem wskaznikow EWD
#' @description
#' Dla podanego rodzaju egzaminu i roku funkcja tworzy odpowiednie skale,
#' potrzebne do obliczania wskaźników EWD, przypisujac do nich wszystkie
#' kryteria odpowiednich części egzaminu. Tworzy też (wykorzystując funkcję
#' \code{\link{stworz_test_z_wielu_czesci}}) testy odpowiadające skalom,
#' które obejmują kilka części egzaminu.
#'
#' Uwaga, skale tworzone są z flagą \code{do_prezentacji} ustawioną na
#' \code{FALSE}.
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param rodzajEgzaminu ciąg znaków
#' @param rok liczba całkowita
#' @param sufiks ciąg znaków - sufiks dodawany do opisów skal (nie jest dodawany
#' do opisu tworzonych testów, ani nazw skal raschowych)
#' @param czyRasch wartość logiczna - czy tworzyć również skale raschowe do
#' Kalkulatora EWD?
#' @param dopisz wartość logiczna - czy jeśli istnieją już w bazie jakieś skale
#' spośród tych, które ma utworzyć funkcja, to pominąć je i utworzyć pozostałe
#' (zamiast nie zapisać nic i zwrócić błąd)?
#' @return wektor liczbowy zawierający id_skali utworzonych skal
#' @export
#' @importFrom stats setNames
stworz_skale_ewd = function(
  P,
  rodzajEgzaminu,
  rok,
  sufiks = "",
  czyRasch = TRUE,
  dopisz = FALSE
) {
  stopifnot(is.character(rodzajEgzaminu)       , length(rodzajEgzaminu) == 1,
            is.numeric(rok)                    , length(rok) > 0,
            is.character(sufiks)               , length(sufiks) == 1,
            all(czyRasch %in% c(TRUE, FALSE))  , length(czyRasch) == 1,
            all(dopisz %in% c(TRUE, FALSE))    , length(dopisz) == 1
  )
  stopifnot(all(as.integer(rok) == rok), rok >= 2002, rok <= 2022)
  stopifnot(rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura"))

  DBI::dbBegin(P)

  # tworzenie listy ze skalami i powiązanymi z nimi częściami egzaminów
  if (rodzajEgzaminu == "sprawdzian") {
    skale = list(
      "ewd;s" = c("")
    )
    if (czyRasch) {
      skale = append(skale, list("ewd;sR" = c("")))
    }
  } else if (rodzajEgzaminu == "egzamin gimnazjalny") {
    if (rok < 2012) {
      skale = list(
        "ewd;gh" = c("humanistyczna"),
        "ewd;gm" = c("matematyczno-przyrodnicza")
      )
      if (czyRasch) {
        skale = append(skale, list(
          "ewd;ghR" = c("humanistyczna"),
          "ewd;gmR" = c("matematyczno-przyrodnicza"))
        )
      }
    } else {
      skale = list(
        "ewd;gh"   = c("j. polski", "historia i WOS"),
        "ewd;gh_h" = c("historia i WOS"),
        "ewd;gh_p" = c("j. polski"),
        "ewd;gm"   = c("matematyka", "przedmioty przyrodnicze"),
        "ewd;gm_m" = c("matematyka"),
        "ewd;gm_p" = c("przedmioty przyrodnicze")
      )
      if (czyRasch) {
        skale = append(skale, list(
          "ewd;ghR"   = c("j. polski", "historia i WOS"),
          "ewd;gh_hR" = c("historia i WOS"),
          "ewd;gh_pR" = c("j. polski"),
          "ewd;gmR"   = c("matematyka", "przedmioty przyrodnicze"),
          "ewd;gm_mR" = c("matematyka"),
          "ewd;gm_pR" = c("przedmioty przyrodnicze"))
        )
      }
    }
  } else if (rodzajEgzaminu == "matura") {
    if (rok <= 2016) {
      skale = list(
        "ewd;m_h"  = c("j. polski podstawowa", "j. polski rozszerzona",
                       "historia podstawowa",  "historia rozszerzona",
                       "WOS podstawowa",       "WOS rozszerzona"),
        "ewd;m_jp" = c("j. polski podstawowa", "j. polski rozszerzona"),
        "ewd;m_m"  = c("matematyka podstawowa", "matematyka rozszerzona"),
        "ewd;m_mp" = c("matematyka podstawowa",  "matematyka rozszerzona",
                       "biologia podstawowa",    "biologia rozszerzona",
                       "chemia podstawowa",      "chemia rozszerzona",
                       "fizyka podstawowa",      "fizyka rozszerzona",
                       "geografia podstawowa",   "geografia rozszerzona",
                       "informatyka podstawowa", "informatyka rozszerzona")
      )
    } else {
      skale = list(
        "ewd;m_h"  = c("j. polski podstawowa", "j. polski rozszerzona",
                       "historia rozszerzona",
                       "WOS rozszerzona"),
        "ewd;m_jp" = c("j. polski podstawowa", "j. polski rozszerzona"),
        "ewd;m_m"  = c("matematyka podstawowa", "matematyka rozszerzona"),
        "ewd;m_mp" = c("matematyka podstawowa",  "matematyka rozszerzona",
                       "biologia rozszerzona",
                       "chemia rozszerzona",
                       "fizyka rozszerzona",
                       "geografia rozszerzona",
                       "informatyka rozszerzona")
      )
    }
    if (czyRasch) {
      skale = append(skale, list(
        "ewd;m_jpR" = c("j. polski podstawowa", "j. polski rozszerzona"),
        "ewd;m_mR"  = c("matematyka podstawowa", "matematyka rozszerzona"))
      )
    }
  }
  testy = sub("R$", "", sub("^ewd;", "", names(skale)))
  maskaNieRasch = !grepl("R$", names(skale))
  names(skale) = paste0(names(skale), ";", rok)
  if (sufiks != "") {
    names(skale)[maskaNieRasch] =
      paste0(names(skale)[maskaNieRasch], ";", sub("^;","", sufiks))
  }
  # mapowanie opisów skal na opisy testów
  testyMapa = list(
    "gh"   = c("humanistyczna", "humanistyczna"),
    "gm"   = c("matematyczno-przyrodnicza", "matematyczno-przyrodnicza"),
    "m_h"  = c("humanistyczna", NA),
    "m_jp" = c("polski", NA),
    "m_m"  = c("matematyka", NA),
    "m_mp" = c("matematyczno-przyrodnicza", NA)
  )
  if (!all(testy[unlist(lapply(skale, length)) > 1] %in% names(testyMapa))) {
    stop("Niektórych opisów skal nie udało się zmapować na opisy testów. Popraw kod funkcji.")
  }
  # sprawdźmy, czy aby skale o takich opisach nie są już zarejestrowane
  skaleWBazie = .sqlQuery(P, "SELECT opis FROM skale")
  skaleWBazie = names(skale)[names(skale) %in% skaleWBazie$opis]
  if (length(skaleWBazie) > 0 & !dopisz) {
    stop("W bazie istnieją już zarejestrowane skale o opisie:\n ",
         paste0(paste0("'", skaleWBazie, "'"), collapse = ",\n "), ".\n",
         "Podaj inny sufiks, aby utworzyć skale o unikalnym opisie, ",
         "lub wywołaj funkcję z argumentem 'dopisz = TRUE'.")
  } else if (length(skaleWBazie) > 0) {
    warning("W bazie istnieją już zarejestrowane skale o opisie:\n ",
            paste0(paste0("'", skaleWBazie, "'"), collapse = ",\n "), ".")
    maska = !(names(skale) %in% skaleWBazie)
    skale = skale[maska]
    testy = testy[maska]
    if (length(skale) == 0) {
      return(vector(mode = "numeric", length = 0))
    }
  }
  # ustawianie, do jakich danych (EWD/CKE) należy się podpinać
  if ((rodzajEgzaminu == "sprawdzian" & rok < 2003) |
      (rodzajEgzaminu == "egzamin gimnazjalny" & rok < 2006) |
      (rodzajEgzaminu == "matura" & rok < 2010)) {
    czyEwd = FALSE
  } else {
    czyEwd = TRUE
  }

  # ruszamy do pracy
  idSkal = setNames(rep(NA, length(skale)), names(skale))
  for (i in 1:length(skale)) {
    # wyszukiwanie id_testów
    message("Tworzenie skali '", names(skale)[i], "'.")
    idTestow = vector(mode = "list", length = length(skale[[i]]))
    for (j in 1:length(idTestow)) {
      idTestow[[j]] = .sqlQuery(
        P,
        "SELECT id_testu
          FROM arkusze JOIN testy USING (arkusz)
          WHERE
            arkusze.rodzaj_egzaminu = $1
            AND arkusze.czesc_egzaminu = $2
            AND EXTRACT(YEAR FROM data_egzaminu) = $3
            AND ewd = $4
        ",
        list(rodzajEgzaminu, skale[[i]][j], rok, czyEwd)
      )
      if (nrow(idTestow[[j]]) == 0) {
        stop("Nie udało się znaleźć testów z wynikami egzaminu '", rodzajEgzaminu,
             "', w roku ", rok, " ze źródła ewd=", czyEwd, ".")
      }
    }
    # wyszukiwanie kryteriow oceny
    idTestow = unique(unlist(idTestow))
    zapytanie = paste0("
      SELECT id_testu, id_kryterium, kolejnosc
      FROM kryteria_oceny INNER JOIN testy_kryteria USING (id_kryterium)
      WHERE id_testu in (", .sqlPlaceholders(idTestow), ")
      ORDER BY id_testu, kolejnosc, id_kryterium
    ")
    kryteria = .sqlQuery(P, zapytanie, idTestow)
    kryteria = kryteria[!duplicated(kryteria$id_kryterium), ]
    # ewentualne tworzenie nowego testu (jeśli skala obejmuje wiele części egzaminu)
    if (length(skale[[i]]) > 1) {
      opisTestu = paste(rodzajEgzaminu, testyMapa[testy[i]][[1]][1], rok, sep = ";")
      idPseudtestu = .sqlQuery(
        P,
        "SELECT id_testu FROM testy WHERE opis = $1",
        opisTestu
      )[, 1]
      if (length(idPseudtestu) == 0) {
        message(" Tworzenie nowego testu: '", opisTestu, "' (może chwilę potrwać...).")
        idPseudtestu = stworz_test_z_wielu_czesci(
          P, rodzajEgzaminu, skale[[i]],
          rok, czyEwd, opisTestu,
          testyMapa[testy[i]][[1]][2],
          pominTransakcje = TRUE
        )
      }
      # przyłączanie kryteriów do tego testu
      wBazie = .sqlQuery(
        P,
        "SELECT count(id_kryterium) FROM testy_kryteria WHERE id_testu = $1",
        idPseudtestu
      )[1, 1]
      if (wBazie == 0) {
        message(" Wypełnianie tablicy 'testy_kryteria' dla testu '", opisTestu, "'.")
        kryteria$id_testu = idPseudtestu
        kryteria = cbind(kryteria, popr_dystraktor = NA)
        .sqlQuery(
          P,
          "INSERT INTO testy_kryteria (id_testu, id_kryterium,kolejnosc, popr_dystraktor) VALUES ($1, $2, $3, $4)",
          kryteria
        )
      }
      idTestow = c(idTestow, idPseudtestu)
    }
    # rejestrowanie skali
    idSkal[i] = stworz_skale(P, names(skale)[i], "ewd", FALSE, idTestow,
                             pominTransakcje = TRUE)
    # przyłączanie kryteriów do skali
    kryteria = data.frame(
      kolejnosc = 1:nrow(kryteria),
      id_skali = unname(idSkal[i]),
      kryteria$id_kryterium
    )
    .sqlQuery(
      P,
      "INSERT INTO skale_elementy (kolejnosc, id_skali, id_kryterium) VALUES ($1, $2, $3)",
      kryteria
    )
    # w przypadku skal raschowych spr. i egz. gimn. - dodawanie skalowania
    # dla normalizacji ekwikwantylowej
    temp = gsub("^[^;]+;([^;]+);.*$", "\\1", names(skale)[i])
    if (substr(temp, nchar(temp), nchar(temp)) == "R" &
        substr(temp, 1, 1) %in% c("s", "g")) {
      message(" Dodawanie skalowania dla normalizacji ekwikwantylowej.")
      skalowania = data.frame(
        skalowanie = 1,
        opis = 'normalizacja ekwikwantylowa EWD',
        estymacja = 'nie dotyczy',
        id_skali = idSkal[i],
        do_prezentacji = FALSE,
        data = Sys.Date()
      )
      .sqlQuery(
        P,
        "INSERT INTO skalowania (skalowanie, opis, estymacja, id_skali, do_prezentacji, data) VALUES ($1, $2, $3, $4, $5, $6)",
        skalowania
      )
      .sqlQuery(
        P,
        "INSERT INTO skalowania_grupy (id_skali, skalowanie, grupa) VALUES ($1, $2, $3)",
        data.frame(id_skali = idSkal[i], skalowanie = 1, grupa = "")
      )
    }
  }

  DBI::dbCommit(P)
  return(idSkal)
}
