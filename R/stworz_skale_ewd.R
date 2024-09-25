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
#' @param BK wartość logiczna - czy tworzyć oddzelne skale dla poszczególnych
#' przedmiotów matury, zgodnie ze zmodyfikowanym podejściem do obliczania
#' maryralnej EWD, zaproponowanym w 2022 r. przez Bartosza Kondratka?
#' (może być \code{TRUE} tylko gdy \code{rodzaj_egzaminu="matura"})
#' @details Uwaga, w przypadku skal wykorzystywanych do obliczania latentnych
#' wskaźników EWD (tj. przy wywołaniu z argumentem \code{BK=TRUE})
#' \strong{wywołanie funkcji z argumentem \code{rodzajEgzaminu="matura"} tworzy
#' również skale dla egzaminu na wejściu}.
#' @return wektor liczbowy zawierający id_skali utworzonych skal
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr  %>% .data filter n_distinct select
stworz_skale_ewd = function(
  P,
  rodzajEgzaminu = c("sprawdzian", "egzamin gimnazjalny", "matura",
                     "egzamin ósmoklasisty"),
  rok,
  sufiks = "",
  czyRasch = FALSE,
  dopisz = FALSE,
  BK = FALSE
) {
  stopifnot(is.character(rodzajEgzaminu)     , length(rodzajEgzaminu) == 1,
            is.numeric(rok)                  , length(rok) == 1,
            is.character(sufiks)             , length(sufiks) == 1,
            all(czyRasch %in% c(TRUE, FALSE)), length(czyRasch) == 1,
            all(dopisz %in% c(TRUE, FALSE))  , length(dopisz) == 1,
            all(BK %in% c(TRUE, FALSE))      , length(BK) == 1
  )
  stopifnot(BK == FALSE || rodzajEgzaminu == "matura")
  stopifnot(all(as.integer(rok) == rok), rok >= 2002, rok <= 2024)
  rodzajEgzaminu = match.arg(rodzajEgzaminu)
  if (BK && czyRasch) {
    warning("Argument `BK=TRUE`, więc argument `czyRasch=TRUE zostanie zignorowany (tj. nie zostaną utworzone skale raschowe).` ")
  }

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
        "ewd;gh" = list(czesci = "humanistyczna",
                        lata = rok),
        "ewd;gm" = list(czesci = "matematyczno-przyrodnicza",
                        lata = rok)
      )
      if (czyRasch) {
        skale = append(skale, list(
          "ewd;ghR" = list(czesci = "humanistyczna",
                           lata = rok),
          "ewd;gmR" = list(czesci = "matematyczno-przyrodnicza",
                           lata = rok))
        )
      }
    } else if (!BK) {
      skale = list(
        "ewd;gh"   = list(czesci = c("j. polski", "historia i WOS"),
                          lata = rok),
        "ewd;gh_h" = list(czesci = c("historia i WOS"),
                          lata = rok),
        "ewd;gh_p" = list(czesci = c("j. polski"),
                          lata = rok),
        "ewd;gm"   = list(czesci = c("matematyka", "przedmioty przyrodnicze"),
                          lata = rok),
        "ewd;gm_m" = list(czesci = c("matematyka"),
                          lata = rok),
        "ewd;gm_p" = list(czesci = c("przedmioty przyrodnicze"),
                          lata = rok)
      )
      if (czyRasch) {
        skale = append(skale, list(
          "ewd;ghR"   = list(czesci = c("j. polski", "historia i WOS"),
                             lata = rok),
          "ewd;gh_hR" = list(czesci = c("historia i WOS"),
                             lata = rok),
          "ewd;gh_pR" = list(czesci = c("j. polski"),
                             lata = rok),
          "ewd;gmR"   = list(czesci = c("matematyka", "przedmioty przyrodnicze"),
                             lata = rok),
          "ewd;gm_mR" = list(czesci = c("matematyka"),
                             lata = rok),
          "ewd;gm_pR" = list(czesci = c("przedmioty przyrodnicze"),
                             lata = rok))
        )
      }
    }
  } else if (rodzajEgzaminu == "matura" && !BK) {
    if (rok <= 2016) {
      skale = list(
        "ewd;m_h"  = list(czesci = c("j. polski podstawowa", "j. polski rozszerzona",
                                     "historia podstawowa",  "historia rozszerzona",
                                     "WOS podstawowa",       "WOS rozszerzona"),
                          lata = rok),
        "ewd;m_jp" = list(czesci = c("j. polski podstawowa", "j. polski rozszerzona"),
                          lata = rok),
        "ewd;m_m"  = list(czesci = c("matematyka podstawowa", "matematyka rozszerzona"),
                          lata = rok),
        "ewd;m_mp" = list(czesci = c("matematyka podstawowa",  "matematyka rozszerzona",
                                     "biologia podstawowa",    "biologia rozszerzona",
                                     "chemia podstawowa",      "chemia rozszerzona",
                                     "fizyka podstawowa",      "fizyka rozszerzona",
                                     "geografia podstawowa",   "geografia rozszerzona",
                                     "informatyka podstawowa", "informatyka rozszerzona"),
                          lata = rok)
      )
    } else {
      skale = list(
        "ewd;m_h"  = list(czesci = c("j. polski podstawowa", "j. polski rozszerzona",
                                     "historia rozszerzona",
                                     "WOS rozszerzona"),
                          lata = rok),
        "ewd;m_jp" = list(czesci = c("j. polski podstawowa", "j. polski rozszerzona"),
                          lata = rok),
        "ewd;m_m"  = list(czesci = c("matematyka podstawowa", "matematyka rozszerzona"),
                          lata = rok),
        "ewd;m_mp" = list(czesci = c("matematyka podstawowa",  "matematyka rozszerzona",
                                     "biologia rozszerzona",
                                     "chemia rozszerzona",
                                     "fizyka rozszerzona",
                                     "geografia rozszerzona",
                                     "informatyka rozszerzona"),
                          lata = rok)
      )
    }
    if (czyRasch) {
      skale = append(skale, list(
        "ewd;m_jpR" = list(czesci = c("j. polski podstawowa", "j. polski rozszerzona"),
                           lata = rok),
        "ewd;m_mR"  = list(czesci = c("matematyka podstawowa", "matematyka rozszerzona"),
                           lata = rok))
      )
    }
  } else if (rodzajEgzaminu == "matura" && BK) {
    skale = list(
      "ewd;m_jp" = list(czesci = c("j. polski podstawowa", "j. polski rozszerzona"),
                        lata = rok),
      "ewd;m_m"  = list(czesci = c("matematyka podstawowa", "matematyka rozszerzona"),
                        lata = rok),
      "ewd;m_b"  = list(czesci = "biologia rozszerzona",
                        lata = rok),
      "ewd;m_c"  = list(czesci = "chemia rozszerzona",
                        lata = rok),
      "ewd;m_f"  = list(czesci = "fizyka rozszerzona",
                        lata = rok),
      "ewd;m_g"  = list(czesci = "geografia rozszerzona",
                        lata = rok),
      "ewd;m_h"  = list(czesci = "historia rozszerzona",
                        lata = rok),
      "ewd;m_i"  = list(czesci = "informatyka rozszerzona",
                        lata = rok),
      "ewd;m_w"  = list(czesci = "WOS rozszerzona",
                        lata = rok),
      "ewd;m_ja" = list(czesci = c("j. angielski podstawowa", "j. angielski rozszerzona"),
                        lata = rok)
    )
    if (rok <= 2022) {
      skale = append(
        skale,
        list(
          "ewd;ghLO" = list(czesci = c("j. polski", "historia i WOS"),
                            lata = (rok - 3L):(rok - 4L)),
          "ewd;gmLO" = list(czesci = c("matematyka", "przedmioty przyrodnicze"),
                            lata = (rok - 3L):(rok - 4L)),
          "ewd;gjaLO" = list(czesci = "j. angielski rozszerzona",
                             lata = (rok - 3L):(rok - 4L))
        ))
    } else if (rok == 2023) {
      skale = append(
        skale,
        list(
          "ewd;e8jpLO" = list(czesci = "j. polski",
                              lata = rok - 4L),
          "ewd;e8mLO" = list(czesci = "matematyka",
                             lata = rok - 4L),
          "ewd;e8jaLO" = list(czesci = "j. angielski",
                              lata = rok - 4L)
        ))
    } else {
      skale = append(
        skale,
        list(
          "ewd;e8jpLO" = list(czesci = "j. polski",
                              lata = (rok - 4L):(rok - 5L)),
          "ewd;e8mLO" = list(czesci = "matematyka",
                             lata = (rok - 4L):(rok - 5L)),
          "ewd;e8jaLO" = list(czesci = "j. angielski",
                              lata = (rok - 4L):(rok - 5L)),
          # skale do przeprowadzenia procedury łączenia kryteriów
          "ewd;e8jp" = list(czesci = "j. polski",
                            lata = rok - 4L),
          "ewd;e8m" = list(czesci = "matematyka",
                           lata = rok - 4L),
          "ewd;e8ja" = list(czesci = "j. angielski",
                            lata = rok - 4L)
        ))
    }
    if (rok <= 2023) {
      skale = append(
        skale,
        list(
          "ewd;ghT"  = list(czesci = c("j. polski", "historia i WOS"),
                            lata = (rok - 4L):(rok - 5L)),
          "ewd;gmT"  = list(czesci = c("matematyka", "przedmioty przyrodnicze"),
                            lata = (rok - 4L):(rok - 5L))
        ))
    } else if (rok == 2024) {
      skale = append(
        skale,
        list(
          "ewd;e8jpT" = list(czesci = "j. polski",
                             lata = rok - 5L),
          "ewd;e8mT" = list(czesci = "matematyka",
                            lata = rok - 5L),
          "ewd;e8jaT" = list(czesci = "j. angielski",
                             lata = rok - 5L)
        ))
    } else {
      skale = append(
        skale,
        list(
          "ewd;e8jpT" = list(czesci = "j. polski",
                             lata = (rok - 5L):(rok - 6L)),
          "ewd;e8mT" = list(czesci = "matematyka",
                            lata = (rok - 5L):(rok - 6L)),
          "ewd;e8jaT" = list(czesci = "j. angielski",
                             lata = (rok - 5L):(rok - 6L))
        ))
    }
  }
  # Obsługa faktu, że dla 2023 r. skale zgodnie ze "starym" schematem tworzone
  # są tylko dla starszej formuły matury (zdawanej w technikach), a skale
  # zgodnie ze schematem "BK" są tworzone tylko dla nowszej formuły matury
  # (zdawanej w LO)
  skale = lapply(skale, append, list(maskaArkusz = ""))
  if (rok == 2023 && rodzajEgzaminu == "matura") {
    if (!BK) {
      skale = lapply(skale,
                     function(x) {
                       x$maskaArkusz = "^E"
                       return(x)})
    } else {
      skale[grep("^ewd;m_", names(skale))] =
        lapply(skale[grep("^ewd;m_", names(skale))],
               function(x) {
                 x$maskaArkusz = "^M"
                 return(x)})
    }
  }
  testy = sub("R$", "", sub("^ewd;", "", names(skale)))
  maskaNieRasch = !grepl("R$", names(skale))
  names(skale) = paste0(names(skale), ";",
                        sapply(skale,
                               function(x) {
                                 return(paste(x$lata, collapse = ""))}))
  if (sufiks != "") {
    names(skale)[maskaNieRasch] =
      paste0(names(skale)[maskaNieRasch], ";", sub("^;","", sufiks))
  }
  # mapowanie opisów skal na opisy testów
  # (muszą tu zostać podane tylko te skale, które składają się z kilku testów,
  #  w związku z czym konieczne jest utworzenie dla nich pseudotestu)
  testyMapa = list(
    #        c("opis testu", "opis pseudotestu")
    "gh"   = c("humanistyczna", "humanistyczna"),
    "gm"   = c("matematyczno-przyrodnicza", "matematyczno-przyrodnicza"),
    "ghLO" = c("humanistyczna", "humanistyczna"),
    "ghT"  = c("humanistyczna", "humanistyczna"),
    "gmLO" = c("matematyczno-przyrodnicza", "matematyczno-przyrodnicza"),
    "gmT"  = c("matematyczno-przyrodnicza", "matematyczno-przyrodnicza"),
    "e8jp" = c("polski", NA_character_),
    "e8jpLO" = c("polski", NA_character_),
    "e8jpT"  = c("polski", NA_character_),
    "e8m" = c("matematyka", NA_character_),
    "e8mLO" = c("matematyka", NA_character_),
    "e8mT"  = c("matematyka", NA_character_),
    "e8ja" = c("angielski", NA_character_),
    "e8jaLO" = c("angielski", NA_character_),
    "e8jaT"  = c("angielski", NA_character_),
    "m_h"  = c("humanistyczna", NA_character_),
    "m_jp" = c("polski", NA_character_),
    "m_m"  = c("matematyka", NA_character_),
    "m_mp" = c("matematyczno-przyrodnicza", NA_character_),
    "m_b"  = c("biologia", NA_character_),
    "m_c"  = c("chemia", NA_character_),
    "m_f"  = c("fizyka", NA_character_),
    "m_g"  = c("geografia", NA_character_),
    "m_h"  = c("historia", NA_character_),
    "m_i"  = c("informatyka", NA_character_),
    "m_w"  = c("WOS", NA_character_),
    "m_ja"  = c("angielski", NA_character_)
  )
  # Obsługa faktu, że dla 2023 r. skale zgodnie ze "starym" schematem tworzone
  # są tylko dla starszej formuły matury (zdawanej w technikach), a skale
  # zgodnie ze schematem "BK" są tworzone tylko dla nowszej formuły matury
  # (zdawanej w LO)
  if (rok == 2023) {
    testyMapa[grep("^m_", names(testyMapa))] =
      lapply(testyMapa[grep("^m_", names(testyMapa))],
             function(x) ifelse(is.na(x), x, paste0(x, ifelse(BK, "NF", "SF"))))
  }
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
  # przerabianie elementów `skale` na ramki danych
  skale = mapply(
    function(x, rodzajEgzaminu) {
      return(data.frame(rodzaj_egzaminu = rodzajEgzaminu,
                        expand.grid(czesc_egzaminu = x$czesci,
                                    rok = x$lata,
                                    stringsAsFactors = FALSE),
                        maskaArkusz = x$maskaArkusz))
    },
    skale,
    c("s" = "sprawdzian",
      "g" = "egzamin gimnazjalny",
      "m" = "matura",
      "e" = "egzamin ósmoklasisty")[substr(testy, 1, 1)],
    SIMPLIFY = FALSE)
  stopifnot(!any(sapply(skale, function(x) any(is.na(x$rodzaj_egaminu)))))
  # ruszamy do pracy
  idSkal = setNames(rep(NA, length(skale)), names(skale))
  for (i in seq_along(skale)) {
    # wyszukiwanie id_testów
    message("Tworzenie skali '", names(skale)[i], "'.")
    idTestow = vector(mode = "list", length = nrow(skale[[i]]))
    for (j in seq_along(idTestow)) {
      idTestow[[j]] = .sqlQuery(
        P,
        "SELECT id_testu, arkusz
          FROM arkusze JOIN testy USING (arkusz)
          WHERE
            arkusze.rodzaj_egzaminu = $1
            AND arkusze.czesc_egzaminu = $2
            AND EXTRACT(YEAR FROM data_egzaminu) = $3
            AND ewd = $4
        ",
        list(skale[[i]]$rodzaj_egzaminu[j], skale[[i]]$czesc_egzaminu[j],
             skale[[i]]$rok[j], czyEwd)
      ) %>%
        filter(grepl(skale[[i]]$maskaArkusz[1], .data$arkusz)) %>%
        select("id_testu")
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
    if (n_distinct(skale[[i]]$czesc_egzaminu) > 1) {
      rodzajEgzaminuSkala = unique(skale[[i]]$rodzaj_egzaminu)
      lata = sort(unique(skale[[i]]$rok), decreasing = TRUE)
      opisyTestow = paste(rodzajEgzaminuSkala, testyMapa[testy[i]][[1]][1],
                          lata, sep = ";")
      for (j in seq_along(opisyTestow)) {
        idPseudtestu = .sqlQuery(
          P,
          "SELECT id_testu FROM testy WHERE opis = $1",
          opisyTestow[j]
        )[, 1]
        if (length(idPseudtestu) == 0) {
          message(" Tworzenie nowego testu: '", opisyTestow[j], "' (może chwilę potrwać...).")
          idPseudtestu = stworz_test_z_wielu_czesci(
            P, rodzajEgzaminuSkala, skale[[i]]$czesc_egzaminu, lata[j], czyEwd,
            opisyTestow[j], testyMapa[testy[i]][[1]][2],
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
          message(" Wypełnianie tablicy 'testy_kryteria' dla testu '", opisyTestow[j], "'.")
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
    }
    # rejestrowanie skali
    idSkal[i] = stworz_skale(P, names(skale)[i], "ewd", FALSE, idTestow,
                             pominTransakcje = TRUE)
    # przyłączanie kryteriów do skali
    kryteria = data.frame(
      kolejnosc = seq_len(nrow(kryteria)),
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
