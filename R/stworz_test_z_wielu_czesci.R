#' @title Tworzenie testów będących złączeniem kilku części egzaminu.
#' @description
#' Funkcja tworzy nowy test ze wskazanych części wskazanego (poprzez rodzaj i rok)
#' egzaminu i wypełnia tablicę \code{dane_osobowe.testy_obserwacje} uczniami, którzy
#' wystąpili w przynajmniej jednej z części wskazanego egzaminu.
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param rodzajEgzaminu rodzaj egzaminu ("sprawdzian" / "egzamin gimnazjalny" / "matura")
#' @param czesciEgzaminu wektor części egzaminu, które mają zostać połączone
#' @param rokEgzaminu rok egzaminu
#' @param czyEwd czy dane mają pochodzić z testów EWD
#' @param opis opis nowotworzonego testu w tablic \code{testy}
#' @param czescEgzaminuZapisz opcjonalnie nazwa części egzaminu, która zostanie
#' przypisana nowej skali w kolumnie \code{czesz_egzaminu} tablicy \code{testy}
#' (znajduje zastosowanie w odniesieniu do egzaminu gimnazjalnego)
#' @param pominTransakcje wartość logiczna pozwalająca wywołać funkcję tak, aby nie używała
#' transakcji - co do zasady nie należy stosować (przydatne tylko, jako sposób na uniknięcie
#' błędu DBI/RPostgres związanego z brakiem obsługi zagnieżdżonych transakcji)
#' @details
#' Tworzony test posiada datę, jak najstarsza z data części egzaminu, z których powstał
#' oraz \code{ewd} zgodne z testami, z których powstał.
#'
#' Spośród kolumn tablicy \code{dane_osobowe.testy_obserwacje} kopiowana jest tylko
#' kolumna \code{id_szkoly}, natomiast kolumna \code{zrodlo} ustawiana jest na wartość
#' \code{baza}. Pozostałe kolumny (np. \code{dysleksja, klasa}, itp.) ustawiane są na
#' \code{NULL}.
#'
#' Jeśli w bazie istnieje już test o takim samym opisie, zrówcone zostanie id_testu tego
#' testu i jednocześnie wygenerowany zostanie warning informujący o tym, że nowy test nie
#' został utworzony. Uwaga! Nie gwarantuje to, że istniejący w bazie test składa się
#' z identycznych testów składowych - porównanie bazuje tylko na opisie testu, który jest
#' ustawiany arbitralnie przez tworządego test.
#' @return id_testu utworzonego testu
#' @export
stworz_test_z_wielu_czesci = function(
  P,
  rodzajEgzaminu,
  czesciEgzaminu,
  rokEgzaminu,
  czyEwd,
  opis,
  czescEgzaminuZapisz = NA,
  pominTransakcje = FALSE
){
  stopifnot(
    is.character(rodzajEgzaminu)  , length(rodzajEgzaminu) == 1,
    is.character(czesciEgzaminu)  , length(czesciEgzaminu) > 1,
    is.numeric(rokEgzaminu)       , length(rokEgzaminu) == 1,
    is.logical(czyEwd)            , length(czyEwd) == 1,
    is.character(opis)            , length(opis) == 1,
    is.character(czescEgzaminuZapisz) | is.na(czescEgzaminuZapisz),
    length(czescEgzaminuZapisz) == 1,
    opis[1] != '',
    is.logical(pominTransakcje)   , length(pominTransakcje) == 1,
    pominTransakcje %in% c(TRUE, FALSE)
  )

  czyJest = .sqlQuery(P, "SELECT id_testu FROM testy WHERE opis = $1", opis)
  if (nrow(czyJest) > 0) {
    warning('w bazie istnieje już taki test')
    return(czyJest[1])
  }

  zapytanie = paste0("
        SELECT id_testu, data_egzaminu
        FROM testy JOIN arkusze USING (arkusz)
        WHERE
          arkusze.rodzaj_egzaminu = $1
          AND extract(year FROM data_egzaminu) = $2
          AND ewd = $3
          AND arkusze.czesc_egzaminu IN (", .sqlPlaceholders(czesciEgzaminu, 4), ")
        ORDER BY data")
  dane = append(list(rodzajEgzaminu, rokEgzaminu, czyEwd), as.list(czesciEgzaminu))
  testy = .sqlQuery(P, zapytanie, dane)

  stopifnot(nrow(testy) > 0)

  if (!pominTransakcje) DBI::dbBegin(P)

  # Pobierz testy składowe
  idTestu = as.integer(.sqlQuery(P, "SELECT nextval('testy_id_testu_seq')")[1, 1])
  zapytanie = "
        INSERT INTO testy (id_testu, opis, data, ewd, arkusz, rodzaj_egzaminu, czesc_egzaminu)
        VALUES ($1, $2, $3, $4, null, $5, $6)"
  dane = data.frame(
    idTestu,
    opis,
    testy$data_egzaminu[1],
    czyEwd,
    rodzajEgzaminu,
    czescEgzaminuZapisz
  )
  .sqlQuery(P, zapytanie, dane)

  # Przygotuj zapytania źródłowe
  coalesce = function(testy, kolumna){
    paste0('t', seq_along(testy$id_testu), '.', kolumna, collapse = ', ')
  }
  zapFrom = paste0("
    FULL JOIN (
      SELECT id_obserwacji, id_szkoly, oke
      FROM dane_osobowe.testy_obserwacje
      WHERE id_testu = $", (1 + seq_along(testy$id_testu)), "
    ) AS t", seq_along(testy$id_testu), " USING (id_obserwacji)",
    collapse = ''
  )
  zapFrom = sub('FULL JOIN', '', zapFrom)
  zapFrom = sub('USING [(]id_obserwacji[)]', '', zapFrom)

  # Wykonaj wstawianie
  zapytanie = paste0("
    INSERT INTO dane_osobowe.testy_obserwacje (id_obserwacji, id_testu, id_szkoly, zrodlo)
    SELECT id_obserwacji, $1, COALESCE(", coalesce(testy, 'id_szkoly'), "), 'baza'
    FROM ", zapFrom,
    collapse = ''
  )
  .sqlQuery(P, zapytanie, c(idTestu, testy$id_testu))

  if (!pominTransakcje) DBI::dbCommit(P)

  return(idTestu)
}
