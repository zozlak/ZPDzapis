#' @title Zapis normalizacji ekwikwantylowej do bazy
#' @description
#' Funkcja służy do zapisania do bazy obliczonych norm ekwikwantylowych.
#' @param P połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}
#' @param normy wektor opisujący unormowanie, typowo zwrócony przez funkcję
#' \code{\link[ZPD]{normalizuj}}
#' @param prefiksRok ciąg znaków postaci \code{'pr'}, gdzie \code{p} oznacza
#' prefiks części egzaminu, a \code{r} rok (zapis czterema cyframi)
#' @details
#' Funkcja nie dopuszcza braków danych w argumencie \code{normy}.
#' Funkcja wymaga też, aby w ramach połączenia nawiązywanego z bazą mieć prawa do
#' modyfikacji tablic 'skale' i 'normy_ekwikwantylowe'.
#' @return funkcja nic nie zwraca
#' @seealso \code{\link[ZPD]{normalizuj}}
#' @export
zapisz_normy = function(
  P,
  normy,
  prefiksRok
) {
  stopifnot(is.numeric(normy), length(normy) > 0, all(!is.na(normy)),
            is.character(prefiksRok), length(prefiksRok) == 1,
  )
  stopifnot(!is.null(names(normy)))
  stopifnot(all(!is.na(as.numeric(names(normy)))))

  stop("Funkcja wymaga przepisania")
  #   wartosci = as.numeric(names(normy))
  #   # sprawdzamy, czy odpowiednia skala już istnieje, a jeśli nie, to ją tworzymy
  #   nazwa = paste("ewd", sub("^([^[:digit:]]+).*", "\\1", prefiksRok), sub(".*([[:digit:]]{4}$)", "\\1", prefiksRok), sep=";")
  #   zapytanie = "SELECT * FROM skale WHERE nazwa = $1"
  #   skala = .sqlQuery(P, zapytanie, nazwa)
  #   if (nrow(skala) == 0) {
  #     zapytanie = "INSERT INTO skale (id_skali, opis, nazwa) VALUES (nextval('skale_id_skali_seq'), '', $1)"
  #     skala = .sqlQuery(P, zapytanie, nazwa)
  #     zapytanie = "SELECT * FROM skale WHERE nazwa = $1"
  #     skala = .sqlQuery(P, zapytanie, nazwa)
  #   }
  #   if (nrow(skala) > 1) stop("W bazie występuje więcej niż jedna skala o nazwie ", nazwa, ".")
  #   # sprawdzamy, czy dla tej skali są już w bazie jakieś normy
  #   zapytanie = "SELECT * FROM normy_ekwikwantylowe WHERE id_skali = $1"
  #   temp = .sqlQuery(P, zapytanie, skala$id_skali)
  #   if (nrow(temp) > 0) {
  #     stop("W bazie znajdują się już normy przypisane do skali ", skala$id_skali, ". Najpierw je usuń.")
  #   } else {
  #     zapytanie = "INSERT INTO normy_ekwikwantylowe (id_skali, wartosc, wartosc_zr) VALUES ($1, $2, $3)"
  #     temp = .sqlQuery(P, zapytanie, data.frame(id_skali=skala$id_skali, wartosc=wartosci, wartosc_zr=normy))
  #   }
  #   invisible()
}
