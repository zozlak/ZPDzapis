% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stworz_skale_ewd.R
\name{stworz_skale_ewd}
\alias{stworz_skale_ewd}
\title{Tworzy w bazie skale zwiazane z obliczaniem wskaznikow EWD}
\usage{
stworz_skale_ewd(
  P,
  rodzajEgzaminu = c("sprawdzian", "egzamin gimnazjalny", "matura",
    "egzamin ósmoklasisty"),
  rok,
  sufiks = "",
  czyRasch = FALSE,
  dopisz = FALSE,
  BK = FALSE
)
}
\arguments{
\item{P}{połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}}

\item{rodzajEgzaminu}{ciąg znaków}

\item{rok}{liczba całkowita}

\item{sufiks}{ciąg znaków - sufiks dodawany do opisów skal (nie jest dodawany
do opisu tworzonych testów, ani nazw skal raschowych)}

\item{czyRasch}{wartość logiczna - czy tworzyć również skale raschowe do
Kalkulatora EWD?}

\item{dopisz}{wartość logiczna - czy jeśli istnieją już w bazie jakieś skale
spośród tych, które ma utworzyć funkcja, to pominąć je i utworzyć pozostałe
(zamiast nie zapisać nic i zwrócić błąd)?}

\item{BK}{wartość logiczna - czy tworzyć oddzelne skale dla poszczególnych
przedmiotów matury, zgodnie ze zmodyfikowanym podejściem do obliczania
maryralnej EWD, zaproponowanym w 2022 r. przez Bartosza Kondratka?
(może być \code{TRUE} tylko gdy \code{rodzaj_egzaminu="matura"})}
}
\value{
wektor liczbowy zawierający id_skali utworzonych skal
}
\description{
Dla podanego rodzaju egzaminu i roku funkcja tworzy odpowiednie skale,
potrzebne do obliczania wskaźników EWD, przypisujac do nich wszystkie
kryteria odpowiednich części egzaminu. Tworzy też (wykorzystując funkcję
\code{\link{stworz_test_z_wielu_czesci}}) testy odpowiadające skalom,
które obejmują kilka części egzaminu.

Uwaga, skale tworzone są z flagą \code{do_prezentacji} ustawioną na
\code{FALSE}.
}
\details{
Uwaga, w przypadku skal wykorzystywanych do obliczania latentnych
wskaźników EWD (tj. przy wywołaniu z argumentem \code{BK=TRUE})
\strong{wywołanie funkcji z argumentem \code{rodzajEgzaminu="matura"} tworzy
również skale dla egzaminu na wejściu}.
}
