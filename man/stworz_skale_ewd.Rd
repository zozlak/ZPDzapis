% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/stworz_skale_ewd.R
\name{stworz_skale_ewd}
\alias{stworz_skale_ewd}
\title{Tworzy w bazie skale zwiazane z wyliczaniem wskaznikow EWD}
\usage{
stworz_skale_ewd(rodzajEgzaminu, rok, sufiks = "", czyRasch = TRUE,
  dopisz = FALSE, zrodloDanychODBC = "EWD")
}
\arguments{
\item{rodzajEgzaminu}{ciąg znaków}

\item{rok}{liczba całkowita}

\item{sufiks}{ciąg znaków - sufiks dodawany do opisów skal (nie jest dodawany
do opisu tworzonych testów)}

\item{czyRasch}{wartość logiczna - czy tworzyć również skale raschowe do
Kalkulatora EWD?}

\item{dopisz}{wartość logiczna - czy jeśli istnieją już w bazie jakieś skale
spośród tych, które ma utworzyć funkcja, to pominąć je i utworzyć pozostałe
(zamiast nie zapisać nic i zwrócić błąd)?}

\item{zrodloDanychODBC}{nazwa żródła danych ODBC, którego należy użyc}
}
\value{
wektor liczbowy zawierający id_skali utworzonych skal
}
\description{
Dla podanego rodzaju egzaminu i roku funkcja tworzy odpowiednie skale,
potrzebne do wyliczania wskaźników EWD, przypisujac do nich wszystkie
kryteria odpowiednich części egzaminu. Tworzy też (wykorzystując funkcję
\code{\link{stworz_test_z_wielu_czesci}}) testy odpowiadające skalom,
które obejmują kilka części egzaminu.

Uwaga, skale tworzone są z flagą \code{do_prezentacji} ustawioną na
\code{FALSE}.
}
