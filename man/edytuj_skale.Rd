% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edytuj_skale.R
\name{edytuj_skale}
\alias{edytuj_skale}
\title{Zapisuje elementy skali do bazy danych}
\usage{
edytuj_skale(P, idSkali, elementy, nadpisz = FALSE)
}
\arguments{
\item{P}{połączenie z bazą danych uzyskane z \code{DBI::dbConnect(RPostgres::Postgres())}}

\item{idSkali}{identyfikator skali, ktora ma zostac zapisana (typowo uzyskany z funkcji "stworz_skale()")}

\item{elementy}{ramka danych opisujaca elementy skali - patrz http://zpd.ibe.edu.pl/doku.php?id=r_zpd_skale}

\item{nadpisz}{czy nadpisac skale, jesli jest juz zdefiniowana w bazie danych}
}
\value{
[data.frame] zapisane elementy skali
}
\description{
Patrz http://zpd.ibe.edu.pl/doku.php?id=r_zpd_skale
}
