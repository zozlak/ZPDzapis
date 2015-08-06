# library(testthat)
# library(RODBCext)
# library(dplyr)
# library(ZPDzapis)
# rm(list=ls())

context('zapisz_skalowanie()')
zrodloDanychODBC = 'EWD_grzes'

# wczytaj i przygotuj przykładowe skalowanie, na którym przeprowadzimy testy
# load('/media/grzesiek/Dell/EWDgithub/ZPDzapis_2/tests/testthat/dane/spr2011EWDskalowanie.RData')
#
# opisSkalowania = list(
#   skalowanie     = 100,
#   opis           = 'skalowanie testowe',
#   estymacja      = 'MML (Mplus)',
#   do_prezentacji = FALSE,
#   data           = '2015-05-01',
#   egzamin        = "sprawdzian",
#   rok            =  2011,
#   rodzajEstymacji = 'EAP'
# )
#
# skalowanie = zmien_obiekty_skalowania(skalowanie, opisSkalowania$rok, opisSkalowania$egzamin, zrodloDanychODBC, 1:10)
# # skalowanie$id_skali = stworz_skale('skala_testowa65_spr_2011', 'ewd', FALSE, unique(skalowanie$oszacowania$id_testu_s), zrodloDanychODBC)
#
# # Test 1 - czy zapisywane są informacje do tabeli "skalowania"
# test_that('zapisz_skalowanie() tworzy skalowanie', {
#   zapisz_skalowanie(skalowanie, opisSkalowania, TRUE, zrodloDanychODBC)
#
#   P = odbcConnect(zrodloDanychODBC)
#   wynik = sqlExecute(P, "SELECT * FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
#                      list(skalowanie$id_skali, opisSkalowania$skalowanie), TRUE)
#   sqlExecute(P, "DELETE FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   odbcClose(P)
#
#   wejscie = as.data.frame(list(opisSkalowania, id_skali = skalowanie$id_skali))
#   expect_equal(wynik %>% select(skalowanie, opis, estymacja, id_skali), wejscie %>% select(skalowanie, opis, estymacja, id_skali))
#   expect_equal(as.character(wynik$data), as.character(wejscie$data))
#   expect_equal(wynik$do_prezentacji, as.numeric(wejscie$do_prezentacji))
# })
#
# # Test 2 - czy skala dostosowywana jest zgodnie z wykazem usuniętych kryteriów oceny
# # TODO - dopisać test dla pseudokryterium
# test_that('zapisz_skalowanie() dostosowuje skalę', {
#   P = odbcConnect(zrodloDanychODBC)
#   sqlExecute(P, "INSERT INTO skale_elementy VALUES (-1, ?, 1424, null, null)", skalowanie$id_skali)
#
#   zapisz_skalowanie(skalowanie, opisSkalowania, TRUE,  zrodloDanychODBC)
#   elementy = sqlExecute(P, "SELECT * FROM skale_elementy WHERE id_skali = ?", skalowanie$id_skali, TRUE)
#
#   sqlExecute(P, "DELETE FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   odbcClose(P)
#
#   expect_equal(elementy, data.frame(
#     kolejnosc          = 1:30,
#     id_skali           = rep(skalowanie$id_skali, 30),
#     id_kryterium       = c(1415:1423, 1425:1445),
#     id_pseudokryterium = rep(NA_integer_, 30),
#     id_skrotu          = rep(NA, 30)
#   ))
# })
#
# # Test 3 - czy parametry zapisywane są do tablicy skalowania_elementy?
# test_that('zapisz_skalowanie() zapisuje parametry', {
#   zapisz_skalowanie(skalowanie, opisSkalowania, TRUE,  zrodloDanychODBC)
#
#   P = odbcConnect(zrodloDanychODBC)
#   parametry = sqlExecute(P, "SELECT * FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ? ORDER BY parametr, kolejnosc", list(skalowanie$id_skali, opisSkalowania$skalowanie), TRUE, stringsAsFactors = FALSE)
#   parametry$id_elementu = rep(NA, nrow(parametry))
#   parametry$wartosc = round(parametry$wartosc, 3)
#   parametry$bs = round(parametry$bs, 3)
#
#   sqlExecute(P, "DELETE FROM skalowania_elementy WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania_obserwacje WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania_grupy WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   sqlExecute(P, "DELETE FROM skalowania WHERE id_skali = ? AND skalowanie = ?",
#              list(skalowanie$id_skali, opisSkalowania$skalowanie))
#   odbcClose(P)
#
#   expect_equal(parametry, data.frame(
#     id_skali    = rep(skalowanie$id_skali, 76),
#     kolejnosc   = c(1:30, 20:24, 29, 20:24, 29, 23:24, 23, NA, 1:30),
#     skalowanie  = rep(opisSkalowania$skalowanie, 76),
#     parametr    = c(rep('a', 30), rep('b1', 6), rep('b2', 6), rep('b3', 2), 'b4', 'r EAP', rep('trudność', 30)),
#     model       = c(rep('2PL', 19), rep('GRM', 5), rep('2PL', 4), 'GRM', '2PL', rep('GRM', 15), 'n.d.', rep('2PL', 19), rep('GRM', 5), rep('2PL', 4), 'GRM', '2PL'),
#     wartosc     = round(c(1.349, 1.619, 1.084, 1.066, 1.774, 1.435, 1.02, 1.262, 1.411, 0.313, 1.046, 1.57, 0.893, 1.225, 0.945, 1.196, 1.245, 0.702, 1.287, 1.52, 1.75, 2.045, 2.011, 0.648, 0.579, 0.811, 0.954, 0.96, 0.499, 0.685, -0.129276, -0.458857, -0.150856, -0.560293, -3.19033, -1.46493, 0.129276, 0.458857, 0.150856, -0.0406514, 0.371399, 1.46493, 0.132894, 2.81893, 0.468051, 0.845918, -2.23499, -2.24151, -0.390221, -2.79362, -2.22041, -2.89129, -0.0509804, -0.376387, -2.19561, 3.1246, -2.05641, -2.19172, -0.0291153, -1.74857, -0.12381, -1.10452, -0.279518, -0.319088, -1.07692, -0.138487, -0.621714, -1.1489, 0.0620338, -0.624486, 1.12953, -1.0148, -1.37736, -1.69896, 1.73948, -1.49051), 3),
#     uwagi       = rep(NA, 76),
#     bs          = round(c(0.01, 0.012, 0.006, 0.009, 0.014, 0.014, 0.005, 0.006, 0.01, 0.005, 0.007, 0.011, 0.005, 0.007, 0.005, 0.006, 0.006, 0.004, 0.006, 0.007, 0.007, 0.01, 0.008, 0.004, 0.005, 0.005, 0.006, 0.007, 0.004, 0.005, 0.00328947, 0.004, 0.00488998, 0.00298359, 0.00925926, 0.00801603, 0.00328947, 0.00285714, 0.00440098, 0.00298359, 0.00617284, 0.01002, 0.00298359, 0.00771605, 0.00298359, NA, 0.00815419, 0.00864731, 0.00369004, 0.00938086, 0.00958286, 0.0125436, 0.00392157, 0.00396196, 0.00779589, 0.0127796, 0.00669216, 0.00828025, 0.00447928, 0.00571429, 0.0042328, 0.0041806, 0.00401606, 0.00569801, 0.004662, NA, NA, NA, NA, NA, 0.00690846, 0.00493218, 0.00524109, 0.00625, NA, 0.00583942), 3),
#     id_elementu = rep(NA, 76),
#     grupowy = rep(NA, 76),
#     grupa= rep(NA, 76),
#     stringsAsFactors = FALSE
#   ))
# })
#
# test_that('zapisz_skalowanie() linkuje wskaźniki', {
#
# })
#
# test_that('zapisz_skalowanie() tworzy pesudotest', {
#
# })
#
# test_that('zapisz_skalowanie() zapisuje oszacowania uczniów', {
#
# })

# 1. stworzenie (uaktualnienie) skalowania,
# 2. ew. edycję skali (usunięcie (pseudo)kryteriów),
# 3. zapis parametrów modelu (parametrów zadań - czyli (pseudo)kryteriów i "pseudozadań" opisujących wybór przedmiotów/tematów - i parametrów grup),
# 4. zapis powiązań pomiędzy skalowaniem a wskaźnikami EWD,
# 5. ew. stworzenie "pseudotestu" (dla skal integrujących kilka części egzaminu) - jest już na to funkcja stworz_test_z_wielu_czesci(),
# 6. zapis oszacowań uczniów.
