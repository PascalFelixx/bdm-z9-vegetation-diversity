# 🗂️ Metadatenbeschreibung – Z9 BDM Datensätze

## 2000_Z9_Kopfdaten.xlsx

Dieses File beinhaltet die durchgeführten Pflanzen- und Moos-Aufnahmen seit 2001.

**Wichtig:** Die Spalten `yearPl` und `yearMoos` beinhalten das Pflanzen- und Moosaufnahmejahr. Immer wenn es hier einen Eintrag gibt, dann ist die entsprechende Aufnahme gültig. Das ist wichtig, weil es Aufnahmen mit Null Arten gibt, aber auch einzelne Aufnahmen, die aus methodischen Gründen ungültig sind. Es kann auch sein, dass die Pflanzenaufnahme gültig, die Moosaufnahme aber ungültig war.

Die Moosproben werden von den Botanikern meistens während der 1. Begehung gesammelt. Deshalb gibt es kein eigenes Erhebungsdatum für die Moose und in der allermeisten Fällen ist das Aufnahmejahr der Pflanzen und Moose das gleiche. In Einzelfällen kann es aber Nachholer im Folgejahr geben, wenn eine Artengruppe nicht methodenkonform erhoben wurde.

---

## 2000_Z9_Pflanzen.xlsx

Enthält die Pflanzennachweise. Diese können mit der `aID_KD` mit den Kopfdaten verknüpft werden.

Die Deckung der Arten wird im BDM erst seit 2016 erfasst (2015 nur teilweise), davor gibt es nur Präsenz-/Absenz-Daten. Zur Umrechnung der Deckungs-Schätzklassen nach Braun-Blanquet verwenden wir die Tabelle im Anhang.

---

## 2000_Z9_Moose.xlsx

Enthält die Moosnachweise. Diese können mit der `aID_KD` mit den Kopfdaten verknüpft werden.
Nur Präsenz-/Absenz-Daten, keine Schätzung zur Häufigkeit der Moosarten.

---

## 📑 Kurzbeschreibung der Spalten in den Dateien

- `aID_STAO`: Eindeutige Nummer für einen Z9-Standort
- `aID_KD`: Eindeutige Nummer für eine Aufnahme (eine Aufnahme beinhaltet alle Begehungen eines Jahres). Dient als Verknüpfungsfeld
- `XKoord_LV95`: X-Koordinate gemäss neuem CH-Koordinatensystem
- `YKoord_LV95`: Y-Koordinate gemäss neuem CH-Koordinatensystem
- `HN`: Hauptnutzung (Siedlung, Wald, ...) der aufgenommenen Fläche
- `Delarze1`: Zuordnung der Fläche zur passenden Kategorie gemäss Delarze et al. 2015 während der Erstbegehung
- `Delarze2`: Zuordnung der Fläche zur passenden Kategorie gemäss Delarze et al. 2015 während der Zweitbegehung (soweit vorhanden)
- `yearPl`: Aufnahmejahr der Pflanzenaufnahmen
- `yearMoos`: Aufnahmejahr der Moosaufnahmen
- `Date_Pl_1`: Aufnahmedatum der ersten Begehung (Pflanzen und Moose)
- `Date_Pl_2`: Aufnahmedatum der zweiten Erhebung (nur Pflanzen, alpine Flächen werden nur einmal begangen)
- `BGR_6`: Die biogeographischen Regionen (6 Grundregionen), Stand 2020
- `Hoehe`: Höhe über Meer (m) gemäss DHM25_LV03
- `Kanton`: Kanton gemäss swissBOUNDARIES3D
- `aID_SP`: Eindeutige ArtID. Dient als Verknüpfungsfeld
- `InfoSpeciesNr`: Artnummer von InfoSpecies, soweit bekannt. Bei den Pflanzen handelt es sich um die Identifikationsnummer gemäss Synonymie-Index (Index synonymique de la flore suisse et territoires limitrophes, ISFS)
- `Gattung`: Gattungsname
- `Art`: Artname
- `ArtD`: Deutschsprachige Bezeichnung für die Art (soweit vorhanden)
- `Pr1`: Wurde die Art während der ersten Begehung nachgewiesen?
- `Pr2`: Wurde die Art während der zweiten Begehung nachgewiesen?
- `Deckung1`: Deckungsschätzung der nachgewiesenen Pflanzen-Arten bei der Erstbegehung nach Braun-Blanquet
- `Deckung2`: Deckungsschätzung der nachgewiesenen Pflanzen-Arten bei der Zweitbegehung nach Braun-Blanquet
