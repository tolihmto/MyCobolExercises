# COBOL_PROGRAM — Résumé et guide

Ce dépôt contient une collection d'exemples COBOL (et un exemple ODBC avec un shim C) pour apprendre et démontrer plusieurs fonctionnalités : I/O console et fichiers, tableaux, tris/recherches, manipulation de couleurs ANSI, art ASCII, calculs (dont Fibonacci), et accès SQL via ODBC.

## Prérequis

- GnuCOBOL (cobc)
- Optionnel (pour l'exemple ODBC/SQLite) :
  - Pilote ODBC pour SQLite
  - Gestionnaire ODBC (unixODBC sous Linux/macOS, ODBC natif sous Windows)
  - Les utilitaires/headers ODBC disponibles à l’édition de liens

Vérifier l’installation de GnuCOBOL:

- Linux/macOS: `cobc -v`
- Windows (MSYS2/Chocolatey ou binaire): `cobc -v`

## Compilation rapide (GnuCOBOL)

- Programme simple:
  - `cobc -x -Wall fibonacci.cbl -o fibonacci`
- Format libre/fixe: la plupart des fichiers sont compatibles avec la commande ci-dessus. En cas de format libre explicite, ajoutez `-free`.
- Exécution:
  - `./fibonacci` (Linux/macOS)
  - `fibonacci.exe` (Windows)

## Aperçu des programmes (par thèmes)

- Débuts / I/O console
  - `00_hello_world.cbl`, `01_hello_world.cbl`, `07_affiche_msg.cbl`, `03_question.cbl`, `05_prenom_age.cbl`, `06_prenom_age.cbl`, `08_bonjour_count.cbl`
- Chaînes / casse
  - `04_minmaj.cbl` (minuscule/majuscule et manipulations de chaîne)
- Tableaux et calculs
  - `09_tableau_entier.cbl` (saisie/affichage d'entiers)
  - `10_tableau_moyenne.cbl` (moyenne)
  - `11_tableau_maxmin.cbl`, `12_tableau_maxminmoy.cbl` (max/min/moyenne)
  - `13_tableau_trie.cbl` (tri, ex. bulle)
  - `14_tableau_recherche.cbl` (recherche)
  - `15_tableau_doublons.cbl` (doublons)
  - `16_tableau_occurs_histo.cbl` (histogramme avec OCCURS)
  - `23_som_moy.cbl` (somme et moyenne — variantes)
  - `24_som_moy_extra.cbl` (somme/moyenne, contrôles supplémentaires)
- Fichiers
  - `17_lecture_fichier.cbl` (lecture ligne à ligne, comptage de mots; usage: `./lecture_fichier_17 <chemin-fichier>`)
  - `25_inventaire.cbl` (inventaire rechargeable: saisie/tri/affichage, totaux & TVA, sauvegarde/chargement)
- Couleurs ANSI / TUI
  - `18_output_colors.cbl` (exemples de styles/couleurs)
  - `19_ansi_colors.cbl` (catalogue structuré des couleurs/styles)
  - `20_ansi_colors_table.cbl` (table de couleurs — si présent)
- ASCII Art
  - `21_firetree.cbl` (sapin en ASCII; usage: `./firetree <taille>`)
  - `22_firetree_color.cbl` (sapin coloré; usage: `./firetree <taille>`)
- Calculs / Fibonacci
  - `fibonacci.cbl` (lit des valeurs dans `numbers.txt`, affiche la séquence jusqu’à n)
  - `big_fibonacci.cbl` (addition grand entier “digit par digit”, zéro‑padding; lit `numbers.txt`)
  - `call_fibonacci/call_fibonacci.cbl` (démonstration de `CALL 'FIBONACCI'`)
- SQL / ODBC / SQLite
  - `odbc_sqlite_test/odbc_sqlite_demo.cbl` (connexion ODBC, création table, INSERT/SELECT)
  - `odbc_sqlite_test/odbc_sqlexecdirect.c` (shim C pour SQLExecDirect/ODBC)
  - `odbc_sqlite_test/Makefile` et `README.md` spécifiques au sous-dossier
- Divers
  - `02_album_example.cbl`, `sql_example.cbl` (illustrations supplémentaires)

## Détails et exécutions notables

- `17_lecture_fichier.cbl`

  - Lit un fichier texte en séquentiel, affiche les lignes, nettoie CR (0x0D), compte les mots par ligne et total.
  - Usage: `./lecture_fichier_17 <chemin-fichier>`

- `18_output_colors.cbl` / `19_ansi_colors.cbl`

  - Démontrent les séquences ANSI: styles (gras, souligné, etc.) et couleurs avant/arrière-plan.
  - Affichage coloré dans un terminal compatible ANSI (Windows 10+ ok dans Windows Terminal/PowerShell moderne).

- `21_firetree.cbl`

  - Calcule largeur/segments et affiche un sapin ASCII centré. Paramètre `taille >= 1`.
  - Exemple: `cobc -x 21_firetree.cbl -o firetree && ./firetree 5`

- `22_firetree_color.cbl`

  - Variante colorée avec feuille/décor aléatoires et étoile. Exemple: `./firetree 6`.

- `fibonacci.cbl`

  - Lit `numbers.txt` (séquentiel), puis pour chaque n affiche 0, 1, … jusqu’à n, avec accumulation simple.

- `big_fibonacci.cbl`

  - Implémente un “grand entier” en chaîne (jusqu’à 300 chiffres) avec addition manuelle et affichage zéro‑paddé.
  - Utile pour illustrer la gestion de grands nombres au-delà des types natifs.

- `call_fibonacci/call_fibonacci.cbl`
  - Montre l’appel d’un autre programme COBOL (`CALL 'FIBONACCI'`). Compiler/placer les binaires dans le même répertoire/exécutable selon l’outil.

- `25_inventaire.cbl`

  - Petit gestionnaire d’inventaire en console.
  - Fonctionnalités:
    - Saisie d’articles: `CODE`, `LIBELLE`, `PU` (prix unitaire), `QTE`.
    - Anti‑doublon sur `CODE` via le paragraphe `DUP-CODE`.
    - Option de chargement d’une sauvegarde détectée au démarrage.
    - Tri décroissant par montant (`PU*QTE`).
    - Affichage tabulaire puis totaux: `TOTAL QTE`, `TOTAL HT`, et `TVA %  TOTAL TTC`.
    - Sauvegarde automatique dans `inventaire_#####.txt` (nom aléatoire) au format texte.
  - TVA (par défaut 20%). Peut être passée en argument (ex: `./25_inventaire 10` pour 10%).
  - Utilisation:
    - Compilation: `cobc -x 25_inventaire.cbl`
    - Exécution: `./25_inventaire [tva]`
    - Lorsqu’une sauvegarde existe, le programme propose de la recharger, puis vous pouvez continuer la saisie.
  - Format de sauvegarde (`LINE SEQUENTIAL`, lisible et rechargeable):
    - Première ligne: `TVA=nn`
    - Lignes suivantes: `CODE|LIB|PU|QTE` où `PU` et `QTE` sont en format texte édité.
    - Exemple:
      ```
      TVA= 20
      A01|Banane|   2.33|      20
      G34|Velo  |1050.45|       1
      ```
  - Exemple d’affichage des totaux:
    ```
    CODE        LIBELLE                    PU          QTE       MONTANT
    A02         Cahier                     2.50         5        12.50
    A03         Classeur                   4.00         3        12.00
    A01         Stylo                      1.20        10        12.00
    --------------------------------------------------------------
    TOTAL QTE:                     18
    TOTAL HT :                 36.50
    TVA  20%  TOTAL TTC:       43.80
    ```

## SQL/ODBC (SQLite)

Les sources dans `odbc_sqlite_test/` montrent:

- Allocation de handles ODBC, connexion sur DSN `sqlite-test`, création/INSERT/SELECT, fetch et lecture de colonne.
- Un shim C (`odbc_sqlexecdirect.c`) pour faciliter l’appel `SQLExecDirect`/`SQLFetch` depuis COBOL.

Compilation (voir le `Makefile` du sous-dossier):

- Linux (exemple générique):
  - Installer `unixODBC` + pilote ODBC SQLite
  - `make` dans `odbc_sqlite_test/`
- Windows:
  - Utiliser l’ODBC natif (pilote SQLite ODBC pour Windows), adapter les chemins/librairies et éventuellement la commande cobc. Référez-vous au `README.md` du sous-dossier et à votre environnement (MSYS2/MinGW, Visual Studio Build Tools, etc.).

Note: Dans `odbc_sqlite_demo.cbl`, certains chemins (`/usr/lib/.../libodbc.so`) sont spécifiques Linux et doivent être adaptés sous Windows.

## Conseils d’utilisation

- Compiler avec `-Wall` pour les avertissements, et ajouter `-free` si vous convertissez un fichier en format libre.
- Pour les programmes lisant des fichiers (ex. `numbers.txt`), placez le fichier dans le répertoire d’exécution.
- Pour l’ANSI, utilisez un terminal compatible. Sous Windows, préférez Windows Terminal/PowerShell moderne.

## Arborescence (extrait)

- `00_hello_world.cbl`, `01_hello_world.cbl`, ...
- `13_tableau_trie.cbl`, `14_tableau_recherche.cbl`, `15_tableau_doublons.cbl`, `16_tableau_occurs_histo.cbl`
- `17_lecture_fichier.cbl`, `18_output_colors.cbl`, `19_ansi_colors.cbl`
- `21_firetree.cbl`, `22_firetree_color.cbl`
- `fibonacci.cbl`, `big_fibonacci.cbl`, `call_fibonacci/`
- `odbc_sqlite_test/` (demo ODBC/SQLite, shim C, Makefile, README propre au dossier)

## Licence

Usage éducatif/démonstration. Adapter selon vos besoins.
