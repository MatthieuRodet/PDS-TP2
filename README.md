# TP2 PDS - OCaml

## Manon Sourisseau & Matthieu Rodet

Nous présentons dans ce dépôt notre compilateur VSL vers LLVM écrit en OCaml.
Les dépôts initiaux sont ici :

La [version OCaml](https://gitlab.istic.univ-rennes1.fr/cferry/PDS-TP2-ocaml)
et la
[version Java](https://gitlab.istic.univ-rennes1.fr/cferry/PDS-TP2-java.git)

## Travail accompli

Notre compilateur traite actuellement :

* Les expressions simples (entiers, additions, soustractions, multiplications, divisions, parenthèses) et la priorité des opérateurs
* Les instructions d'affectations entières
* La gestion des blocs
* Les déclarations de variables (entiers et tableaux d'entiers)
* Les expressions avec variables
* Les instructions de contrôle (While et If)
* Les fonctions de la bibliothèque (READ et PRINT)
* Les définitions et appels de fonction (sans le passage de tableaux en paramètre)
* La gestions des tableaux (expression, affectation et lecture)
* Un Pretty-Printer

Il nous reste à étendre et améliorer notre compilateur sur les points suivants :

* Le passage de tableaux en paramètre

Nous allons également nous pencher sur les extensions suivantes :

* Multi-threading d'un processus
* Application du Map and Reduce (<https://fr.wikipedia.org/wiki/MapReduce>)

## Construction d'un programme

Comme le formalise notre ASD, un programme se compose pour le moment d'un ensemble de blocs, qui chacun sont formés d'un ensemble de déclarations et d'un ensemble d'instructions. Les instructions composent l'ensemble des instructions VSL demandées à l'exception de l'affectation des tableaux. Les expressions sont composées de l'ensemble des opérateurs demandés, des variables entières, mais à l'exception des accès tableaux et de la priorité des opérateurs.

En définitif, un programme de notre version actuelle de VSL est une liste de blocs VSL classiques.

## Détails d'implémentation

Notre version actuelle gère la localité des variables. Ainsi une variable ne sera définie qu'au sein du bloc contenant sa déclaration. Pour cela, on associe à chaque variable lors de sa déclaration un identificateur unique (issu de la concaténation du nom de la variable à un entier incrémenté à chaque nouvelle définition), que l'on utilise dans le code compilé pour définir la variable et éviter les multiples variables du même nom. On remarque alors qu'il est possible de définir plusieurs variables du même nom dans des localités différentes, la variable la plus locale est alors affectée, modifiée et/ou lue.

## Makefile et tests

Nous avons retiré le warning d'override de module à la compilation, un fichier est donc bien compilé lors de l'absence de message d'erreur après exécution de `compile` sur le fichier.

Nous avons composé un ensemble complet de tests unitaires dans le dossier `test/perso/`, il est donc possible de tester une fonctionnalité précise à l'aide de la commande `./compile test/perso/[nom_du_test]`.

Il est également possible de lancer l'ensemble des tests unitaires du fichier `test/perso/` à l'aide de la commande `make tests_unit`.

De même, `make tests_level1`, `make tests_level2`, `make tests_level3`, `make tests_level4` et  `make tests_error` lancent respectivement les tests dans les dossiers `tests/testlevel1/`, `tests/testlevel2/`, `tests/testlevel3/`, `tests/testlevel4/`, `tests/testlevelerror/`.

Enfin, `make tests` lance l'ensemble des tests précédemment décrits.
