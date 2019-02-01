

# Planificateur de tournée optimale 🚀

Ceci est le manuel d'utilisation du projet de recherche d'un planificateur de tournée optimale. Il est important de le lire afin d'exploiter au maximum les fonctionnalités proposées


<details>
 <summary><strong>Table des matières</strong> (cliquer pour en savoir plus)</summary>

* [Utilisation](#-utilisation)
* [Arborescence des fichiers](#-Arborescence des fichiers)
</details>


## 💾 Utilisation

### Système Linux

Le projet est construit de manière à compiler avec **une version d'OCaml >= à la version 4.02**. Il a été testé sur les machines de la salle 237 sans warning ni erreur.

Le projet comporte une partie sur les graphes complets et une autre sur les graphes non complets. Chaque partie possèdent deux règles dans le Makefile. Une qui crée un exécutable sur les fichiers ville.txt et param.txt et une autre qui effectue des tests unitaires.

 
Introduction : Les différents flags
----
Chaque exécutable peut être associé à un ou des flags, dans l'ordre souhaité, pour obtenir des résultats plus ou moins précis. Voici la liste des différents flags :

 * **-r** permet de lancer l'exécution de la recherche d'une tournée la plus optimale possible. *Sans ce flag, l'exécutable ne produit aucune sortie !*
 * **-v** permet d'afficher des informations de débogages sur la sortie standard 
 * **-g** permet d'afficher l'évolution de la construction du graphe de manière graphique. Chez moi, j'utilise des sleepf. Sur les machines de l'école, cette fonction est trop récente, je les ai remplacé par des sleep. Cependant sleep 1 ne semble pas attendre une seconde comme annoncé dans la doc. Par conséquent, l'affichage est un peu rapide ... *Je vous invite tout de même à tester la fonctionnalité !* 



Le test proposé se situe dans les fichiers ville.txt et param.txt. C'est un petit test sur 10 villes pour lesquelles un grand nombre de routes sont coupées. Je vous invite à le tester via les commandes 1 et 3 suivantes :

Partie 1 : Graphe complet
---
**1. Pour lancer le programme sur les fichiers ville.txt et param.txt se trouvant à la racine du projet :**

```bash
make clean && make complet && ./run -r
```

**2. Pour lancer les tests unitaires :**

```bash
make clean && make test_complet && ./run
```

Partie 2 : Graphe non complet
---
**3. Pour lancer le programme sur les fichiers ville.txt et param.txt se trouvant à la racine du projet :**

```bash
make clean && make non_complet && ./run -r
```

**4. Pour lancer les tests unitaires :**

```bash
make clean && make test_non_complet && ./run
```

Tests unitaires des fonctions du programme
---

**5. Pour lancer les tests unitaires des fonctions du projet :**

```bash
make clean && make test_unitaires && ./run
```

Nettoyage du dossier
---

**6. Pour nettoyer le dossier :**

```bash
make clean
```

## 💾 Arborescence des fichiers

À la racine du projet se trouvent tous les fichiers de code. Le rapport (rapport.pdf) et le Makefile se situent aussi à la racine. Dans les dossiers test_complet et test_non_complet se trouvent les fichiers de tests utilisés dans les tests unitaires de la partie 1 et 2. 