# Introduction 
Comment Vetting is an API coded in R that characterizes content that is likely unsuitable for publication in regulatory comments.

# Getting Started
Comment Vetting is an R application that uses a wide variety of dependencies. As a starting point, you'll need:
1.	RStudio https://www.rstudio.com/
2.	Python 3 https://www.python.org/downloads/
3.	Pip https://pypi.org/project/pip/
4.	Docker https://docs.docker.com/engine/
5.	Docker Compose https://docs.docker.com/compose/

Once these are installed, you'll need to manually install each of the dependencies. Please see the Dockerfile for the current list of requirements.

## Build and Test
To build and test Comment Vetting, ensure that the current user can execute docker, change to the project directory, and type:
```
docker build . --no-cache --file Dockerfile --tag commentsvetting:$(date +%s)
``` 
## Running
If you want to launch the API as a service, use:
```
docker-compose up
``` 

The default port is `8000`; you can test that it is running properly with:

```
curl http://localhost:8000
```
The service also exposes a Swagger UI at `http://localhost:8000/__docs__/`

# Contribute
As always, the code needs more tests, documentation, and user feedback. Feel free to contribute!

-------------------
# Introduction 
« Vérification des commentaires » est une API codée en R qui caractérise le contenu qui est probablement impropre à publier parmi les commentaires réglementaires.

# Démarrage
« Vérification des commentaires » est une application R qui utilise [...] Vous aurez besoin de :
1.	RStudio https://www.rstudio.com/
2.	Python 3 https://www.python.org/downloads/
3.	Pip https://pypi.org/project/pip/
4.	Docker https://docs.docker.com/engine/
5.	Docker Compose https://docs.docker.com/compose/

## Construire et tester
Pour construire et tester « vérification des commentaires », assurez-vous que R est installé et sur votre chemin, et tapez :
```
docker build . --no-cache --file Dockerfile --tag commentsvetting:$(date +%s)
``` 
## Exécution
Pour lancer l'API en tant que service, utilisez :
```
docker-compose up
``` 

Le port par défaut est `[Howard to fill this in.]` ; vous pouvez tester qu'il fonctionne correctement avec :

```
curl http://localhost:8000
```
Le service expose également une interface Swagger à `http://localhost:8000/__docs__/`

# Contribuer
Comme toujours, le code a besoin de plus de tests, de documentation et de commentaires des utilisateurs. N'hésitez pas à contribuer !
