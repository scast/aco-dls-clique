aco-dls-clique
===========

Implementaciones en C++ de los siguientes algoritmos:

- Dynamic Local Search según Pullan & Hoos [2006].
- Ant-Clique con Vertex AC según Solnon & Fenet [2006].

Implementación en C++ de la versión inicial de los siguientes algoritmos:

- Collaborative DLS
- Dynamic Ant-Clique

Implementaciones en Haskell de los siguientes algoritmos:

- Dynamic Local Search según Pullan & Hoos [2006]

Fuentes del trabajo entregado como proyecto final de la materia Diseño
de Algoritmos 2 en la Universidad Simón Bolívar por Simón Castillo y
Alejandro Flores. Julio de 2013.

Papers de referencia por Pullan & Hoos y Solnon & Fenet.

Dependencias
============

Los algoritmos implementados se conoce que compilan y corren sobre
Ubuntu 12.04. Es necesario tener los headers de -dev
instalados. En Ubuntu, debería ser suficiente:

`sudo apt-get install build-essential`

Adicionalmente, dependiendo de la plataforma que utilice posiblemente
tenga que instalar:

Para correr el codigo en C++
----------------------------

`sudo apt-get install libboost-all-dev`

Para correr el codigo en Haskell
--------------------------------

`sudo apt-get install haskell-platform`

Para compilar las fuentes del trabajo final
-------------------------------------------

`sudo apt-get install texlive`

Compilación & Ejecución
=======================

En la carpeta `src/` se encuentran las fuentes del proyecto. Tanto el
codigo en C++ como en Haskell cuentan con un Makefile. Para compilar
el codigo en C++:

`make`

Esto generará los binarios `local-go` y `aco-go` para Collaborative
DLS y Dynamic Ant-Clique respectivamente. Para la ejecución se
incluyen en la carpeta instances/ un comprimido con todas las
instancias de prueba. Adicionalmente, se incluyen dos scripts
`run-local.sh` y `run-aco.sh` que fueron los utilizados para generar
los resultados de las corridas (estos ya tienen los parámetros para
cada instancia).

Correr `run-aco.sh` y `run-local.sh` correrá 3 ejecuciones
independientes de toda las instancias (esto toma bastante
tiempo!). Los resultados de las corridas se guardan en archivos en la
carpeta actual con el nombre [instancia].result o
[instancia].acoresult, para DLS y ACO respectivamente.

Correr instancias para DLS

`./run-dls.sh`

Correr instancias para ACO:

`./run-aco.sh`
