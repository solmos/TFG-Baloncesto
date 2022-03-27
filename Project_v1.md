Métodos estadísticos aplicados al baloncesto
================
Paula Moreno
21/1/2022

# Abstract

Hoy en día, el deporte es un hobby muy popular por todo el mundo. Des de
pequeños, la gran mayoría de niños practican algún tipo de deporte,
especialmente aquellos que son de equipo. Eso nos lleva a querer saber
más del deporte, más detalles, más información. Nos entra la curiosidad
de “¿quién es el mejor jugador?”, “Qué equipo es mejor?”, o incluso
intentar prevenir qué equipo ganará según sus resultados anteriores. Y
gracias a los avances tecnológicos, cada vez se nos facilita más poder
seguir un deporte des de casa, ver la estadística de los deportistas e
incluso hay plataformas o juegos que nos permiten ser, de manera
virtual, managers de los clubs y, por lo tanto, nos facilitan mucha
información que antes era más difícil de saber.

Eso hace que, de manera progresiva, también mejore el estudio y el
análisis de cada deporte, y cada vez sea más específica para cada
deporte, implementando nuevos recursos para mejorar los resultados.

En este trabajo estudiaremos más a fondo el Baloncesto, el segundo
deporte más popular de Europa (solo superado por el futbol), y el cual
yo tengo relación personal, ya que lo practico des de los 4 años. En
especial nos centraremos en el Baloncesto profesional Europeo, de donde
podemos obtener más datos.

Este trabajo surgió del constante pensamiento de que los análisis
actuales que se hacen en este deporte en Europa son bastante pobres a
nivel informativo, puesto que se basan en conceptos muy básicos. Para
que nos hagamos una idea, el estadístico por preferencia es el llamado
“Valoración” y que se originó en 1991 (hace 30 años) y des de entonces
nunca se ha modificado.

Es por eso que, considero que actualmente los análisis que se hacen de
este deporte necesitan una actualización para llegar a informar de todos
aquellos datos que hoy en día si se pueden recoger gracias a los avances
tecnológicos.

# Contenido

0.  Recursos informaticos nuevos que se han utilizado para hacer este
    trebajo: GitHub. Explicacion de qué es, como funciona y ventajas que
    tiene.

1.  Introducción al baloncesto y breve explicación de cómo se juega
    (historia, conceptos importantes de conocer, y metodologia del
    juego)

2.  Explicación de la Estadística Actual (formulas, definiciones)

3.  Nuevos Analisis (breve explicacion de la nueva forma de recoger
    datos de los jugadores, y luego el mas/menos ajustado, Gini…)

4.  Analisis de los datos Temporada 2018-2019

# Recursos Informaticos

Para realizar este trabajo, mi tutor Sergio Olmos, me recomendó utilizar
GitHub porque era una manera de poder compartir mi proyecto de Markdown
con mis tutores de manera constante. Requería trabajar con el proyecto
publicado en dicha plataforma y de manera automática, cuando yo
modificara mi archivo, ellos podrían ver este cambio al momento. Yo
desconocía totalmente de este espacio, por lo que una parte de mi
trabajo era aprender a trabajar en GitHub.

### ¿Qué es GitHub?

GitHub es una plataforma de alojamiento, propiedad de Microsoft, que
ofrece a los desarrolladores la posibilidad de crear repositorios de
código y guardarlos en la nube de forma segura, usando un sistema de
control de versiones, llamado Git.

Como he comentado, facilita la organización de proyectos y permite la
colaboración de varios desarrolladores en tiempo real. Es decir, nos
permite centralizar el contenido del repositorio para poder colaborar
con los otros miembros de nuestro grupo des de varios dispositivos.

GitHub está basada en el sistema de control de versiones distribuida de
Git, por lo que se puede contar con sus funciones y herramientas, aunque
GitHub ofrece varias opciones adicionales y su interfaz es mucho más
fácil de manejar, por lo que no es absolutamente necesario que las
personas que lo utilizan tengan un gran conocimiento técnico.

### Ventajas

Hay un gran número de razones por las que GitHub es una gran opción para
el control y gestión de proyectos de código. Como por ejemplo:

  - GitHub permite que alojemos proyectos en repositorios de forma
    gratuita
  - Los repositorios son públicos por defecto. Sin embargo, GitHub te
    permite también alojar tus proyectos de manera privada
  - Puedes crear y compartir páginas web estáticas con GitHub Pages
  - Facilita compartir tus proyectos de una forma mucho más fácil y
    crear un portafolio
  - Te permite colaborar para mejorar los proyectos de otros y a otros
    mejorar o aportar a los tuyos
  - Ayuda reducir significativamente los errores humanos y escribir tu
    código más rápido con GitHub Copilot
  - Te da control de versiones, una herramienta muy útil.

### ¿Qué es el control de versiones?

Se le llama control de versiones a la administración de los cambios que
se realizan sobre los elementos o la configuración de algún proyecto. En
otras palabras, el control de versiones sirve para conocer y autorizar
los cambios que realicen los colaboradores en tu proyecto, guardando
información extra de qué están incluyendo los cambios y cuándo se
hicieron. Este control comienza con una versión básica del documento y
luego va guardando los cambios que se hagan a lo largo del proyecto.

El control de versiones es una herramienta muy valiosa, pues con ella
puedes tener acceso a las versiones anteriores de tu proyecto si es que
en algún momento no llega a funcionar de forma correcta.

### ¿Qué es Git?

Git es un software de control de versiones diseñado por Linus Torvalds,
pensando en la eficiencia, la confiabilidad y compatibilidad del
mantenimiento de versiones de aplicaciones cuando estas tienen un gran
número de archivos de código fuente.

#### Diferencias Git vs GitHub

Entonces, ¿qué diferencia Git de GitHub?. La principal diferencia es que
Git es un sistema que permite establecer un control de versiones,
mientras que GitHub es una plataforma que ofrece un grupo de funciones
que facilitan el uso de Git y la colaboración en tiempo real, así como
el almacenamiento en la nube.

# El baloncesto

## Historia y reglas básicas

El baloncesto es un deporte de equipo que se originó en 1891, por James
Naismith, profesor de educación física en la escuela, que buscaba idear
un deporte que sus alumnos pudieran practicar bajo techo, pues los duros
inviernos en Nueva Inglaterra dificultaban la realización de ejercicio
al aire libre. Con el paso de los años, este deporte que empezó como
actividad de colegio, ha ido evolucionando mucho, añadiendo más reglas,
conceptos nuevos, límites de números de jugadores, se ha determinado
tiempos de juego, las canastas tienen un valor distinto según la
distancia, etc.

Actualmente, las normas más básicas de este deporte son:

  - En las ligas superiores, hay un total de 4 cuartos de 10 minutos y
    pueden estar en pista 5 jugadores por equipo.
  - No te puedes desplazar con la pelota en las manos, es obligatorio
    botar con una mano (si no será una infracción y conllevará la
    perdida de pelota y saque de banda del equipo rival).
  - Cada jugador puede realizar hasta un total de 5 faltas, que será
    penalizado con un saque de banda o con un tiro libre (dependerá de
    la situación). El jugador que realiza 5 faltas será expulsado del
    partido.
  - El objetivo es encestar el máximo de puntos posibles, teniendo en
    cuenta que pueden sumar 1, 2 o 3 puntos, según la distancia.

## Conceptos y definiciones básicas del baloncesto:

Para que podamos entender a que nos referimos en este trabajo, es
necesario comprender unos conceptos básicos de vocabulario. Tendremos en
cuenta los conceptos que se necesitan para realizar la valoración del
jugador y/o del equipo que se utilizan en las estadísticas federadas.

  - Puntos: Acumulación de canastas encestadas multiplicadas por su
    valor, que cada jugador y/o equipo realiza durante el partido
  - Minutos: Número de minutos que el jugador está en pista
  - Falta: Acción en la que un defensor bloquea el avance de su rival
    sin tener control de balón o de manera no reglamentaria (empujar,
    agarrar…)
  - Perdidas de balón: cuando un equipo pierde el control del balón y
    pasa a ser del equipo rival.
  - Rebotes: Recuperación de pelota después de que el tiro sea
    ejecutado, pero no haya encestado.
  - Recuperación de balón: Cuando un equipo consigue robar el balón al
    equipo rival.
  - Asistencia: Es un pase a un jugador que se encuentra en una posición
    de ventaja o que le ayuda a conseguir una canasta sin hacer ningún
    bote.
  - Tapón: Bloqueo de un tiro en el aire.

## Análisis

Viendo la gran cantidad de datos que se pueden extraer de cada partido
(y de cada equipo), se han ido creando análisis que recogen estos datos
y los analizan para ayudarnos a identificar y desarrollar hipótesis
sobre cada jugador y/o equipo.

## Boxscore

El primer análisis que se hizo fue un *Box Score* (Caja de puntuación)
donde se recopilaba únicamente los puntos de cada jugador según el valor
de esta y las faltas realizadas. Posteriormente, se fue mejorando
añadiendo conceptos como rebotes, tapones, perdidas de balón,
recuperaciones de balón… Y se añadió el estadístico (que acutalmente es
por defecto) que se realiza a partir de todos estos datos: “Valoración”
(en inglés PIR, *Performance Index Rating*) que engloba todo lo básico
que pasa en el partido de manera individual y que, cuanto más positivo,
mejor. Este estadístico se calcula utilizando la siguiente fórmula:

\[PIR = (Puntos + Rebotes + Asistencias + Robos + Tapones + Faltas Recibidas) - (Tiros de Campo Fallados + Tiros Libres Fallados + Tapones Recibidos + Pérdidas + Faltas Realizadas) \]

![Imagen 1. Boxscore del partido de la NBA de Philadelphia Warriors
contra New York Knicks, del 2 de Marzo de
1962](imagenes/BoxScore1962.jpg)

![Imagen 2. Boxscore del partido de la Euroliga de Real Madrid contra FC
Barcelona, del 11 de Febrero del
2022](imagenes/BoxScore2022_EuroligaBarca.JPG)

(en el [Anexo 1](#Anexo-1:-Descripción-de-las-variables) encontraréis la
descripción de cada variable)

Posteriormente, se añadió la variable “Más/Menos” (P/M, *Plus/Minus*)
que tiene que ver con la diferencia de puntos en el marcador durante el
tiempo que el jugador está en pista. Esta variable sirve para ver la
contribución de los jugadores cuando están en pista. Todos los jugadores
parten inicialmente con un 0, y según van entrando y saliendo de la
pista, esta variable se va actualizando. Por ejemplo, los jugadores que
son del quinteto inicial, empiezan con el marcador 0 - 0, y un P/M = 0.
Si en el minuto 5, se substituye un jugador en cancha del equipo local
(J1) por otro que está descansando (J2), y el marcador va 12 - 7, el P/M
del J1 pasará a ser +5. Y si al cabo de 3 minutos, se sustituye el J2
por otro (J3) y el marcador ahora va 20 - 9, el P/M del J2 será +6 ($
(20-12) - (9-7) = 8 - 2 = +6 $).

Aunque un *Box Score* es muy útil para realizar análisis básicos, ya que
es muy visual y cualquier persona sin la necesidad de muchos recursos
puede analizar y predecir ciertos valores, pero estadísticamente
perdemos una parte importante de la información de los datos, puesto que
no nos los muestra progresivamente, sino que nos da los valores
acumulados al final del tiempo establecido, y muchas veces contiene
información engañosa, especialmente en las estadísticas defensivas.

Por lo que, para el desarrollo temporal del partido y para conocer
cierta información de equipo que piden entrenadores y clubs, no nos
sirve (como por ejemplo la eficacia de los quintetos, el desarrollo del
marcador o de cualquier otra variable del equipo entero durante un
tiempo determinado del partido, etc.)

## play-by-play

Este tipo de recogida de información se creo para solucionar el problema
que teniamos con el *Box Score*. Los datos de *Play-by-Play* (PBP) han
sido la fuente principal de muchas estadísticas avanzadas, como el
más-menos ajustado, que se desarrollará en este trabajo.

Jugada por jugada proporciona una transcripción del juego en un formato
de eventos individuales. Los datos típicos de jugada por jugada deben
tener la siguiente información: + El tiempo de la posesión, + El jugador
que inició la posesión (en caso de robo o rebote defensivo, + El jugador
contrario que inició la posesión (en caso de un tiro fallado o pérdida
de balón), incluida la ubicación en el piso desde donde se realizó el
tiro y algunos otros identificadores únicos que usamos para clasificar
el tipo de posesión.

## shot-charts

-----

# Bibliografia

Sport in Europe (Wikipedia):
<https://en.wikipedia.org/wiki/Sport_in_Europe>

Valoración (Wikipedia):
<https://en.wikipedia.org/wiki/Performance_Index_Rating>

Git (Wikipedia): <https://es.wikipedia.org/wiki/Git>

GitHub (Wikipedia): <https://es.wikipedia.org/wiki/GitHub>

Baloncesto (Wikipedia): <https://es.wikipedia.org/wiki/Baloncesto>

Valoración (Wikipedia):
<https://es.wikipedia.org/wiki/Valoraci%C3%B3n_(baloncesto)>

# Anexo 1: Descripción de las variables

  - MIN (*Minutes*): Minutos totales jugados
  - PTS (*points*): Puntos totales realizados
  - 2FGA (*2-point Field Goals Attempted*): Número de canastas de 2
    puntos intentadas
  - 2FGM (*2-point Field Goals Made*): Número de canastas de 2 puntos
    anotadas
  - 3FGA (*3-point Field Goals Attempted*): Número de canastas de 3
    puntos (“triples”) intentadas
  - 3FGM (*3-point Field Goals Made*): Número de canastas de 3 puntos
    (“triples”) anotadas
  - FTA (*Free Throws Attempted*): Número de tiros libres intentados
  - FTM (*Free Throws Made*): Número de tiros libres anotados

-----

# Datos

``` r
library(readr)
pbp2018 <- read.csv(file="pbp2018.csv", head=TRUE, sep=",")
#View(pbp2018)
```

### Paquetes

``` r
library(dplyr)
library(ggplot2)
```

#### COPY AND PASTE - Areglarlo\!

### Alineaciones con jugadores repetidos

## Paquetes

``` r
library(here)
library(tidyverse)
```

## Problema

Algunos registros en `pbp_2018.csv` contenían alineaciones con un mismo
jugador que aperece dos veces.

Tras inspeccionar la función del paquete eurolig que obtiene las
alineaciones de estos datos, descubrí que se trataba de un error en la
función que asigna las alineaciones en las situaciones en las que hay
cambios de jugadores cuando hay tiros libres. Solo afectaba a las
variables `away_player4` y `away_player5`.

``` r
pbp_2018 <- read_csv("pbp2018.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

``` r
## Check how many rows are affected by this
bad_lineups <- pbp_2018 %>%
  select(matches("_player[1-5]")) %>%
  apply(1, function(x) max(table(x)) > 1)
pbp_bad <- pbp_2018 %>%
  filter(bad_lineups)
pbp_bad %>%
  select(season, game_code, play_number, play_type, away_player4, away_player5)
```

    ## # A tibble: 10,260 × 6
    ##    season game_code play_number play_type away_player4 away_player5
    ##     <dbl>     <dbl>       <dbl> <chr>     <chr>        <chr>       
    ##  1   2018         2          93 IN        TOMIC, ANTE  TOMIC, ANTE 
    ##  2   2018         2          94 OUT       TOMIC, ANTE  TOMIC, ANTE 
    ##  3   2018         2          95 OUT       TOMIC, ANTE  TOMIC, ANTE 
    ##  4   2018         2          96 IN        TOMIC, ANTE  TOMIC, ANTE 
    ##  5   2018         2          97 OUT       TOMIC, ANTE  TOMIC, ANTE 
    ##  6   2018         2          98 IN        TOMIC, ANTE  TOMIC, ANTE 
    ##  7   2018         2          99 OUT       TOMIC, ANTE  TOMIC, ANTE 
    ##  8   2018         2         100 FTM       TOMIC, ANTE  TOMIC, ANTE 
    ##  9   2018         2         101 FTM       TOMIC, ANTE  TOMIC, ANTE 
    ## 10   2018         2         116 AST       KURIC, KYLE  KURIC, KYLE 
    ## # … with 10,250 more rows

## Solución

He creado unas funciones en [`fix-lineups.R`](fix-lineups.R) para
corregir este error en los datos que ya tenemos.

``` r
source(here("R", "fix-lineups.R"))
## Function fix_lineups() only takes data from a single game,
## so I split the data and apply the function to each splitted data frame.
pbp_2018_fixed <- split(pbp_2018, factor(pbp_2018$game_code)) %>%
  map_df(fix_lineups)
## Check that this has been fixed
pbp_2018_fixed %>%
  select(matches("_player[1-5]")) %>%
  apply(1, function(x) max(table(x)) > 1) %>%
  sum()
```

    ## [1] 0

Finalmente escribo los datos corregidos en `data/pbp_2018_fixed.csv`.

``` r
#write_csv(pbp_2018_fixed, here("data", "pbp_2018_fixed.csv"))

#pbp2018_f <- read.csv(file="pbp_2018_fixed.csv", head=TRUE, sep=",")
#View(pbp2018_f)
```

-----

# MAS/MENOS

# Code siguiendo enlace sobre: MAS/MENOS AJUSTADO en R (season 2018)

``` r
# Lista de jugadores:
players_rep <- pbp_2018_fixed$player_name
players <- unique(players_rep)
```

EXPLICAR BIEN TODOS LOS DATOS (los utilizados almenos)\!\!\!

``` r
# Matriz Stint y vectores respuesta:
stints <- as.data.frame(matrix( ,0,length(players)))
names(stints) <- players
stintCount = 1

# Acciones que termina una posesion de pelota:
## Canastas encestada (2/3/tiro libre), rebote defensivo, pelota recuperada
shots <- pbp_2018_fixed[which(pbp_2018_fixed$play_type == "2FGM" | pbp_2018_fixed$play_type == "3FGM" | pbp_2018_fixed$play_type == "FTM" | pbp_2018_fixed$play_type == "DRB" | pbp_2018_fixed$play_type == "TOV"),]

#View(shots)
num_shots <- dim(shots)[1]

shots$points = 0
for(i in 1:num_shots){
  if(shots$play_type[i] == "2FGM") shots$points[i]=2
  if(shots$play_type[i] == "3FGM") shots$points[i]=3
  if(shots$play_type[i] == "FTM") shots$points[i]=1
}

awayplayersStart <- levels(droplevels(as.factor(unique(unlist(shots[30,24:28])))))
homeplayersStart <- levels(droplevels(as.factor(unique(unlist(shots[1,19:23])))))

awayplayers<-matrix(,num_shots,5)
homeplayers<-matrix(,num_shots,5)

for (i in 1:num_shots) {
  awayplayers[i,] <- levels(droplevels(as.factor(unique(unlist(shots[i,24:28])))))
  homeplayers[i,] <- levels(droplevels(as.factor(unique(unlist(shots[i,19:23])))))
}

#View(awayplayers)

bothHome <- homeplayersStart %in% homeplayers
bothAway <- awayplayersStart %in% awayplayers

possessions=0

#NO FUNCIONA!! 
if(all(bothHome) == TRUE & all(bothAway)==TRUE){
  for (i in 1:num_shots) {
  if(shots[i,'play_type'] == "FTM"| shots[i,'play_type'] == "2FGM"| shots[i,'play_type'] == "3FGM"){
    possessions = possessions + 1
  }
  else if(shots[i,'play_type'] == "DRB"){
    possessions= possessions + 1
  }
  else if(shots[i,'play_type'] == "TOV"){
    possessions= possessions + 1
  }
  }
}
```
