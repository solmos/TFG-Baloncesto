Alineaciones con jugadores repetidos
================

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
write_csv(pbp_2018_fixed, here("data", "pbp_2018_fixed.csv"))
```
