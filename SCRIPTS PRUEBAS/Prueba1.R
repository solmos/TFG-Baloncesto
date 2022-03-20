## PRUEBA 1 - Con cual nos quedamos

players_Merged <- df_order_toy$Merged_Players == lag(df_order_toy$Merged_Players)
SG_DIF <- df_order_toy$Merged_SG == lag(df_order_toy$Merged_SG)

df_SamePlayers <- data.frame(
  "ID_row" = df_order_toy$id_play,
  "SG" = c(SG_DIF[-1], "FALSE"),
  "TF" = c(players_Merged[-1], "FALSE")  # Eliminamos el primer NA y añadimos 
  # FALSE a la ultima entrada
)

#TRUE: No hay cambios. FALSE: Hay cambios

df_SamePlayers

df_SamePlayers[((df_SamePlayers$TF=="FALSE")|(df_SamePlayers$SG=="FALSE")),]
# para quedarnos las que haya un cambio de jugar y tambien un cambio de partido

### Hay que quedarse filas: 4, 7, 8, 9, 10, 15