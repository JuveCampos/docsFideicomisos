library(tidyverse)
library(rtweet)

# Documentacion:
# https://github.com/ropensci/rtweet

# Buscar tweets de un tema particular.

# Busqueda Avanzada: https://twitter.com/search-advanced?lang=es
# Ejemplo: "fideicomiso" min_faves:100 lang:es until:2020-09-30 since:2020-09-28
# Busca la palabra exacta fideicomisos, con minimo 100 likes, en español, del 28 al 30 de septiembre del 2020.
query <- 'fideicomisos lang:es'
# query <- 'fideicomiso'

# Busqueda
# (Solo se pueden descargar 15,000 tweets cada 15 minutos)
bd <- search_tweets(query,  # Busqueda
                    n = 15000, # Numero Maximo de Tweets
                    include_rts = FALSE, # Incluir Rts
                    retryonratelimit = TRUE)



# Nube de palabras:
create_wordcloud <- function(data, stop_words = c(), num_words = 100, background = "white",  mask = NULL) {
  # Checar si esta instalado Pacman
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(wordcloud2, tm, stringr)

  # Pre-Función para eliminar simbolos raros
  quitar_signos <- function(x)  stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))

  # If text is provided, convert it to a dataframe of word frequencies
  # Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    # Convertimos el texto dentro del Corpus a Minusculas
    corpus <- tm_map(corpus, tolower)
    # Removemos la puntuacion (.,-!?)
    corpus <- tm_map(corpus, removePunctuation)
    # Removemos los numeros
    corpus <- tm_map(corpus, removeNumbers)
    # Removemos los signos de admiracion e interrogacion al reves
    corpus <- tm_map(corpus, quitar_signos)
    # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
    # a las oraciones. Para saber cuales, escribir: stopwords("spanish))
    corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
    # Generamos una matriz para hacer el conteo
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    # Obtenemos el numero de la frecuencia de cada palabra
    data <- sort(rowSums(tdm), decreasing = TRUE)
    # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }

  freq_palabras <<- data

  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }

  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  wordcloud2(data, backgroundColor = background, color = "random-dark", fontFamily = "Asap", size = 2)
}

# Creamos una visualización:
create_wordcloud(data = bd$text)

library(rebus)

bd$seg <-  str_extract(tolower(bd$text),
            pattern = "fideicomisos" %R% SPC %R% capture(one_or_more(WRD)) %R% SPC %R% capture(one_or_more(WRD)) %R% SPC %R% capture(one_or_more(WRD)))

phra <- bd %>%
  select(seg) %>%
  na.omit() %>%
  group_by(seg) %>%
  count() %>%
  arrange(-n)


# # Stream de tweets
# rt <- stream_tweets(timeout = 30)
#
# # Obtener a quienes sigue un usuario ----
# juve_fds <- get_friends("JuvenalCamposF")
#
# ## Ver bien a los usuarios
# juve_fds_data <- lookup_users(juve_fds$user_id)
# juve_fds_data$screen_name
#
# ## Quien sigue al usuario ----
# ## get user IDs of accounts following CNN
# juve_flw <- get_followers("JuvenalCamposF", n = 75000)
#
# ## lookup data on those accounts
# juve_flw_data <- lookup_users(juve_flw$user_id)
# juve_flw_data$screen_name
#
# # Likes y Favs
# juveFavs <- get_favorites("JuvenalCamposF", n = 100)
#
# juveFaves <- juveFavs %>%
#   select(screen_name, text, created_at) %>%
#   arrange(created_at) %>%
#   mutate(Fecha = as.Date(created_at)) %>%
#   group_by(Fecha) %>%
#   count() %>%
#   ggplot(aes(x = Fecha, y = n)) +
#   geom_col()
#
# plotly::ggplotly(juveFaves)
