library(rvest)
library(tidyverse)

site_url <- "Archiwum Danych o Posłach_2.html"

urls_list <- read_html(site_url) %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  keep(~!is.na(.x))

scrape_url <- function(selected_url) {
  
  selected_url <- read_html(selected_url)
  
  tibble(
    id = selected_url %>%
      html_elements("[class='posel']") %>%
      html_text(),
    komisje = selected_url %>%
      html_elements("[class='old']") %>%
      html_text2() %>%
      str_split("\n") %>%
      flatten_chr(),
    klub = selected_url %>%
      html_elements("[class='Klub']") %>%
      html_text() %>% 
      .[1]
  )
}

res <- urls_list %>% 
  map_dfr(scrape_url, .progress = T)

res <- res %>% 
  mutate(
    komisje = if_else(
      komisje == "Handlu Wenętrznego",
      "Handlu Wewnętrznego",
      komisje
    ),
    komisje = if_else(
      komisje == "Przemyłu Lekkiego, Rzemiosła i Spółdzielczości Pracy",
      "Przemysłu Lekkiego, Rzemiosła i Spółdzielczości Pracy",
      komisje
    ),
    komisje = if_else(
      komisje == "Przemysłu Lekkiego, Rzemiosła i Spółdzielczośsci Pracy",
      "Przemysłu Lekkiego, Rzemiosła i Spółdzielczości Pracy",
      komisje
    )
  )

res

library(igraph)

net <- graph_from_data_frame(
  res %>%
    transmute(from = id, to = komisje),
  directed = F
) 

net <- net %>% 
  set_vertex_attr(
    "type",
    value = bipartite_mapping(net)$type
  )

tibble(
  name = V(net)$name,
  btwn = betweenness(net)) %>% 
  arrange(desc(btwn)) %>% 
  print(n = 100)

nets <- bipartite_projection(net)

btwn <- betweenness(nets$proj2,
                    weights = get.edge.attribute(nets$proj2, "weight"),
                    normalized = T,cutoff = -1)


res %>% 
  distinct(komisje) %>% 
  left_join(
    tibble(
      komisje = names(btwn),
      bc = as.numeric(btwn)
    )
  ) %>% 
  filter(bc > 0) %>% 
  ggplot(aes(x = komisje, y = bc)) +
  geom_col() +
  coord_flip() 


plot(net,
     vertex.size=3,
     vertex.label=NA,
     layout=layout.kamada.kawai,
     vertex.color=if_else(bipartite_mapping(net)$type, "gold", "red"))

plot(nets$proj1, vertex.size=3, vertex.label=NA,
     edge.width = (get.edge.attribute(nets$proj1, "weight")))

plot(nets$proj2, edge.width = (get.edge.attribute(nets$proj2, "weight")))

net1 <- delete.vertices(
  net, 
  V(net)[ degree(net) == 1 ] 
)

plot(net1,
     vertex.size=3,
     vertex.label=NA,
     layout=layout.kamada.kawai,
     vertex.color=if_else(bipartite_mapping(net1)$type, "gold", "red"))
