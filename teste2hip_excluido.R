# Segundo teste
## Segundo teste

- <i>H<sub>0</sub></i>: A proporção de pessoas com e sem casa própria que gastam mais de 5 horas em redes sociais é igual; 
- <i>H<sub>a</sub></i>: A proporção de pessoas com e sem casa própria que gastam mais de 5 horas em redes sociais é diferente; 

``` {r teste_2, fig.align='center'}

dummy_hm_owner <- dummy_db |> 
  filter(isHomeOwner)

dummy_not_hm_owner <- dummy_db |> 
  filter(!isHomeOwner)                  

p1 <- sum(dummy_hm_owner$more_than_5) / nrow(dummy_hm_owner)
p2 <- sum(dummy_not_hm_owner$more_than_5) / nrow(dummy_not_hm_owner) 
p <- ((nrow(dummy_hm_owner) * p1) + (nrow(dummy_not_hm_owner) * p2)) / nrow(dummy_db)

z <- (p1 - p2)/(sqrt(p*(1-p)*((1 / nrow(dummy_hm_owner)) + (1 / nrow(dummy_not_hm_owner)))))

p_valor <- pnorm(z)

hm_owner <- c(sum(dummy_hm_owner$more_than_5), nrow(dummy_hm_owner) - sum(dummy_hm_owner$more_than_5), nrow(dummy_hm_owner))
not_hm_owner <- c(sum(dummy_not_hm_owner$more_than_5), nrow(dummy_not_hm_owner) - sum(dummy_not_hm_owner$more_than_5), nrow(dummy_not_hm_owner))

table_hm_owners <- data.frame(Possui = hm_owner, Não_Possui = not_hm_owner)

knitr::kable(t(table_hm_owners),
             col.names = c("Mais de 5 horas", "5 horas ou menos", "Total"), 
             row.names = TRUE) %>%
  kable_styling(full_width = F, font_size = 23) 

```
## Conclusão

```{r conc_teste_2, fig.align='center'}

ggplot() +
  stat_function(fun = dnorm,
                geom = "line", 
                xlim = c(-4,4),
                linewidth = 0.8) +
  stat_function(fun = dnorm,
                geom = "area", fill = "red",
                alpha = 0.2,
                xlim = c(-5, z)) +
  stat_function(fun = dnorm,
                geom = "area", fill = "red",
                alpha = 0.3,
                xlim = c(-5, lim_inf)) +
  stat_function(fun = dnorm,
                geom = "area", fill = "red",
                alpha = 0.3,
                xlim = c(5, lim_sup)) +
  geom_line(aes(x = lim_inf, y = c(0, 0.06))) +
  geom_line(aes(x = lim_sup, y = c(0, 0.06))) +
  geom_line(aes(x = z, y = c(0, 0.387))) +
  annotate(geom = "text", 
           x = -3.45, 
           y = 0.1, 
           label = "-1.96",
           family = "Playfair",
           size = 13) +
  annotate(geom = "text", 
           x = 3.35, 
           y = 0.1, 
           label = "1.96",
           family = "Playfair",
           size = 13) +
  annotate(geom = "text", 
           x = 1.80, 
           y = 0.37, 
           label = "0.24",
           family = "Playfair",
           size = 13) + 
  annotate(geom = "text", 
           x = 3.5, 
           y = 0.4, 
           label = paste("P-valor: ", round(p_valor, 4)),
           family = "Playfair",
           size = 13) +
  annotate(geom = "curve", 
           x = lim_inf,
           y = 0.06,
           xend = -3, 
           yend = 0.1,
           linewidth = 0.5,
           arrow = arrow(length = unit(3, "mm"))
  ) +
  annotate(geom = "curve", 
           x = lim_sup,
           y = 0.06,
           xend = 3, 
           yend = 0.1,
           linewidth = 0.5,
           curvature = -0.4,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "curve", 
           x = z,
           y = 0.385,
           xend = 1.45, 
           yend = 0.375,
           linewidth = 0.5,
           curvature = -0.3,
           arrow = arrow(length = unit(3, "mm"))) +
  labs(x = "Z",
       y = "Densidade") +
  theme(plot.background = element_rect(fill = "#fcfbf9"),
        panel.background = element_rect(fill = NA),
        plot.title = element_text(family = "Playfair", hjust = .5, size = 45, lineheight = 0.5),
        axis.title = element_text(family = "Playfair", hjust = .5, size = 30),
        axis.text = element_text(family = "Playfair", size = 30))

```

Portando aceitarmos <i>H<sub>0</sub></i>. 
Desta forma, concluímos que não há evidências para acreditarmos que há diferença entre a proporção de pessoas que possuem e não possuem casa que usam as redes sociais por mais de 5 horas.