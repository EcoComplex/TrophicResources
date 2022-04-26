
#
# Leer datos csv
#
require(tidyverse)
require(forcats)

data <- read_csv2("Data/ecologia_trofica_21.csv",locale = readr::locale(encoding = "ISO-8859-1")) %>% mutate(METHOD=ifelse(is.na(METHOD), "review", METHOD ))
names(data)
str(data)

#
# Chequear si hay NA en GENERO
#

data %>% filter(is.na(G_SP)) 
                                        
datosna <- data %>% filter(is.na(ORDER))


#
# Agrupar por TAXONOMICO por METODO para grafico ----------- ORDENAR BARRAS DE MAYOR A MENOR ----------
#
frec_data <- data %>% mutate(METHOD=factor(METHOD),TAXONOMIC_LEVEL=factor(TAXONOMIC_LEVEL)) %>% group_by(METHOD,TAXONOMIC_LEVEL) %>% summarise(n=n()) %>% mutate(TAXONOMIC_LEVEL=fct_recode(TAXONOMIC_LEVEL, Species="SPECIE",Family="FAMILY",Genera="GENERA"))

# Reordenar
#
f <- frec_data %>% group_by(METHOD) %>% summarise(n=sum(n)) %>% arrange(desc(n)) 
frec_data$METHOD <- fct_relevel(frec_data$METHOD,as.character(f$METHOD))
#frec_data$TAXONOMIC_LEVEL <- fct_relevel(frec_data$TAXONOMIC_LEVEL, c("Family","GENERA","SPECIES"))

frec_data  %>% ggplot( aes(x=METHOD, y=n, fill=TAXONOMIC_LEVEL)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired",name="Taxonomic\nLevel")+ xlab("Method")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

ggsave("Figures/Metodo_ByTaxLevel.jpeg",width=6,height=6,units="in",dpi=600)


#
# Agrupar por TAXONOMICO por RECURSO para grafico --- limpiar columnas con nombre parecido 
#
frec_data <- data %>% mutate(RESOURCE=factor(RESOURCE),TAXONOMIC_LEVEL=factor(TAXONOMIC_LEVEL)) %>% group_by(TAXONOMIC_LEVEL,RESOURCE) %>% summarise(n=n())  %>% mutate(TAXONOMIC_LEVEL=fct_recode(TAXONOMIC_LEVEL, Species="SPECIE",Family="FAMILY",Genera="GENERA"))

# Reordenar
#
f <- frec_data %>% group_by(RESOURCE) %>% summarise(n=sum(n)) %>% arrange(desc(n)) 
frec_data$RESOURCE <- fct_relevel(frec_data$RESOURCE,as.character(f$RESOURCE))
#frec_data$TAXONOMIC_LEVEL <- fct_relevel(frec_data$TAXONOMIC_LEVEL, c("FAMILY","SUBFAMILY","GENERA","SPECIES"))

ggplot(data=frec_data, aes(x=RESOURCE, y=n, fill=TAXONOMIC_LEVEL)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired",name="Taxonomic\nLevel")+ xlab("Resource")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave("Figures/Recursos_ByTaxLevel.jpeg",width=6,height=6,units="in",dpi=600)

#
# Agrupar por TAXONOMICO por METODO para grafico --- limpiar columnas con nombre parecido 
#
frec_data <- data %>% group_by(METHOD,RESOURCE) %>% summarise(n=n()) 

# Reordenar
#
f <- frec_data %>% group_by(RESOURCE) %>% summarise(n=sum(n)) %>% arrange(desc(n)) 
frec_data$RESOURCE <- fct_relevel(frec_data$RESOURCE,as.character(f$RESOURCE))

ggplot(data=frec_data, aes(x=RESOURCE, y=n, fill=METHOD)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired",name="Method")+ xlab("Resource")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave("Figures/Recursos_ByMetodo.jpeg",width=6,height=6,units="in",dpi=600)



#
# Distribucion de recursos por grupo taxonomico (GENERO) --- Agregar columna grupo trofico
#
distr_data <- data %>% group_by(GENERA,RESOURCE) %>% distinct(RESOURCE) %>% ungroup() %>% count(GENERA) %>%  mutate(TaxonomicGroup = reorder(GENERA,-n)) 
ggplot(data=distr_data, aes(x=TaxonomicGroup, y=n, color=TaxonomicGroup)) +
  geom_point()+
  scale_color_viridis_d(guide=FALSE)+
  theme_minimal() + theme(axis.text.x=element_blank()) # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6)) 


distr_data <- data %>% filter(!is.na(SUBCLASE)) %>%  group_by(SUBCLASE,GENERA,RESOURCE) %>% distinct(RESOURCE) %>% ungroup() %>% count(SUBCLASE,GENERA) %>%  
  mutate(TaxonomicGroup = reorder(str_to_title(GENERA),n))


ggplot(data=distr_data, aes(y=TaxonomicGroup, x=n, color=n,)) +
  geom_jitter(size=.5)+ 
  scale_x_continuous(limits = c(0, 10), expand = c(0.005, 0.005),breaks=1:11) +
  scale_color_viridis_c(guide=FALSE,option="B")+
  theme_classic() + theme(axis.text.x=element_blank()) + facet_wrap( ~SUBCLASE) + coord_flip()

ggsave("Figures/Distrib_Res_ByTaxGr.jpeg",width=6,height=6,units="in",dpi=600)

#
# Hay un 20% que tiene >= 7 recursos, 60% tiene 6- 3 , 20% tiene 1-2 recursos
#

#
# Distribucion de cantidad de grupos taxonomicos por recurso = CUALES SON LOS RECURSOS mas usados
#

distr_data <- data %>% group_by(GENERA,RESOURCE) %>% distinct(RESOURCE) %>% ungroup() %>% count(RESOURCE) %>%  mutate(Resource = reorder(RESOURCE,-n)) 

ggplot(data=distr_data, aes(x=Resource, y=n, fill=Resource)) +
  geom_bar(stat="identity")+
  scale_fill_viridis_d(guide=FALSE,option="B")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=8))

ggsave("Figures/Distrib_TaxGr_ByRes.jpeg",width=6,height=6,units="in",dpi=600)


#
# La distribucion de grupos taxomomicos por recurso es más sesgada que la distribucion de recursos por grupo taxonomico 
# 




#
# Frecuencia de RECURSO por GENERO en cantidad de citas (SIRVE PARA HACER EL TRIANGULO)
#
data_recurso <- data %>% group_by(GENERA,RESOURCE)  %>% summarise(n=n()) %>% mutate(freq = n / sum(n))

#
# Frecuencia de RECURSO por FAMILIA en cantidad de citas (SIRVE PARA HACER EL TRIANGULO)
#
data_recurso <- data %>% filter(!is.na(FAMILY)) %>% group_by(FAMILY,RESOURCE)  %>% summarise(n=n()) %>% mutate(freq = n / sum(n))
write_csv(data_recurso,"data_caja_familia.csv")

#
# Citas por METODO 
#
data_metodo <- data %>% group_by(Cita,METHOD) %>% summarise(n=n())

#
# Graficos ternarios
#
# https://cran.r-project.org/web/packages/Ternary/vignettes/Ternary.html
#
# install.packages('Ternary')

data_recurso <- data %>% drop_na(ORDER) %>% filter(ORDER!="Holothyrida") %>%  group_by(ORDER,RESOURCE) %>% summarise(n=n()) %>% mutate(freq = n / sum(n)) %>% select(RESOURCE,ORDER,freq) 
require(igraph)
g <- graph_from_data_frame(data_recurso)
g
V(g)$label <- V(g)$name
V(g)$type <- !(V(g)$label %in% c("Arthropleona","Mesostigmata","Neelipleona",   
                                "Opilioacarida","Sarcoptiformes","Symphypleona","Trombidiformes"))

require(ggraph)
require(ggrepel)

ggraph(g, layout="bipartite") +  
  geom_node_point(aes(colour = V(g)$name),size=8) +   geom_edge_link(aes(alpha = E(g)$freq,width = E(g)$freq),
                                                              #arrow = arrow(length = unit(4, 'mm')), 
                                                              start_cap = circle(3, 'mm'),
                                                              end_cap = circle(3, 'mm')) +  
  #geom_node_text(aes(label = V(g)$name), repel=TRUE,nudge_y = 0.0,label.size=NA) +  
  geom_label_repel(aes(x=x,y=y,label=V(g)$name,colour=V(g)$name),nudge_y = 0.0) +
  theme_graph() +
  scale_color_viridis_d(option="plasma",guide=NULL) + theme(legend.position="none") 

ggsave("Figures/Bi_Orden_Recurso.jpeg",width=9,height=6,units="in",dpi=600)
#
# Guardar como PDF y editar
#

