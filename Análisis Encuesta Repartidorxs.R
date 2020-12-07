########################################################
#
########################################################
rm(list = ls())

### Instalar una paquetería que facilita apertura de paqueterías: pacman
#install.packages("pacman") 
library(pacman)

# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, DescTools, lmtest)


### Setup ----
options(scipen=999) # Prevenir notación científica
#-----------------

###Carpeta de trabajo y carga de datos
setwd ("~/Google Drive/Gatitos/Proyectos/Menos (Rappi)do, más justo/Senado/Encuesta/")
datitos <- read_csv("Condiciones laborales de rep. 14 Julio 2020.csv")


glimpse(datitos)



#2. Metodología  
dim(datitos)
#2.1 Entidad de trabajo
cro(datitos$entidad_trabajo)

datitos %<>% 
  mutate( entidad_trabajo_r = case_when(
      entidad_trabajo == "Ciudad de México" ~ "CDMX",
      entidad_trabajo == "Jalisco"    ~ "Jalisco, NL, Puebla",
      entidad_trabajo == "Puebla"     ~ "Jalisco, NL, Puebla",
      entidad_trabajo == "Nuevo León" ~ "Jalisco, NL, Puebla",
      TRUE                      ~ "Otros"
    ))

cro(datitos$entidad_trabajo_r)

head(datitos$plataformas)
  





#2.1. Características de la población encuestada
#Empresa, 

#Demográficos: 
#Sexo, 
cro(datitos$genero)
class(datitos$genero)
datitos %<>% 
  mutate(genero=  factor(genero))
cro(datitos$genero)

#edad, 
class(datitos$edad)
summary(datitos$edad)
cro(datitos$edad)
cro(datitos$edad, datitos$genero)

#educación
class(datitos$edu_ego)
datitos %<>% 
  mutate(edu_ego=  factor(edu_ego))
cro(datitos$edu_ego)
cro(datitos$edu_ego,datitos$genero)

#Se consideran de tal clase, y han tenido tal movilidad social.

class(datitos$clase_subjetiva)
datitos %<>% 
  mutate(clase_subjetiva=  factor(clase_subjetiva))
cro(datitos$clase_subjetiva)




# 3. Precariedad laboral

# 3.1. ¿Realmente permite la flexibilidad laboral que promete?

#Dias de trabajo
class(datitos$trabajo_dias)
summary(datitos$trabajo_dias)
cro(datitos$trabajo_dias)


#   Tiempo de trabajo, 
class(datitos$trabajo_horas)
summary(datitos$trabajo_horas)
cro(datitos$trabajo_horas)
cro(datitos$trabajo_horas, datitos$genero)


tabla <- datitos %>%
  group_by(genero) %>%
  summarise(p25= quantile(trabajo_horas,0.25,na.rm=TRUE ),
            media= mean(trabajo_horas,na.rm=TRUE),
            mediana= median(trabajo_horas,na.rm=TRUE),
            p75= quantile(trabajo_horas,0.75,na.rm=TRUE ))
tabla
rm(tabla)


#Trabajo de cuidados, 
cro(datitos$trabajo_horas)
datitos$trabajo_horas[is.na(datitos$trabajo_horas)] <- 0
cro(datitos$trabajo_horas)
datitos$trabajo_cuidados_tiempo[is.na(datitos$trabajo_cuidados_tiempo)] <- 0
datitos$trabajo_tiempo_transporte[is.na(datitos$trabajo_tiempo_transporte)] <- 0  

tabla <- datitos %>%
  group_by(genero) %>%
  summarise(p25= quantile(trabajo_cuidados_tiempo,0.25,na.rm=TRUE ),
            media= mean(trabajo_cuidados_tiempo,na.rm=TRUE),
            mediana= median(trabajo_cuidados_tiempo,na.rm=TRUE),
            p75= quantile(trabajo_cuidados_tiempo,0.75,na.rm=TRUE ))
tabla


datitos %<>% 
  mutate(suma_horas=  trabajo_horas + trabajo_cuidados_tiempo )

cro(datitos$suma_horas)
summary(datitos$suma_horas)

tabla <- datitos %>%
  group_by(genero) %>%
  summarise(p25= quantile(suma_horas,0.25,na.rm=TRUE ),
            media= mean(suma_horas,na.rm=TRUE),
            mediana= median(suma_horas,na.rm=TRUE),
            p75= quantile(suma_horas,0.75,na.rm=TRUE ))
tabla




#Horarios

cro(datitos$trabajo_hora_inicio)
cro(datitos$trabajo_hora_final)

tabla <- datitos %>%
  group_by(genero) %>%
  summarise(p25= quantile(trabajo_tiempo_transporte,0.25,na.rm=TRUE ),
            media= mean(trabajo_tiempo_transporte,na.rm=TRUE),
            mediana= median(trabajo_tiempo_transporte,na.rm=TRUE),
            p75= quantile(trabajo_tiempo_transporte,0.75,na.rm=TRUE ))
tabla

cro(datitos$gasto_diarios_comida)
 
# 3.2. ¿Se puede generar ingresos para sobrevivir?
#   Ingresos, gastos y utilidades.
datitos$gasto_sem_trabajo[is.na(datitos$gasto_sem_trabajo)] <- 0
datitos$gasto_diarios_comida[is.na(datitos$gasto_diarios_comida)] <- 0
datitos$gasto_mensual_reparac[is.na(datitos$gasto_mensual_reparac)] <- 0

datitos$ingreso_sem[is.na(datitos$ingreso_sem)] <- 0

datitos$trabajo_horas[is.na(datitos$trabajo_horas)] <- 40
cro(datitos$utilidades_sem)

tabla <- datitos %>%
  summarise(p25= quantile(ingreso_sem,0.25,na.rm=TRUE ),
            media= mean(ingreso_sem,na.rm=TRUE),
            mediana= median(ingreso_sem,na.rm=TRUE),
            p75= quantile(ingreso_sem,0.75,na.rm=TRUE ))
tabla



datitos %<>% 
  mutate( gasto_tot_sem= gasto_sem_trabajo+(gasto_diarios_comida*7)+(gasto_mensual_reparac/30.5*7),
          utilidades_sem = ingreso_sem - gasto_tot_sem,
          utilidad_hora = utilidades_sem / trabajo_horas)


tabla <- datitos %>%
  summarise(p25= quantile(gasto_tot_sem,0.25,na.rm=TRUE ),
            media= mean(gasto_tot_sem,na.rm=TRUE),
            mediana= median(gasto_tot_sem,na.rm=TRUE),
            p75= quantile(gasto_tot_sem,0.75,na.rm=TRUE ))
tabla


tabla <- datitos %>%
  summarise(p25= quantile(utilidades_sem,0.25,na.rm=TRUE ),
            media= mean(utilidades_sem,na.rm=TRUE),
            mediana= median(utilidades_sem,na.rm=TRUE),
            p75= quantile(utilidades_sem,0.75,na.rm=TRUE ))
tabla

tabla <- datitos %>%
  summarise(p25= quantile(utilidad_hora,0.25,na.rm=TRUE ),
            media= mean(utilidad_hora,na.rm=TRUE),
            mediana= median(utilidad_hora,na.rm=TRUE),
            p75= quantile(utilidad_hora,0.75,na.rm=TRUE ))
tabla

# Cuantos tienen otros empleos (probabilidad según nivel de ingreso u horas de trabajo)
cro(datitos$trabajo_otros_empleos)
datitos_f <- datitos %>%
  filter (trabajo_horas>39)
cro(datitos_f$trabajo_otros_empleos)

# Comparar con línea de pobreza, ingreso per cápita.
cro(datitos$ingreso_dependientes)
datitos$ingreso_dependientes[is.na(datitos$ingreso_dependientes)] <- 0

datitos %<>% 
  mutate( tam_hog= 1 + ingreso_dependientes,
          utilidades_pc = (utilidades_sem/7*30.5)/tam_hog)
          
tabla <- datitos %>%
  summarise(p25= quantile(utilidades_pc,0.25,na.rm=TRUE ),
            media= mean(utilidades_pc,na.rm=TRUE),
            mediana= median(utilidades_pc,na.rm=TRUE),
            p75= quantile(utilidades_pc,0.75,na.rm=TRUE ))
tabla



# 4. Precariedad laboral
# 4.1. Discriminación repartidores
# Frecuencia y razones discriminación. Relación con tono de piel y orientación sexual.
class(datitos$discrim_si)
datitos %<>% 
  mutate(discrim_si=  factor(discrim_si))
cro(datitos$discrim_si)
cro(datitos$discrim_si, datitos$genero)
cro(datitos$discrim_si, datitos$orientacion_sex)

datitos$orientacion_sex_r  <-  fct_recode(datitos$orientacion_sex, 
                                   no_hetero = "bisexual",
                                   no_hetero = "Bisexual",
                                   no_hetero = "Homosexual",
                                   no_hetero = "Pansexual")
                                   
cro(datitos$orientacion_sex_r)                                                 
cro(datitos$orientacion_sex_r, datitos$discrim_si)
cro(datitos$tono_perla, datitos$discrim_si)
cro(datitos$h_ind, datitos$discrim_si)

# Discriminación en uso de baño y en anclaje de vehículos.
cro(datitos$discrim_estacionar)
cro(datitos$discrim_banio)

# Frecuencia acoso. Relación con mujeres.
cro(datitos$acoso_si)
cro(datitos$acoso_si,datitos$genero)


# 4.2. Riesgos de accidentes y robos
# Frecuencia de accidentes. Atención médica, Hospitalaria, apoyo por parte de la app.
cro(datitos$accidente_si)
cro(datitos$accidente_hospit, datitos$accidente_si)


hospitalizados <- datitos %>%
  filter(accidente_si=="Sí, requerí hospitalización.") 

cro(hospitalizados$accidente_hospit_dias)


cro(datitos$accidente_apoyo_app)
accidentados <- datitos %>%
  filter(accidente_si=="Sí") 
cro(accidentados$accidente_apoyo_app)

  




rm(hospitalizados,tabla,accidentados)



# Frecuencia de robos
cro(datitos$robo_si)
cro(datitos$entidad_trabajo_r, datitos$robo_si)





# 5. Precariedad laboral

cro(datitos$castigo_si)
cro(datitos$desactivado_si)


cro(datitos$socio)

#########################
#Gráfica 1
#########################
datitos %<>%
  filter(suma_horas<168)
datitos_f <-datitos %>%
  filter(genero %in% c("Hombre", "Mujer"))

##Figure 1 
#####################
#Principal AES
ggplot(data =datitos_f,
       mapping  = aes(x = suma_horas  
                      ,
                      fill =genero
                      #y = porc_viv,
                      #group=genero,
                      #size=hli,
                      #label = ent_corto
       )
) +
  #Geom_point
  geom_density(alpha=0.4
    #fill="#dc5356", color="#dc5356", alpha=0.8
    )+
  #COlores
  scale_fill_manual(values=c("#ffd756", "#373535"))+
  #facet_wrap(~genero) +
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
    #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(
    #labels =scales::dollar
    #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Horas trabajadas",
         y = "Densidad",
         title = "Gráfica 2: Horas trabajadas semanalmente por repartidorxs de apps",
         subtitle = "Suma de trabajo remunerado y no remunerado"
        # ,
         #caption = "Fuente: Elaboración propia, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
  )
        #)+
  #Lineas de mediana y media
  #Linea 1:
  # geom_vline(aes(xintercept=quantile(suma_horas, 0.25)),
  #            color="#3a3a39", linetype="dashed", size=0.5)+
  # geom_text(aes(x=quantile(suma_horas, 0.25), label="25% con menos de 50 horas\n", y=0.0028), colour="#3a3a39", angle=90, text=element_text(size=11)
  # )+
  # #Linea 2:
  # geom_vline(aes(xintercept=median(suma_horas)),
  #            color="#3a3a39", linetype="dashed", size=0.5)+
  # geom_text(aes(x=median(suma_horas), label="Mediana: 72 horas\n", y=0.0025), colour="#3a3a39", angle=90, text=element_text(size=11))
#Save Figure 1
#ggsave("Utilidades diarias_w.png", width = 7)




##Figure English
#####################
#Principal AES
ggplot(data =datitos_f,
       mapping  = aes(x = suma_horas  
                      ,
                      fill =genero
                      #y = porc_viv,
                      #group=genero,
                      #size=hli,
                      #label = ent_corto
       )
) +
  #Geom_point
  geom_density(alpha=0.4
               #fill="#dc5356", color="#dc5356", alpha=0.8
  )+
  #COlores
  #scale_fill_discrete(name = "Sex", labels = c("A", "B"))+
  scale_fill_manual(values=c("#ffd756", "#373535"),
                    name = "Sex", labels = c("Man", "Woman"))+
  #facet_wrap(~genero) +
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
    #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(
    #labels =scales::dollar
    #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Weekly Worked Hours",
         y = "Density"
         ,
         fill="Sex"
         #,
         #title = "Gráfica 2: Horas trabajadas semanalmente por repartidorxs de apps",
         #subtitle = "Suma de trabajo remunerado y no remunerado"
         # ,
         #caption = "Fuente: Elaboración propia, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
  ) 
ggsave("Weekly Worked Hours.png", width = 7)





#########################
#Gráfica 2
#########################

datitos %<>%
  filter(suma_horas<168)
cro(datitos$genero)

datitos_g <- datitos %>%
  filter(genero==c("Hombre","Mujer"))

cro(datitos_g$genero)

##Figure 1 
#####################
#Principal AES
ggplot(data =datitos,
       mapping  = aes(x = suma_horas  
                      ,
                      #y = porc_viv,
                      group=genero,
                      #size=hli,
                      #label = ent_corto
       )
) +
  #Geom_point
  geom_density(fill="#dc5356", color="#dc5356", alpha=0.8)+
  facet_wrap(~genero) +
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
    #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(
                    #labels =scales::dollar
                     #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Horas trabajadas",
         y = "Densidad",
         title = "Horas trabajadas semanalmente por repartidorxs de apps",
         subtitle = "Contando trabajo remunerado y no remunerado",
         caption = "Fuente: Elaboración propia, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
  )+
  #Lineas de mediana y media
  #Linea 1:
  geom_vline(aes(xintercept=quantile(suma_horas, 0.25)),
             color="#3a3a39", linetype="dashed", size=0.5)+
  geom_text(aes(x=quantile(suma_horas, 0.25), label="25% con menos de 50 horas\n", y=0.0028), colour="#3a3a39", angle=90, text=element_text(size=11)
  )+
  #Linea 2:
  geom_vline(aes(xintercept=median(suma_horas)),
             color="#3a3a39", linetype="dashed", size=0.5)+
  geom_text(aes(x=median(suma_horas), label="Mediana: 72 horas\n", y=0.0025), colour="#3a3a39", angle=90, text=element_text(size=11))
#Save Figure 1
#ggsave("Utilidades diarias_w.png", width = 7)






summary(datitos$utilidad_hora)

datitos_f <-datitos %>%
  filter(utilidad_hora>-50 & utilidad_hora<180)


##Figure 2
#####################
#Principal AES
ggplot(data =datitos_f,
       mapping  = aes(x = utilidad_hora
                      #y = porc_viv,
                      #color=elec_gob_2020_21,
                      #size=hli,
                      #label = ent_corto
       )
) +
  #Geom_point
  geom_density(fill="#dc5356", color="#dc5356", alpha=0.8)+
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
    #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(labels =scales::dollar
                     #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Utilidades",
         y = "Densidad",
         title = "Gráfica 3: Utilidades por hora generadas ",
         subtitle = "Luego de descontar costos de trabajo y antes del pago de impuestos"
         #caption = "Fuente: Elaborado por @gatitosvsdesig, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
  )
  #Lineas de mediana y media
  #Linea 1:
  # geom_vline(aes(xintercept=quantile(utilidad_hora, 0.25)),
  #            color="#3a3a39", linetype="dashed", size=0.5)+
  # geom_text(aes(x=quantile(utilidad_hora, 0.25), label="25% inferior gana $4 por hora o menos \n", y=0.0085), colour="#3a3a39", angle=90, text=element_text(size=7)
  # )+
  # #Linea 3:
  # geom_vline(aes(xintercept=quantile(utilidad_hora, 0.75)),
  #            color="#3a3a39", linetype="dashed", size=0.5)+
  # geom_text(aes(x=quantile(utilidad_hora, 0.75), label="25% superior gana $54 por hora o más \n", y=0.008), colour="#3a3a39", angle=90, text=element_text(size=7)
  # )+
  # #Linea 2:
  # geom_vline(aes(xintercept=median(utilidad_hora)),
  #            color="#3a3a39", linetype="dashed", size=0.5)+
  # geom_text(aes(x=median(utilidad_hora), label="Mediana: $42 por hora \n", y=0.008), colour="#3a3a39", angle=90, text=element_text(size=11))
  # 

# #Save Figure 1
# ggsave("Utilidades por hora.png", width = 7)




##Figure 2. English
#####################
#Principal AES
ggplot(data =datitos_f,
       mapping  = aes(x = utilidad_hora
                      #y = porc_viv,
                      #color=elec_gob_2020_21,
                      #size=hli,
                      #label = ent_corto
       )
) +
  #Geom_point
  geom_density(fill="#dc5356", color="#dc5356", alpha=0.8)+
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
    #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(labels =scales::dollar
                     #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Utilities per Gained Hour (mexican pesos)",
         y = "Density"
         #,
         #title = "Gráfica 3: Utilidades por hora generadas ",
         #subtitle = "Luego de descontar costos de trabajo y antes del pago de impuestos"
         #caption = "Fuente: Elaborado por @gatitosvsdesig, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
  )


