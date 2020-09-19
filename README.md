# agromet

El paquete {agromet} permite calcular distintos indices y estadísticos asociados climáticos e hidrológicos. 

Cálculo de índices agroclimáticos nacionales
Se proponen, en primera instancia los siguientes índices, para ser calculados en distintas escalas temporales

INDICES DE PRECIPITACION; Estos tienen como datos de entrada las lluvias en general y las temperaturas y ETP, otros de ellos.; 

*cantidad de días secos; * Deciles; *Porcentaje de precipitación habitual: *Anomalía porcentual; *Índice normalizado de precipitación (SPI); *Índice estandarizado de precipitación y evapotranspiración (SPEI); *Índice de severidad de sequía de Palmer; *Índice autocalibrado de severidad de sequía de Palmer (sc-PDSI);



Podés instalar este paquete corriendo el siguiente comando:

`remotes::install_github("AgRoMeteorologiaINTA/agromet")` 

Las funciones reciben como datos de entrada, los datos disponibles en el [Sistema de Información y Gestión Agrometeorológica - INTA](http://siga.inta.gov.ar/), y permiten incorporarse a un flujo de trabajo mantenible y extensible. 

Los objetos devueltos por cada función tienen un método de visualización rápido usando escalas de colores consistentes y una identidad visual apropiada al INTA. 

Finanlmente el paquete cuenta con una plantilla  de RMarkdown para el reporte de índices y estadísticos implementados generada utilizando una interfaz gráfica. 
Plantilla de reporte con interfaz gráfica (código en repositorio GitHub).
