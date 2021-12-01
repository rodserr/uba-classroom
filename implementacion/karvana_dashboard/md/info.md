
![shinylogo](shiny-logo-transparent.png?display=inline-block) Elaborado por [Rodrigo Serrano](https://www.linkedin.com/in/rodrigo-serrano-26143a84/)

Los datos son obtenidos de  [![fbreflogo](fbref-logo-transparent.png?display=inline-block)](https://fbref.com/en/)

Las secciones **Venex** y **Eliminatorias** se actualizan de forma manual, por lo que la información puede no estar al día. Los datos son obtenidos de las paginas de *fbref* utilizando el paquete `rvest` y `worldfootballR`. En el caso de **Venex**, se almacenan en tablas de BigQuery. Para **Eliminatorias** se almacenan en un .csv local debido a su baja frecuencia de actualización (luego de cada juego de eliminatoria).

La aplicación esta conformada por tres secciones:

- **Eliminatorias:** Estadísticas básicas de los jugadores del equipo de fútbol Nacional de Venezuela en la eliminatoria (actualmente solo datos de la eliminatoria al Mundial de Qatar 2022)
- **Venex:** Goles, asistencias y minutos de juego de la temporada en curso (o última temporada jugada) de los futbolistas venezolanos que participan en ligas y torneos fuera de Venezuela. Sólo jugadores activos
- **Scout:** Calendario de los jugadores `venex` en los próximos 7 días. Resumen del desempeño de los jugadores `venex` en partidos jugados en los últimos 7 días

Para mas información visitar: <https://fbref.com/en/country/players/VEN/Venezuela-Football-Players>