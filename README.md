# RKA_SCB_API_2.0_Interaktion
Kejsar Zings formelsamling (RKAs interaktion med SCB API 2.0. Hämta metadata och data)

## SCB_API_funktionssamling.R
Innehåller en handfull funktioner som underlättar RKAs datahämtning från SCBs API v.2.
En funktion hämtar all tillgänglig metadata för en specifik tabell baseat på TAB-id. 

Funktionen används sedan i scriptet **SCB_API_fetchmetadata.R** som exporterar den 
angivna tabellens metadata till en excelfil. Excelfilen används av
RKAs statistiker kan tillskriva variabelkombinationer unika komponentID.

Excelfilen används i sin tur som underlag för scriptet **SCB_fetchdata.R** som 
hämtar faktiska data och tvättar den för att kunna läsas in i databasen Kolada.

Avslutningsvis finns en hårdkodad character vector som innehåller ID för
regioner, kommuner och riket. Mest för att ha den tillgänglig men sopad under mattan.