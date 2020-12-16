rm(list=objects())
library("tidyverse")
library("seplyr")
library("skimr")
library("sf")
library("geojson")
source("ClimateData.R")
source("wmo.R")
source("ClimateObjects.R")


ANNOI<-1991
ANNOF<-2020

PARAMETRO<-"Prcp"
ANNIMANCANTI<-15


#lettura anagrafica
read_delim("reg.aeronautica.info.csv",delim=";",col_names=TRUE)->ana

list.files(path = "./dati",pattern = "^.+txt",full.names = TRUE)->ffile

purrr::map(ffile,.f=function(nomeFile){ 
  
  codice<-str_extract(nomeFile,pattern="[0-9]+")

  read_delim(nomeFile,
             delim=",",
             col_names=TRUE,
             col_types = cols(year=col_integer(),month=col_integer(),day=col_integer(),.default=col_double()))->dati
  
  dati %>%
    seplyr::select_se(c("year","month","day",tolower(PARAMETRO))) %>%
    rename(yy=year,mm=month,dd=day) %>%
    seplyr::rename_se(c("value":=tolower(PARAMETRO)))->dati
  
  dati %>%
    filter(yy>=ANNOI & yy<=ANNOF)->subDati
  
  if(!nrow(subDati)) return(NULL)

  ClimateData(x=subDati,param = tolower(PARAMETRO))->clSerie
  aggregaCD(clSerie,max.na = 0,rle.check = TRUE,max.size.block.na = 0)->clSerieMonthly 
  
  climatologiciMensili(clSerieMonthly,yearS = ANNOI,yearE = ANNOF,max.na=ANNIMANCANTI,rle.check = FALSE,max.size.block.na = ANNIMANCANTI)->clMensili
  aggregaCD(clMensili,ignore.par = FALSE,max.na = 0,rle.check = TRUE,max.size.block.na = 0,seasonal = TRUE)->clAnnuali

  as_tibble(as.matrix(clMensili))->clMensili
  
  if(all(is.na(clMensili))) return(NULL)
  
  as_tibble(as.matrix(clAnnuali))->clAnnuali

  names(clMensili)<-codice
  names(clAnnuali)<-codice
  
  list(clMensili,clAnnuali)
    
})->listaOut

purrr::compact(listaOut)->listaOut


purrr::map_dfc(listaOut,1)->valoriMensili
purrr::map_dfc(listaOut,2)->valoriAnnuali

valoriAnnuali %>%
  mutate(yy=2005) %>%
  dplyr::select(yy,everything()) %>%
  gather(key="SiteID",value="climatologico",-yy) %>%
  mutate(SiteID=as.numeric(SiteID)) %>%
  filter(!is.na(climatologico))->gvaloriAnnuali

left_join(gvaloriAnnuali,ana)->dati

write_delim(dati %>%dplyr::select(-area,-State,-Comments),file=glue::glue("{PARAMETRO}_1991_2020_annuali.csv"),delim=";",col_names = TRUE)

dati %>%
  filter(is.null(SiteCode))->daVerificare

if(nrow(daVerificare)) browser()

st_as_sf(dati,coords=c("Longitude","Latitude"),crs=4326)->puntiStazione
geojson::geo_write(as.geojson(as_Spatial(puntiStazione)),file=glue::glue("{PARAMETRO}_annuali.geojson"))

valoriMensili %>%
  mutate(mm=1:12) %>%
  dplyr::select(mm,everything()) %>%
  gather(key="SiteID",value="climatologico",-mm) %>%
  mutate(SiteID=as.numeric(SiteID))->gvaloriMensili

left_join(gvaloriMensili,ana)->dati
dati %>%
  filter(is.null(SiteCode))->daVerificare

if(nrow(daVerificare)) browser()

purrr::walk(1:12,.f=function(mese){ 
  
  dati %>%
    filter(mm==mese) %>%
    filter(!is.na(climatologico))->subDati
  
  write_delim(subDati %>%dplyr::select(-area,-State,-Comments),file=glue::glue("{PARAMETRO}_{month.abb[mese]}.csv"),delim=";",col_names = TRUE)
  

  st_as_sf(subDati,coords=c("Longitude","Latitude"),crs=4326)->puntiStazione
  geojson::geo_write(as.geojson(as_Spatial(puntiStazione)),file=glue::glue("{PARAMETRO}_{month.abb[mese]}.geojson"))

  
})
