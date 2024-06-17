library(sfdep)
library(tidyverse)
library(sf)
library(patchwork)
library(httr)
library(ows4R)
library(lwgeom)

if(!file.exists("./data")){dir.create("./data")}

# ==== EXAMPLE 1 OF SPATIAL LAG ====

data(guerry, package = "sfdep")

view(guerry)

st_geometry(guerry) |>
        plot()

gg_crime_obs <- ggplot(guerry, aes(fill = crime_pers)) +
        geom_sf(color = "black", lwd = 0.15) +
        scale_fill_viridis_c(limits = range(guerry$crime_pers)) +
        theme_void()

crime_lags <- guerry |>
        mutate(
                nb = st_contiguity(geometry),
                wt = st_weights(nb), 
                crime_lag = st_lag(crime_pers, nb, wt)
        )

gg_crime_lag <- ggplot(crime_lags, aes(fill = crime_lag)) +
        geom_sf(color = "black", lwd = 0.15) +
        scale_fill_viridis_c(limits = range(guerry$crime_pers)) +
        theme_void()

hist(crime_lags$crime_pers)
hist(crime_lags$crime_lag)
mean(crime_lags$crime_pers)

patchwork::wrap_plots(gg_crime_obs, gg_crime_lag)

# ==== EXAMPLE 2 BOSTON DISTRICTS ====


# ==== EXAMPLE 3 HELSINKI DISTRICTS USING WFS ====

wfslink <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs"

wfsurl <-  parse_url(wfslink)

# --- option 1: get available services using getCapabilities

wfsurl$query <- list(service = "wfs",
                     request = "GetCapabilities"
                     )

request <- build_url(wfsurl)

wClient <- WFSClient$new(wfslink, serviceVersion = "2.0.0") # Note: serviceVersion is mandatory, usually 2.0.0

wClient$getFeatureTypes() %>% map_chr(function(x){x$getName()})

wClient$
        describeFeatureType(typeName = "avoindata:Kaupunginosajako") %>% 
        map_chr(function(x){x$getName()})

# list of all available WFS operations

wClient$
        getCapabilities()$
        getOperationsMetadata()$
        getOperations() %>%
        map_chr(function(x){x$getName()})

# list of all available output formats

wClient$
        getCapabilities()$
        getOperationsMetadata()$
        getOperations() %>%
        map(function(x){x$getParameters()}) %>%
        pluck(3, "outputFormat")

# --- option 2: build sf object of districts

wfsurl$query <- list(service = "wfs",
                     request = "GetFeature",
                     typeName = "avoindata:Kaupunginosajako",
                     srsName = "EPSG:3879")

request <- build_url(wfsurl)

helsinkiDistricts <- read_sf(request)

units::set_units(st_area(helsinkiDistricts),km^2)

st_is_longlat(helsinkiDistricts)
st_set_crs(helsinkiDistricts, 3879)
st_crs(helsinkiDistricts)

helsinkiDistricts <- helsinkiDistricts %>% mutate(area = units::set_units(st_area(helsinkiDistricts), km^2))

# --- draw map to verify

ggplot(helsinkiDistricts) + geom_sf()

# ==== EXAMPLE 4 FINLAND AREAS ====

wfsUrl <- "http://geo.stat.fi/geoserver/vaestoalue/wfs?"


# ==== REFERENCES ====

# 1) Guerry data presentation by Josiah Parry 
# 2) Boston data presentation by Josiah Parry https://www.youtube.com/watch?v=i_MA1U6SJ1Y&t=3351s 
# 3) Using WFS Services in R https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
# 4) EPGS:3879 https://epsg.io/3879
