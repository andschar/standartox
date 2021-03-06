% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standartox.R
\name{stx_query}
\alias{stx_query}
\title{Retrieve Standartox toxicity values}
\usage{
stx_query(
  cas = NULL,
  concentration_unit = NULL,
  concentration_type = NULL,
  duration = NULL,
  endpoint = c("XX50", "NOEX", "LOEX"),
  effect = NULL,
  exposure = NULL,
  chemical_role = NULL,
  chemical_class = NULL,
  taxa = NULL,
  ecotox_grp = NULL,
  trophic_lvl = NULL,
  habitat = NULL,
  region = NULL,
  vers = NULL,
  ...
)
}
\arguments{
\item{cas}{character, integer; Limit data base query to specific CAS numbers, multiple entries possible (e.g. 1071-83-6, 1071836), NULL (default).}

\item{concentration_unit}{character; Limit data base query to specific concentration units (e.g. ug/l - default).}

\item{concentration_type}{character; Limit data base query to specific concentration types, can be one of NULL (default), 'active ingredient', 'formulation', 'total', 'not reported', 'unionized', 'dissolved', 'labile'. See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.4.}

\item{duration}{integer vector of length two; Limit data base query to specific test durations (hours) (e.g. c(24, 48)). NULL (default).}

\item{endpoint}{character; Choose endypoint type, must be one of 'XX50' (default), 'NOEX', 'LOEX'.}

\item{effect}{character; Limit data base query to specific effect groups, multiple entries possible (e.g. 'Mortality', 'Intoxication', 'Growth'). See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.95. NULL (default).}

\item{exposure}{character; Choose exposure type, (e.g. aquatic, environmental, diet). NULL (default).}

\item{chemical_role}{character; Limit data base query to specific chemical roles (e.g. insecticide), multiple entries possible. NULL (default).}

\item{chemical_class}{character; Limit data base query to specific chemical classes (e.g. neonicotinoid), multiple entries possible. NULL (default).}

\item{taxa}{character; Limit data base query to specific taxa, multiple entries possible. NULL (default).}

\item{ecotox_grp}{character; Convenience grouping of organisms in ecotoxicology, must be one of NULL (default), 'invertebrate', 'fish', 'plant_land', 'macrophyte', 'algae'.}

\item{trophic_lvl}{character; Trophic level of organism, must be one of NULL (default), 'autotroph', 'heterotroph'.}

\item{habitat}{character; Limit data base query to specific organism habitats, can be one of NULL (default) 'marine', 'brackish', 'freshwater', 'terrestrial'.}

\item{region}{character; Limit data base query to organisms occurring in specific regions, can be one of NULL (default) 'africa', 'america_north', 'america_south', 'asia', 'europe', 'oceania'.}

\item{vers}{integer; Choose the version of the EPA Ecotox on which Standartox is based on. NULL (default) accesses the most recent version.}

\item{...}{currently not used}
}
\value{
Returns a list of three data.tables (filtered data base query results, aggregated data base query results, meta information)
}
\description{
Retrieve toxicity values from the Standartox data base \url{http://standartox.uni-landau.de/}.
}
\examples{
\donttest{
# might fail if API is not available
stx_query('1071-83-6')
stx_query(cas = '1071-83-6',
          duration = c(48, 120),
          concentration_unit = 'ug/l')
stx_query(cas = '1071-83-6',
          duration = c(48, 120),
          concentration_unit = 'ug/l',
          endpoint = 'XX50')
}

}
\author{
Andreas Scharmueller \email{andschar@protonmail.com}
}
