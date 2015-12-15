## Synopsis

**AgID** is the Public Agency at National level responsible for implementing the Italian Digital Agenda.
The eGovernment Laboratory within Ca' Foscari in collaboration with AgID developed the National Catalogue of PA Databases.
This is a concise data analysis of available datsets in the catalogue related to health and social services.

## Structure

    |
    |---docs : final report (italian language)
    |---export : charts produced by R script 
    |---input : input files (cvs, txt) for R script; 
                query.txt : the original database query
                trasformazioni.json is a script for "open refine"ing the "agid_dati_amministrazioni_sanita_query_result.csv" script
                thus obtaining agid_dati_amministrazioni_sanita[refined].csv
    |---R : various test script, main script is "script_analisi.R", commented in english

## License
CC BY 3.0
https://creativecommons.org/licenses/by/3.0/


