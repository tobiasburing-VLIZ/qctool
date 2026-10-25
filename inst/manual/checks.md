# Appendix: the check catalogue

Made from the rules workbook on 2026-10-25 by `dev/write_check_catalogue.R`: 161 checks, 144 implemented.
In the app: Help > Checks (with search). In R: `qc_checks("words")`.

## Files and columns

| Check | Severity | What | Online |
|---|---|---|---|
| **Archive cannot be read** (`archive_readable`) | error | The zip or folder opens, meta.xml is valid and every file it lists exists. |  |
| **Broken characters (text not valid UTF-8)** (`text_encoding_valid`) | warning | Text contains no invalid characters (e.g. '�' or bytes that are not UTF-8), no broken accents (e.g. 'Ã©' instead of 'é') and no HTML codes such as &quot; or &amp;. Text that is not valid UTF-8 (a file saved as Windows-1252, e.g. by Excel) is read as Windows-1252, so the other checks can run, and listed here. |  |
| **Column name is not a Darwin Core term** (`field_names_known`) | warning | Every column is a known Darwin Core, OBIS, DNA or Dublin Core term. Near misses (wrong capitals, typos, 'rank' instead of 'taxonRank') get a suggestion. |  |
| **Column names in the data file not usable** (`file_headers_valid`) | warning | Compares only the first line of each data file with meta.xml, never the data rows, so it costs the same for 100 or 10 million records. Reports (a) files without any header row: the names exist only in meta.xml, and the tool rebuilt them; (b) header names that differ from the Darwin Core term in meta.xml (typo, capitals, own name such as 'Lat'); (c) columns that meta.xml does not map to any term (not harvested). |  |
| **Completely empty column** (`no_empty_columns`) | info | A column is present but has no value in any record. |  |
| **Core is not Event or Occurrence** (`core_type_supported`) | error | The core table is an Event or Occurrence core. |  |
| **Identical rows** (`exact_duplicate_rows`) | warning | Two or more rows are identical in every column except the row id. |  |
| **Occurrence table missing** (`occurrence_table_present`) | error | The archive contains an Occurrence table, as core or as extension. |  |
| **Old MoF used instead of eMoF** (`emof_extension_used`) | error | Measurements use the ExtendedMeasurementOrFact extension, not the older MeasurementOrFact. |  |
| **Space at the start or end of a value** (`no_leading_trailing_spaces`) | warning | Values have no leading or trailing spaces. |  |
| **Tab or line break inside a value** (`no_line_breaks_in_values`) | warning | Values do not contain tab characters or line breaks. |  |
| **Table is empty** (`table_not_empty`) | error | Every table listed in the archive has at least one record. |  |
| **Term in an unexpected table** (`field_in_expected_table`) | warning | Each term sits in a table where EurOBIS expects it (event, occurrence, eMoF or DNA), e.g. samplingProtocol belongs in the event table of an event-core dataset. |  |
| **Text placeholder instead of an empty value** (`no_placeholder_values`) | warning | Values are not placeholders such as NA, NULL, N/A, -, none or ?. |  |
| **Value does not match the data type of the term** (`field_datatype_valid`) | error | Values fit the data type of their term: decimal, integer, ISO 8601 date, URI, IUPAC DNA letters, ... Hints for common causes such as a decimal comma ('1,5'). Fields with their own dedicated check (coordinates, depth, eventDate, ...) are skipped here to avoid double reporting. |  |
| **Value too long for the EurOBIS database** (`field_length_within_eurobis_limit`) | error | No value is longer than the column size in the EurOBIS database (e.g. nvarchar(255) for scientificNameID): error. Values above a lower limit of the current harvester (associatedOccurrences: 350 characters) but within the database size give a warning. |  |

## Identifiers

| Check | Severity | What | Online |
|---|---|---|---|
| **DNA record's occurrenceID not in the Occurrence table** (`dna_occurrenceid_exists`) | error | Event core: every filled occurrenceID in the DNA-derived data exists in the Occurrence table. |  |
| **Events and occurrences share their IDs (one occurrence per event)** (`eventid_equals_occurrenceid`) | info | Reports when each event has one single occurrence and eventID equals occurrenceID. |  |
| **Loop in the event hierarchy** (`event_hierarchy_no_loops`) | error | No event is its own parent or its own ancestor (A > B > A). |  |
| **Record points to an event or occurrence that does not exist** (`extension_links_to_core`) | error | Every occurrence, measurement and DNA record points, through the archive's id column (core link), to an existing event (event core) or occurrence (occurrence core). |  |
| **eMoF event differs from the occurrence's event** (`emof_event_matches_occurrence`) | error | For eMoF records with an occurrenceID, the linked event is the same as the event of that occurrence. |  |
| **eMoF occurrenceID not in the Occurrence table** (`emof_occurrenceid_exists`) | error | Event core: every filled occurrenceID in the eMoF exists in the Occurrence table. |  |
| **eMoF record not linked** (`emof_record_linked`) | error | Every eMoF record is linked to an event and/or an occurrence. |  |
| **eventID names another or a missing event** (`extension_eventid_matches_coreid`) | error | Event core: an extension's own eventID column names an existing event, the same one the record is linked to through the id column. |  |
| **eventID used more than once** (`eventid_unique`) | error | No two events share the same eventID (without an eventID column: the same id). |  |
| **measurementID used more than once** (`measurementid_unique`) | warning | If measurementID is used, no two measurements share it. |  |
| **occurrenceID may not be unique outside this dataset** (`occurrenceid_globally_unique`) | off | occurrenceIDs are not just running numbers such as 1, 2, 3 or very short codes. |  |
| **occurrenceID names another or a missing occurrence** (`extension_occurrenceid_matches_coreid`) | error | Occurrence core: an extension's own occurrenceID column (eMoF, DNA) names an existing occurrence, the same one the record is linked to through the id column. |  |
| **occurrenceID used more than once** (`occurrenceid_unique`) | error | No two occurrences share the same occurrenceID. |  |
| **parentEventID points to a missing event** (`parenteventid_exists`) | error | Every filled parentEventID exists as an eventID (without an eventID column: as an id of the event core). |  |

## Required fields

| Check | Severity | What | Online |
|---|---|---|---|
| **Missing value found in another field** (`missing_values_found_elsewhere`) | warning | When eventDate, coordinates or depth are empty, looks for them in eventRemarks, verbatimEventDate, verbatimLocality, locality and dynamicProperties (e.g. 'Jan2008' in eventRemarks, 'collection date: 2013-06-04' in dynamicProperties) and shows what was found. |  |
| **More than one datasetName** (`datasetname_single_value`) | warning | datasetName has only one distinct value in the whole dataset. |  |
| **No presence records** (`occurrencestatus_has_presence`) | warning | At least one occurrence has occurrenceStatus 'present'. |  |
| **Recommended field missing or empty** (`recommended_fields`) | warning | Fields recommended by the profile are present and filled. |  |
| **Required column missing** (`required_fields_present`) | error | Every column required by the chosen profile exists, in its own table or inherited from a parent event. |  |
| **Required value empty** (`required_fields_filled`) | error | Required fields have a value in every record, counting values inherited from parent events. |  |
| **datasetName differs from the dataset title** (`datasetname_equals_title`) | error | datasetName is the same as the title in the metadata (EML). Small differences are shown with a similarity score. |  |
| **dcterms:modified missing or empty** (`modified_filled`) | info | The date a record was last changed (dcterms:modified) is given in the event and occurrence tables that have the column; a dataset without the column gets one issue. |  |

## Vocabularies

| Check | Severity | What | Online |
|---|---|---|---|
| **basisOfRecord is not a standard term** (`basisofrecord_valid`) | error | basisOfRecord is one of HumanObservation, MachineObservation, MaterialSample, PreservedSpecimen, LivingSpecimen, FossilSpecimen, MaterialCitation, Occurrence. Other spellings get a hint (e.g. 'MATERIAL_SAMPLE' or 'materialsample' -> 'MaterialSample'). |  |
| **countryCode is not ISO 3166-1 alpha-2** (`countrycode_valid`) | warning | countryCode is a valid two-letter ISO country code. |  |
| **establishmentMeans, degreeOfEstablishment or pathway not valid** (`establishment_terms_valid`) | warning | These fields use the GBIF vocabularies. |  |
| **geodeticDatum is not valid** (`geodeticdatum_valid`) | warning | geodeticDatum is an EPSG code or a recognised name such as WGS84. |  |
| **lifeStage is not from the controlled vocabulary** (`lifestage_valid`) | warning | lifeStage is a term from the GBIF LifeStage vocabulary (adult, juvenile, larva, egg, ...). |  |
| **occurrenceStatus is not an accepted term** (`occurrencestatus_valid`) | error | occurrenceStatus is 'present' or 'absent' (lower case). The EMODnet profile also accepts 'detected' and 'notDetected'. Accepted terms per profile are in the vocabulary file. |  |
| **sex is not from the controlled vocabulary** (`sex_valid`) | warning | sex is a term from the GBIF Sex vocabulary (female, male, hermaphrodite). |  |
| **taxonRank is not a standard rank** (`taxonrank_valid`) | warning | taxonRank is a standard rank name (species, genus, family, ...). Other spellings get a hint (e.g. 'super kingdom' -> 'superkingdom'). |  |
| **type is not a DCMI type term** (`dctype_valid`) | warning | dcterms:type is a DCMI Type term (Event, PhysicalObject, StillImage, ...). For event kinds such as cruise, station or sample, use eventType. |  |

## Dates

| Check | Severity | What | Online |
|---|---|---|---|
| **Date outside the metadata time span** (`dates_within_eml_coverage`) | warning | All eventDates fall within the temporal coverage in the EML. Also reports when the coverage is much wider than the data. |  |
| **Event date outside the parent event's date** (`child_date_within_parent`) | warning | The eventDate of an event falls within the eventDate of its parent event. |  |
| **Start date after end date** (`eventdate_interval_order`) | error | In a date interval (start/end) the start is not after the end. |  |
| **dateIdentified not valid** (`dateidentified_valid`) *not built yet* | warning | dateIdentified follows ISO 8601 and is not before eventDate. |  |
| **eventDate does not follow ISO 8601** (`eventdate_iso8601`) | error | eventDate is a valid ISO 8601 date, date-time or interval: 2019, 2019-05, 2019-05-21, 2019-05-21T14:30Z, 2019-05-21/2019-05-25, 2019-05-21/25 and similar. Inherited dates are checked too. |  |
| **eventDate in a form EurOBIS cannot read** (`eventdate_eurobis_compatible`) | error | Valid ISO 8601 forms that the EurOBIS harvester cannot read yet, such as the shortened interval '2011-07/09' (error). Suggests the full form '2011-07/2011-09'. An interval whose end is only a time ('2019-05-21T10:00/12:00') is a warning. |  |
| **eventDate in the future** (`eventdate_not_future`) | error | No eventDate (or interval end) lies after today. |  |
| **eventDate suspiciously old** (`eventdate_not_suspiciously_old`) | warning | No eventDate lies before a threshold year. |  |
| **eventTime does not follow ISO 8601** (`eventtime_valid`) | warning | eventTime is a valid ISO 8601 time, e.g. 14:30, 14:30:00Z or 14:30+01:00. |  |
| **startDayOfYear or endDayOfYear not valid** (`dayofyear_valid`) *not built yet* | warning | Values are 1-366 and agree with eventDate. |  |
| **year, month or day differ from eventDate** (`year_month_day_match_eventdate`) | warning | If year, month or day are filled, they agree with eventDate. |  |
| **year, month or day not valid** (`year_month_day_valid`) | warning | year is a 4-digit year not in the future, month is 1-12, day is 1-31 and exists in that month. |  |

## Location

| Check | Severity | What | Online |
|---|---|---|---|
| **Coordinates are 0,0** (`coordinates_not_zero`) | warning | Latitude and longitude are not both 0. |  |
| **Coordinates are not numbers** (`coordinates_numeric`) | error | decimalLatitude and decimalLongitude contain only numbers with a dot as decimal separator. Hints for common causes: decimal comma, degree signs, N/S/E/W letters. |  |
| **Coordinates out of range** (`coordinates_in_range`) | error | Latitude is between -90 and 90, longitude between -180 and 180. |  |
| **Latitude and longitude possibly swapped** (`coordinates_swapped`) | warning | Hint when latitude is outside -90 to 90 but longitude is inside, or when swapping puts the point inside the metadata area. |  |
| **Longitude in 0-360 notation** (`longitude_0_360`) | error | Longitudes between 180 and 360 look like 0-360 notation. The message suggests longitude minus 360. |  |
| **Marine-only taxon on land** (`marine_taxon_on_land`) | error | Records on land of taxa that WoRMS lists as marine only (not brackish, freshwater or terrestrial). Shown on the map. | OBIS (coast; offline: coarse coastline) + WoRMS |
| **Position on land** (`points_on_land`) | warning | The position lies on land, further inland than a 3 km coastal buffer. Flag only: records from estuaries, rivers or lagoons can be correct, so they are shown on the map for a person to judge. | OBIS (coast; offline: coarse coastline) |
| **Position outside the metadata area** (`coordinates_within_eml_area`) | warning | Points fall inside the geographic coverage (bounding box) in the EML. |  |
| **Unrealistic number of decimals** (`coordinates_precision`) | warning | Coordinates have at most 7 decimals (about 1 cm). More usually comes from a conversion, e.g. 43.558590000000002 from Excel. |  |
| **coordinateUncertaintyInMeters is not a positive number** (`coordinateuncertainty_valid`) | warning | Value is numeric, greater than 0 and at most 20,037,509 m (half the Earth's circumference). Inherited values are checked too. |  |
| **country or countryCode does not match the position** (`country_matches_coordinates`) *not built yet* | warning | country/countryCode agree with the position, using EEZ boundaries from Marine Regions. | web |
| **county information repeated in locality** (`county_and_locality`) *not built yet* | info | When county is filled, locality is filled too and does not contradict it. |  |
| **footprintWKT invalid or does not contain the point** (`footprintwkt_valid`) *not built yet* | warning | footprintWKT is valid WKT and decimalLatitude/decimalLongitude lies inside or close to it. |  |
| **stateProvince, county and locality inconsistent** (`locality_consistent`) *not built yet* | off | These fields agree with the coordinates. | web |
| **waterBody invalid or does not match the position** (`waterbody_valid`) *not built yet* | warning | waterBody uses Marine Regions names and the position lies in that water body. | web |

## Depth

| Check | Severity | What | Online |
|---|---|---|---|
| **Depth implausible for the location** (`depth_vs_bathymetry`) | error | The depth (maximumDepthInMeters, or minimumDepthInMeters when no maximum is given) is not deeper than the sea floor at that position plus a margin. | OBIS (sea floor depth) |
| **Depth is not a number** (`depth_numeric`) | error | minimumDepthInMeters and maximumDepthInMeters are numeric. Hints when units ('10 m') or ranges ('10-20') are typed in. |  |
| **Depth outside 0 to 11,000 m** (`depth_in_range`) | error | Depth is not negative and not deeper than the deepest point of the ocean. |  |
| **Elevation not numeric, or minimum above maximum** (`elevation_valid`) | warning | minimum/maximumElevationInMeters are numeric and minimum is not greater than maximum. |  |
| **Minimum depth greater than maximum depth** (`depth_min_le_max`) | error | minimumDepthInMeters is less than or equal to maximumDepthInMeters. |  |

## Taxonomy

| Check | Severity | What | Online |
|---|---|---|---|
| **AphiaID is not the accepted name** (`aphiaid_accepted`) | warning | Unaccepted, deleted or quarantined AphiaIDs are reported together with the accepted name. | WoRMS |
| **AphiaID not found in WoRMS** (`scientificnameid_exists_in_worms`) | error | The AphiaID in scientificNameID exists in WoRMS. | WoRMS |
| **Higher classification differs from WoRMS** (`classification_matches_worms`) *not built yet* | warning | kingdom, phylum, class, order, family and genus in the data agree with WoRMS for that AphiaID. | WoRMS |
| **Name without a WoRMS ID (ID suggested)** (`names_without_id_matched`) | info | For records without scientificNameID, looks up the name in WoRMS and suggests a match. | WoRMS |
| **Not identified to family level or lower** (`taxon_identified_to_family_or_lower`) | warning | Records identified only above family level (class, phylum, kingdom) are reported. | WoRMS |
| **Taxon is not marine or brackish** (`taxon_is_marine`) | warning | Reports taxa that WoRMS lists as freshwater-only or terrestrial-only. | WoRMS |
| **scientificName and scientificNameID do not go one-to-one** (`scientificname_id_one_to_one`) | error | Each scientificName has one scientificNameID and each scientificNameID one scientificName, in the whole dataset. Records with a name but no ID (or an ID but no name) are reported when other records of that name (ID) have one, with it as suggestion. |  |
| **scientificName contains more than the name** (`scientificname_clean`) | warning | scientificName holds no qualifiers (cf., aff., sp., spp., ?) and no extra words. Use identificationQualifier and scientificNameAuthorship for these. For DNA data also: underscores ('Candidatus_Nitrosopelagicus'), 'uncultured', 'unculturable', 'environmental sample', 'metagenome'. |  |
| **scientificName does not match the AphiaID** (`scientificname_matches_aphiaid`) | error | scientificName equals the WoRMS name of that AphiaID, ignoring authorship. Small differences (up to 5 characters) give a warning, larger ones an error. | WoRMS |
| **scientificNameID is not a WoRMS LSID** (`scientificnameid_worms_format`) | error | scientificNameID has the form urn:lsid:marinespecies.org:taxname:<number>. |  |

## Measurements (eMoF)

| Check | Severity | What | Online |
|---|---|---|---|
| **Count given twice (individualCount and eMoF)** (`individualcount_also_in_emof`) | warning | An occurrence does not have both individualCount and an eMoF count (OCOUNT01) with different values. |  |
| **Counts without a sample size** (`sample_size_with_counts`) *not built yet* | warning | When counts or abundances are given, a sample size or effort is also given. |  |
| **Deprecated vocabulary term** (`vocabulary_terms_not_deprecated`) | warning | measurementTypeID, measurementUnitID and measurementValueID are not deprecated. Suggests the replacement term. | NERC Vocabulary Server |
| **Fact without a value ID, or measurement without a unit** (`value_has_id_or_unit`) | warning | Non-numeric values (e.g. 'adult', 'grab') need a measurementValueID; numeric values need a measurementUnit. |  |
| **Implausible measurement value** (`measurement_value_plausible`) *not built yet* | warning | No negative counts, weights or lengths, and no percentages above 100. |  |
| **Measurement belongs in a Darwin Core field** (`emof_type_has_dwc_field`) | warning | eMoF types that have their own Darwin Core field (scientific name, sampling protocol, depth, ...) should use that field instead. |  |
| **Positive abundance for an absent taxon** (`positive_quantity_with_absence`) | warning | An occurrence with occurrenceStatus 'absent' has no quantity greater than 0. |  |
| **Sampling effort not described** (`sampling_effort_present`) | warning | The dataset describes sampling effort: an effort or sampling-descriptor term in the eMoF, or sampleSizeValue/sampleSizeUnit, samplingEffort or samplingProtocol. The result lists which of these places are used. |  |
| **Sampling instrument not described** (`sampling_instrument_present`) | warning | The eMoF contains a sampling instrument term (e.g. NMSPINST with an L22 value). |  |
| **Several units for one measurement type** (`unit_consistent_per_type`) | warning | The same measurementTypeID (or measurementType) uses one unit throughout the dataset. |  |
| **Unit does not fit the measurement type** (`type_unit_compatible`) | error | measurementUnitID is one of the units that NERC links to that P01 measurementTypeID. | NERC Vocabulary Server |
| **Unit text does not match the unit ID** (`unit_text_matches_unitid`) | error | measurementUnit (text) means the same as the NERC label of measurementUnitID, e.g. not 'ind L-1' with an ID for 'per cubic metre'. | NERC Vocabulary Server |
| **Value and value ID do not go one-to-one** (`measurementvalueid_consistent`) | warning | Within a measurement type, the same measurementValue always has the same measurementValueID, and the same measurementValueID the same measurementValue. Measured numbers have no measurementValueID: one ID for different numbers is reported. |  |
| **Value not numeric although a unit is given** (`value_numeric_when_unit`) | warning | When measurementUnit is filled, measurementValue is a number. |  |
| **Vocabulary URI malformed** (`nvs_uri_format`) | warning | NERC URIs have the form http://vocab.nerc.ac.uk/collection/<collection>/current/<code>/ : http (not https), '/current/', trailing slash, no spaces. |  |
| **Zero abundance or biomass for a present taxon** (`zero_quantity_with_presence`) | warning | An occurrence with occurrenceStatus 'present' has no quantity (abundance, biomass, count, individualCount, organismQuantity) equal to 0. |  |
| **measurementType and measurementTypeID do not go one-to-one** (`measurementtype_id_one_to_one`) | error | Each measurementType text goes with one measurementTypeID, and each measurementTypeID with one measurementType text, in the whole dataset. |  |
| **measurementType text does not fit the measurementTypeID** (`measurementtype_matches_typeid`) | info | The measurementType text is similar to the NERC label of the ID. Also catches typos such as 'Weigth'. | NERC Vocabulary Server |
| **measurementTypeID empty** (`measurementtypeid_filled`) | warning | Every measurementType has a measurementTypeID (NERC P01 or another vocabulary). Occurrence-linked and event-linked records are reported in one list. |  |
| **measurementTypeID not found in NERC** (`measurementtypeid_valid`) | error | measurementTypeID is an existing term in the NERC Vocabulary Server. | NERC Vocabulary Server |
| **measurementUnit and measurementUnitID do not go one-to-one** (`measurementunit_id_one_to_one`) | error | Each measurementUnit text goes with one measurementUnitID, and each measurementUnitID with one measurementUnit text, in the whole dataset. |  |
| **measurementUnitID empty although a unit is given** (`measurementunitid_filled`) | warning | Every filled measurementUnit has a measurementUnitID (NERC P06). |  |
| **measurementUnitID not found in NERC P06** (`measurementunitid_valid`) | error | measurementUnitID is an existing P06 unit. | NERC Vocabulary Server |
| **measurementValueID not found in NERC** (`measurementvalueid_valid`) | error | measurementValueID is an existing NERC term. | NERC Vocabulary Server |
| **organismQuantity not used as EMODnet expects** (`organismquantity_use`) | warning | Non-DNA datasets: organismQuantity/organismQuantityType should be given as eMoF instead. DNA datasets: organismQuantityType and sampleSizeUnit must be 'DNA sequence reads'. |  |

## Duplicates

| Check | Severity | What | Online |
|---|---|---|---|
| **Duplicate eMoF records** (`duplicate_emof_records`) | warning | Two eMoF records with the same link, type, typeID, value, valueID and unit. |  |
| **Potential duplicate occurrences** (`duplicate_occurrences`) | warning | Occurrences that are identical in all chosen fields (coordinates, date, depth, event, lifeStage, sex, samplingProtocol, name, ID, identificationQualifier, identificationRemarks, plus inherited event fields and selected eMoF values). Fields that are missing are skipped. |  |
| **Same measurement type twice for one event** (`duplicate_type_per_event`) | warning | One event (for measurements without occurrenceID) has the same measurementType or measurementTypeID more than once. |  |
| **Same measurement type twice for one occurrence** (`duplicate_type_per_occurrence`) | warning | One occurrence has the same measurementType or measurementTypeID more than once. |  |

## Other fields

| Check | Severity | What | Online |
|---|---|---|---|
| **Other values and their IDs do not go one-to-one** (`values_and_ids_one_to_one`) | warning | The pairs of fields in reference/one_to_one_pairs.csv go one-to-one in every table that has both: institutionCode and institutionID, collectionCode and collectionID, datasetName and datasetID, recordedBy and recordedByID, identifiedBy and identifiedByID, country and countryCode, higherGeography and higherGeographyID, scientificName and taxonID. Pairs marked 'one way' only need one value per ID: locationID and locality, organismID and scientificName, and in an occurrence core eventID and eventDate / decimalLatitude / decimalLongitude. |  |
| **dynamicProperties is not valid JSON** (`dynamicproperties_json`) | warning | If dynamicProperties is filled, it is valid JSON. |  |
| **individualCount is not a whole number** (`individualcount_valid`) | warning | individualCount is a whole number of 0 or more. |  |
| **organismQuantity without a type** (`organismquantity_with_type`) | warning | organismQuantity and organismQuantityType are either both filled or both empty. |  |

## DNA

| Check | Severity | What | Online |
|---|---|---|---|
| **DNA-derived data extension missing** (`dna_extension_present`) | error | When records look DNA-derived (organismQuantityType 'DNA sequence reads', associatedSequences, ENA/MGnify sample links, DNA-type identificationRemarks), the archive has a DNA-derived data extension. |  |
| **DNA-derived data incomplete** (`dna_fields_complete`) | error | For DNA-derived datasets: the fields of Manual V5 table 1 are present and filled. Required: target_gene; for metabarcoding also associatedSequences. Highly recommended: DNA_sequence, pcr_primer_forward/reverse, pcr_primer_name_forward/reverse, pcr_primer_reference, seq_meth, target_subfragment, organismQuantity/Type, sampleSizeValue/Unit, identificationReferences/Remarks, taxonConceptID, materialSampleID, otu_class_appr, otu_db. |  |
| **DNA_sequence contains other than nucleotide codes** (`dna_sequence_valid`) | warning | DNA_sequence contains only IUPAC nucleotide codes (A, C, G, T, U, R, Y, S, W, K, M, B, D, H, V, N), no spaces or other characters. |  |
| **target_gene is not a known marker name** (`dna_target_gene_known`) *not built yet* | warning | target_gene can be matched to a standard gene name (16S rRNA, 18S rRNA, COI, ITS, 12S rRNA, ...). Variants get a suggestion. |  |

## Outliers

| Check | Severity | What | Online |
|---|---|---|---|
| **Date far outside the dataset's sampling period** (`outlier_date_dataset`) | warning | Same method as for positions, on the event dates: flags dates far outside the period of the middle 95% of sampling dates (widened by half its length, at least 2 years). |  |
| **Depth far outside the dataset's depth range** (`outlier_depth_dataset`) | warning | Same method on depth: flags depths far outside the range of the middle 95% of depths (widened by half its size, at least 50 m). |  |
| **Length or weight above the known maximum for the species** (`outlier_size_reference`) *not built yet* | warning | Lengths and weights larger than the known maximum size of the species (WoRMS body-size attributes, FishBase/SeaLifeBase). | web |
| **Position far outside the dataset's sampling area** (`outlier_location_dataset`) | warning | Takes the area covered by the middle 95% of the sampling positions (2.5th to 97.5th percentile of latitude and of longitude), widens it by half its size on each side (at least 1 degree) and flags positions outside it. Only clearly distant points are flagged, not simply the outer 5%. Works across the 180° meridian. |  |
| **Species far from its other records** (`outlier_species_location`) | warning | Per species: flags positions far from where the same species was found elsewhere in this dataset (distance to the species' median position beyond the 95% range of distances, widened by half). Needs at least 10 positions for that species. |  |
| **Unusual length or weight for that species** (`outlier_size_per_species`) | warning | Per species and per length or weight type and unit: flags values far from the other values of the same species in this dataset (more than 3 interquartile ranges away on a log scale). Needs at least 10 values for that species. |  |
| **Unusual measurement value** (`outlier_measurement_value`) | warning | Per measurement type and unit: flags values far outside the middle 50% of the values of that type (more than 3 interquartile ranges away; on a log scale for counts, weights, lengths and other positive quantities). Needs at least 20 values of that type. |  |

## Metadata (EML)

| Check | Severity | What | Online |
|---|---|---|---|
| **Abstract missing** (`eml_abstract_filled`) | error | The metadata has an abstract. |  |
| **Abstract not informative** (`eml_abstract_informative`) | warning | The abstract has at least 50 words. |  |
| **Data creator missing** (`eml_creator_filled`) | error | There is at least one data creator (person or institute). |  |
| **Dataset citation missing** (`eml_citation_filled`) | error | The metadata has a dataset citation. |  |
| **Dataset title missing** (`eml_title_filled`) | error | The metadata has a dataset title. |  |
| **Geographic coverage missing** (`eml_geographic_coverage_filled`) | error | The metadata has a geographic coverage with a bounding box. |  |
| **IPT resource URL longer than 255 characters** (`ipt_url_length`) | error | The IPT resource URL (alternate identifier) has at most 255 characters. |  |
| **Institutes without an identifier** (`eml_organisation_identifier`) | info | Institutes in the metadata have an identifier (EDMO code or ROR). |  |
| **Language not given** (`eml_language_given`) | info | The metadata states the language of the dataset and of the metadata. |  |
| **Licence in the data differs from the metadata** (`license_data_matches_eml`) | warning | If the data has a license column, it matches the licence in the EML. |  |
| **Licence missing, or not CC0, CC-BY or CC-BY-NC** (`eml_license_valid`) | error | The metadata has a machine-readable licence: CC0, CC-BY or CC-BY-NC. |  |
| **Link in the metadata does not work** (`eml_links_work`) *not built yet* | warning | URLs in the metadata respond (no dead links). | web |
| **Metadata file missing** (`eml_present`) | error | The archive contains an eml.xml metadata file. |  |
| **Metadata provider missing or without e-mail** (`eml_metadata_provider_filled`) | error | There is a metadata provider with name, institute and email. |  |
| **Methods not described** (`eml_methods_described`) | warning | The metadata describes how the data were collected (methods, sampling description, study extent). |  |
| **No persistent identifier (DOI)** (`eml_persistent_identifier`) | warning | The metadata has a DOI or another persistent identifier for the dataset. |  |
| **People without an ORCID** (`eml_people_have_orcid`) | warning | Creator, contact and metadata provider have an ORCID (userId). |  |
| **Placeholder text in the metadata** (`eml_no_placeholders`) | warning | Metadata fields do not contain placeholders like '-----', 'TBD', 'xxx' or 'In press'. |  |
| **Point of contact missing or without e-mail** (`eml_contact_filled`) | error | There is a contact with name, institute and a valid email address. |  |
| **Taxonomic coverage missing** (`eml_taxonomic_coverage_filled`) | error | The metadata has a taxonomic coverage. |  |
| **Temporal coverage missing** (`eml_temporal_coverage_filled`) | error | The metadata has a temporal coverage. |  |
| **Themes and keywords missing** (`eml_keywords_filled`) | error | The metadata has keywords / themes. |  |
| **Title not informative** (`eml_title_informative`) | warning | The title has at least 5 words, is not just the IPT short name or a code, and is not in capitals only. Ideally it says what, where and when. |  |
| **license is not an accepted licence** (`license_field_valid`) | error | If the data has a license column, it is CC0, CC-BY or CC-BY-NC as a URL or standard name. |  |

## Versions

| Check | Severity | What | Online |
|---|---|---|---|
| **IDs changed between versions** (`ids_stable_between_versions`) *not built yet* | warning | eventIDs and occurrenceIDs of the previous version still exist. | previous run (history) |
| **Record count dropped** (`record_count_not_decreased`) *not built yet* | warning | Compared with the previous checked version, the number of events, occurrences or eMoF records did not go down. | previous run (history) |

## Out of scope

| Check | Severity | What | Online |
|---|---|---|---|
| **Data provider display name missing** (`provider_display_name`) *not built yet* | off | Checks the ' - Version ' suffix of displayName in the dataproviders table. | EurOBIS database |
| **Duplicate IDs in the EurOBIS harvest** (`duplicate_ids_in_eurobis`) *not built yet* | off | Detects duplicate occurrenceIDs and eventIDs across eurobis_harvest and dwc_harvester_que. | EurOBIS database |

## Where each check lives

For every check that is built: the file with its code (folder `R/` of the tool, function `check_<check_id>()`),
its settings (column *settings* of the sheet *Checks* in the rules workbook; change the numbers there) and the
lists it reads. Lists in `reference/` are the CSV files in `inst/reference/` (manual, section 7.2).

### Files and columns

| Check | Code | Settings | Values from |
|---|---|---|---|
| Archive cannot be read (`archive_readable`) | `R/checks_01_structure.R` |  |  |
| Broken characters (text not valid UTF-8) (`text_encoding_valid`) | `R/checks_01_structure.R` |  |  |
| Column name is not a Darwin Core term (`field_names_known`) | `R/checks_01_structure.R` |  | reference/dwc_terms.csv, reference/term_synonyms.csv |
| Column names in the data file not usable (`file_headers_valid`) | `R/checks_01_structure.R` |  |  |
| Completely empty column (`no_empty_columns`) | `R/checks_01_structure.R` |  |  |
| Core is not Event or Occurrence (`core_type_supported`) | `R/checks_01_structure.R` |  |  |
| Identical rows (`exact_duplicate_rows`) | `R/checks_01_structure.R` |  |  |
| Occurrence table missing (`occurrence_table_present`) | `R/checks_01_structure.R` |  |  |
| Old MoF used instead of eMoF (`emof_extension_used`) | `R/checks_01_structure.R` |  |  |
| Space at the start or end of a value (`no_leading_trailing_spaces`) | `R/checks_01_structure.R` |  |  |
| Tab or line break inside a value (`no_line_breaks_in_values`) | `R/checks_01_structure.R` |  |  |
| Table is empty (`table_not_empty`) | `R/checks_01_structure.R` |  |  |
| Term in an unexpected table (`field_in_expected_table`) | `R/checks_01_structure.R` |  | reference/dwc_terms.csv |
| Text placeholder instead of an empty value (`no_placeholder_values`) | `R/checks_01_structure.R` |  | reference/placeholders.csv |
| Value does not match the data type of the term (`field_datatype_valid`) | `R/checks_01_structure.R` |  | reference/dwc_terms.csv |
| Value too long for the EurOBIS database (`field_length_within_eurobis_limit`) | `R/checks_01_structure.R` |  | rules workbook, sheet 'Field lengths' |

### Identifiers

| Check | Code | Settings | Values from |
|---|---|---|---|
| DNA record's occurrenceID not in the Occurrence table (`dna_occurrenceid_exists`) | `R/checks_02_identifiers.R` |  |  |
| Events and occurrences share their IDs (one occurrence per event) (`eventid_equals_occurrenceid`) | `R/checks_02_identifiers.R` |  |  |
| Loop in the event hierarchy (`event_hierarchy_no_loops`) | `R/checks_02_identifiers.R` |  |  |
| Record points to an event or occurrence that does not exist (`extension_links_to_core`) | `R/checks_02_identifiers.R` |  |  |
| eMoF event differs from the occurrence's event (`emof_event_matches_occurrence`) | `R/checks_02_identifiers.R` |  |  |
| eMoF occurrenceID not in the Occurrence table (`emof_occurrenceid_exists`) | `R/checks_02_identifiers.R` |  |  |
| eMoF record not linked (`emof_record_linked`) | `R/checks_02_identifiers.R` |  |  |
| eventID names another or a missing event (`extension_eventid_matches_coreid`) | `R/checks_02_identifiers.R` |  |  |
| eventID used more than once (`eventid_unique`) | `R/checks_02_identifiers.R` |  |  |
| measurementID used more than once (`measurementid_unique`) | `R/checks_02_identifiers.R` |  |  |
| occurrenceID may not be unique outside this dataset (`occurrenceid_globally_unique`) | `R/checks_02_identifiers.R` | `min_length = 6` |  |
| occurrenceID names another or a missing occurrence (`extension_occurrenceid_matches_coreid`) | `R/checks_02_identifiers.R` |  |  |
| occurrenceID used more than once (`occurrenceid_unique`) | `R/checks_02_identifiers.R` |  |  |
| parentEventID points to a missing event (`parenteventid_exists`) | `R/checks_02_identifiers.R` |  |  |

### Required fields

| Check | Code | Settings | Values from |
|---|---|---|---|
| Missing value found in another field (`missing_values_found_elsewhere`) | `R/checks_03_required.R` |  |  |
| More than one datasetName (`datasetname_single_value`) | `R/checks_03_required.R` |  |  |
| No presence records (`occurrencestatus_has_presence`) | `R/checks_03_required.R` |  |  |
| Recommended field missing or empty (`recommended_fields`) | `R/checks_03_required.R` |  | rules workbook, sheet 'Fields' |
| Required column missing (`required_fields_present`) | `R/checks_03_required.R` |  | rules workbook, sheet 'Fields'; reference/inheritable_fields.csv |
| Required value empty (`required_fields_filled`) | `R/checks_03_required.R` |  | rules workbook, sheet 'Fields'; reference/inheritable_fields.csv |
| datasetName differs from the dataset title (`datasetname_equals_title`) | `R/checks_03_required.R` |  |  |
| dcterms:modified missing or empty (`modified_filled`) | `R/checks_03_required.R` |  |  |

### Vocabularies

| Check | Code | Settings | Values from |
|---|---|---|---|
| basisOfRecord is not a standard term (`basisofrecord_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/basisOfRecord.csv |
| countryCode is not ISO 3166-1 alpha-2 (`countrycode_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/countryCode.csv |
| establishmentMeans, degreeOfEstablishment or pathway not valid (`establishment_terms_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/establishmentMeans.csv, degreeOfEstablishment.csv, pathway.csv |
| geodeticDatum is not valid (`geodeticdatum_valid`) | `R/checks_04_vocabulary.R` |  |  |
| lifeStage is not from the controlled vocabulary (`lifestage_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/lifeStage.csv |
| occurrenceStatus is not an accepted term (`occurrencestatus_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/occurrenceStatus.csv |
| sex is not from the controlled vocabulary (`sex_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/sex.csv |
| taxonRank is not a standard rank (`taxonrank_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/taxonRank.csv |
| type is not a DCMI type term (`dctype_valid`) | `R/checks_04_vocabulary.R` |  | reference/vocabularies/dcType.csv |

### Dates

| Check | Code | Settings | Values from |
|---|---|---|---|
| Date outside the metadata time span (`dates_within_eml_coverage`) | `R/checks_05_dates.R` | `coverage_wider_days = 365` |  |
| Event date outside the parent event's date (`child_date_within_parent`) | `R/checks_05_dates.R` |  |  |
| Start date after end date (`eventdate_interval_order`) | `R/checks_05_dates.R` |  |  |
| eventDate does not follow ISO 8601 (`eventdate_iso8601`) | `R/checks_05_dates.R` |  |  |
| eventDate in a form EurOBIS cannot read (`eventdate_eurobis_compatible`) | `R/checks_05_dates.R` |  |  |
| eventDate in the future (`eventdate_not_future`) | `R/checks_05_dates.R` |  |  |
| eventDate suspiciously old (`eventdate_not_suspiciously_old`) | `R/checks_05_dates.R` | `min_year = 1600; suspicious_dates = 1900-01-01, 1970-01-01` |  |
| eventTime does not follow ISO 8601 (`eventtime_valid`) | `R/checks_05_dates.R` |  |  |
| year, month or day differ from eventDate (`year_month_day_match_eventdate`) | `R/checks_05_dates.R` |  |  |
| year, month or day not valid (`year_month_day_valid`) | `R/checks_05_dates.R` |  |  |

### Location

| Check | Code | Settings | Values from |
|---|---|---|---|
| Coordinates are 0,0 (`coordinates_not_zero`) | `R/checks_06_location.R` |  |  |
| Coordinates are not numbers (`coordinates_numeric`) | `R/checks_06_location.R` |  |  |
| Coordinates out of range (`coordinates_in_range`) | `R/checks_06_location.R` |  |  |
| Latitude and longitude possibly swapped (`coordinates_swapped`) | `R/checks_06_location.R` |  |  |
| Longitude in 0-360 notation (`longitude_0_360`) | `R/checks_06_location.R` |  |  |
| Marine-only taxon on land (`marine_taxon_on_land`) | `R/checks_06_location.R` | `buffer_m = 3000` |  |
| Position on land (`points_on_land`) | `R/checks_06_location.R` | `buffer_m = 3000` |  |
| Position outside the metadata area (`coordinates_within_eml_area`) | `R/checks_06_location.R` |  |  |
| Unrealistic number of decimals (`coordinates_precision`) | `R/checks_06_location.R` | `max_decimals = 7` |  |
| coordinateUncertaintyInMeters is not a positive number (`coordinateuncertainty_valid`) | `R/checks_06_location.R` |  |  |

### Depth

| Check | Code | Settings | Values from |
|---|---|---|---|
| Depth implausible for the location (`depth_vs_bathymetry`) | `R/checks_07_depth.R` | `margin_m = 150` |  |
| Depth is not a number (`depth_numeric`) | `R/checks_07_depth.R` |  |  |
| Depth outside 0 to 11,000 m (`depth_in_range`) | `R/checks_07_depth.R` | `max_m = 11000` |  |
| Elevation not numeric, or minimum above maximum (`elevation_valid`) | `R/checks_07_depth.R` |  |  |
| Minimum depth greater than maximum depth (`depth_min_le_max`) | `R/checks_07_depth.R` |  |  |

### Taxonomy

| Check | Code | Settings | Values from |
|---|---|---|---|
| AphiaID is not the accepted name (`aphiaid_accepted`) | `R/checks_08_taxonomy.R` |  |  |
| AphiaID not found in WoRMS (`scientificnameid_exists_in_worms`) | `R/checks_08_taxonomy.R` |  |  |
| Name without a WoRMS ID (ID suggested) (`names_without_id_matched`) | `R/checks_08_taxonomy.R` |  |  |
| Not identified to family level or lower (`taxon_identified_to_family_or_lower`) | `R/checks_08_taxonomy.R` | `lowest_rank = family` |  |
| Taxon is not marine or brackish (`taxon_is_marine`) | `R/checks_08_taxonomy.R` |  |  |
| scientificName and scientificNameID do not go one-to-one (`scientificname_id_one_to_one`) | `R/checks_15_one_to_one.R` |  |  |
| scientificName contains more than the name (`scientificname_clean`) | `R/checks_08_taxonomy.R` |  | reference/name_patterns.csv |
| scientificName does not match the AphiaID (`scientificname_matches_aphiaid`) | `R/checks_08_taxonomy.R` | `max_distance = 5` |  |
| scientificNameID is not a WoRMS LSID (`scientificnameid_worms_format`) | `R/checks_08_taxonomy.R` |  |  |

### Measurements (eMoF)

| Check | Code | Settings | Values from |
|---|---|---|---|
| Count given twice (individualCount and eMoF) (`individualcount_also_in_emof`) | `R/checks_09_emof.R` |  |  |
| Deprecated vocabulary term (`vocabulary_terms_not_deprecated`) | `R/checks_09_emof_nerc.R` |  |  |
| Fact without a value ID, or measurement without a unit (`value_has_id_or_unit`) | `R/checks_09_emof.R` |  |  |
| Measurement belongs in a Darwin Core field (`emof_type_has_dwc_field`) | `R/checks_09_emof.R` |  | reference/p01_to_dwc.csv |
| Positive abundance for an absent taxon (`positive_quantity_with_absence`) | `R/checks_09_emof.R` |  | reference/nerc_quantity_terms.csv |
| Sampling effort not described (`sampling_effort_present`) | `R/checks_09_emof.R` |  | reference/sampling_effort_terms.csv |
| Sampling instrument not described (`sampling_instrument_present`) | `R/checks_09_emof.R` |  | reference/sampling_instrument_terms.csv |
| Several units for one measurement type (`unit_consistent_per_type`) | `R/checks_09_emof.R` |  |  |
| Unit does not fit the measurement type (`type_unit_compatible`) | `R/checks_09_emof_nerc.R` |  |  |
| Unit text does not match the unit ID (`unit_text_matches_unitid`) | `R/checks_09_emof_nerc.R` |  | reference/unit_synonyms.csv |
| Value and value ID do not go one-to-one (`measurementvalueid_consistent`) | `R/checks_09_emof.R` |  |  |
| Value not numeric although a unit is given (`value_numeric_when_unit`) | `R/checks_09_emof.R` |  |  |
| Vocabulary URI malformed (`nvs_uri_format`) | `R/checks_09_emof.R` |  |  |
| Zero abundance or biomass for a present taxon (`zero_quantity_with_presence`) | `R/checks_09_emof.R` |  | reference/nerc_quantity_terms.csv |
| measurementType and measurementTypeID do not go one-to-one (`measurementtype_id_one_to_one`) | `R/checks_15_one_to_one.R` |  |  |
| measurementType text does not fit the measurementTypeID (`measurementtype_matches_typeid`) | `R/checks_09_emof_nerc.R` |  |  |
| measurementTypeID empty (`measurementtypeid_filled`) | `R/checks_09_emof.R` |  |  |
| measurementTypeID not found in NERC (`measurementtypeid_valid`) | `R/checks_09_emof_nerc.R` |  |  |
| measurementUnit and measurementUnitID do not go one-to-one (`measurementunit_id_one_to_one`) | `R/checks_15_one_to_one.R` |  |  |
| measurementUnitID empty although a unit is given (`measurementunitid_filled`) | `R/checks_09_emof.R` |  |  |
| measurementUnitID not found in NERC P06 (`measurementunitid_valid`) | `R/checks_09_emof_nerc.R` |  |  |
| measurementValueID not found in NERC (`measurementvalueid_valid`) | `R/checks_09_emof_nerc.R` |  |  |
| organismQuantity not used as EMODnet expects (`organismquantity_use`) | `R/checks_09_emof.R` |  |  |

### Duplicates

| Check | Code | Settings | Values from |
|---|---|---|---|
| Duplicate eMoF records (`duplicate_emof_records`) | `R/checks_10_duplicates.R` |  |  |
| Potential duplicate occurrences (`duplicate_occurrences`) | `R/checks_10_duplicates.R` |  | reference/duplicate_fields.csv, reference/size_types.csv |
| Same measurement type twice for one event (`duplicate_type_per_event`) | `R/checks_10_duplicates.R` |  |  |
| Same measurement type twice for one occurrence (`duplicate_type_per_occurrence`) | `R/checks_10_duplicates.R` |  |  |

### Other fields

| Check | Code | Settings | Values from |
|---|---|---|---|
| Other values and their IDs do not go one-to-one (`values_and_ids_one_to_one`) | `R/checks_15_one_to_one.R` |  | reference/one_to_one_pairs.csv |
| dynamicProperties is not valid JSON (`dynamicproperties_json`) | `R/checks_11_other.R` |  |  |
| individualCount is not a whole number (`individualcount_valid`) | `R/checks_11_other.R` |  |  |
| organismQuantity without a type (`organismquantity_with_type`) | `R/checks_11_other.R` |  |  |

### DNA

| Check | Code | Settings | Values from |
|---|---|---|---|
| DNA-derived data extension missing (`dna_extension_present`) | `R/checks_14_dna.R` |  |  |
| DNA-derived data incomplete (`dna_fields_complete`) | `R/checks_14_dna.R` |  | rules workbook, sheet 'Fields' (applies_to = dna-derived) |
| DNA_sequence contains other than nucleotide codes (`dna_sequence_valid`) | `R/checks_14_dna.R` |  |  |

### Outliers

| Check | Code | Settings | Values from |
|---|---|---|---|
| Date far outside the dataset's sampling period (`outlier_date_dataset`) | `R/checks_13_outliers.R` | `inner_percent = 95; widen_factor = 0.5; min_margin_years = 2; min_dates = 20` |  |
| Depth far outside the dataset's depth range (`outlier_depth_dataset`) | `R/checks_13_outliers.R` | `inner_percent = 95; widen_factor = 0.5; min_margin_m = 50; min_values = 20` |  |
| Position far outside the dataset's sampling area (`outlier_location_dataset`) | `R/checks_13_outliers.R` | `inner_percent = 95; widen_factor = 0.5; min_margin_degrees = 1; min_positions = 20` |  |
| Species far from its other records (`outlier_species_location`) | `R/checks_13_outliers.R` | `inner_percent = 95; widen_factor = 0.5; min_positions = 10; min_margin_km = 100` |  |
| Unusual length or weight for that species (`outlier_size_per_species`) | `R/checks_13_outliers.R` | `iqr_factor = 3; min_values = 10` | reference/size_types.csv |
| Unusual measurement value (`outlier_measurement_value`) | `R/checks_13_outliers.R` | `iqr_factor = 3; min_values = 20` |  |

### Metadata (EML)

| Check | Code | Settings | Values from |
|---|---|---|---|
| Abstract missing (`eml_abstract_filled`) | `R/checks_12_metadata.R` |  |  |
| Abstract not informative (`eml_abstract_informative`) | `R/checks_12_metadata.R` | `min_words = 50` |  |
| Data creator missing (`eml_creator_filled`) | `R/checks_12_metadata.R` |  |  |
| Dataset citation missing (`eml_citation_filled`) | `R/checks_12_metadata.R` |  |  |
| Dataset title missing (`eml_title_filled`) | `R/checks_12_metadata.R` |  |  |
| Geographic coverage missing (`eml_geographic_coverage_filled`) | `R/checks_12_metadata.R` |  |  |
| IPT resource URL longer than 255 characters (`ipt_url_length`) | `R/checks_12_metadata.R` | `max_characters = 255` |  |
| Institutes without an identifier (`eml_organisation_identifier`) | `R/checks_12_metadata.R` |  |  |
| Language not given (`eml_language_given`) | `R/checks_12_metadata.R` |  |  |
| Licence in the data differs from the metadata (`license_data_matches_eml`) | `R/checks_12_metadata.R` |  |  |
| Licence missing, or not CC0, CC-BY or CC-BY-NC (`eml_license_valid`) | `R/checks_12_metadata.R` |  |  |
| Metadata file missing (`eml_present`) | `R/checks_12_metadata.R` |  |  |
| Metadata provider missing or without e-mail (`eml_metadata_provider_filled`) | `R/checks_12_metadata.R` |  |  |
| Methods not described (`eml_methods_described`) | `R/checks_12_metadata.R` |  |  |
| No persistent identifier (DOI) (`eml_persistent_identifier`) | `R/checks_12_metadata.R` |  |  |
| People without an ORCID (`eml_people_have_orcid`) | `R/checks_12_metadata.R` |  |  |
| Placeholder text in the metadata (`eml_no_placeholders`) | `R/checks_12_metadata.R` |  | reference/placeholders.csv |
| Point of contact missing or without e-mail (`eml_contact_filled`) | `R/checks_12_metadata.R` |  |  |
| Taxonomic coverage missing (`eml_taxonomic_coverage_filled`) | `R/checks_12_metadata.R` |  |  |
| Temporal coverage missing (`eml_temporal_coverage_filled`) | `R/checks_12_metadata.R` |  |  |
| Themes and keywords missing (`eml_keywords_filled`) | `R/checks_12_metadata.R` |  |  |
| Title not informative (`eml_title_informative`) | `R/checks_12_metadata.R` | `min_words = 5` |  |
| license is not an accepted licence (`license_field_valid`) | `R/checks_04_vocabulary.R` |  |  |

