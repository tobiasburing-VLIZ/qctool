# A complete EML example, used by several test files.
example_eml <- '<?xml version="1.0" encoding="UTF-8"?>
<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0" packageId="abc-123/v1.2" xml:lang="eng">
  <dataset>
    <alternateIdentifier>https://doi.org/10.15468/abcd12</alternateIdentifier>
    <alternateIdentifier>https://ipt.vliz.be/eurobis/resource?r=test_data</alternateIdentifier>
    <title xml:lang="eng">Benthos of the Belgian part of the North Sea 2010-2020</title>
    <creator>
      <individualName><givenName>Ann</givenName><surName>Example</surName></individualName>
      <organizationName>Flanders Marine Institute (VLIZ)</organizationName>
      <electronicMailAddress>ann@example.org</electronicMailAddress>
      <userId directory="https://orcid.org/">0000-0002-1825-0097</userId>
    </creator>
    <metadataProvider><organizationName>VLIZ</organizationName><electronicMailAddress>data@vliz.be</electronicMailAddress></metadataProvider>
    <pubDate>2024-03-01</pubDate>
    <language>eng</language>
    <abstract><para>First paragraph.</para><para>Second paragraph.</para></abstract>
    <keywordSet><keyword>Occurrence</keyword><keyword>Observation</keyword><keywordThesaurus>GBIF Dataset Type Vocabulary</keywordThesaurus></keywordSet>
    <intellectualRights><para>This work is licensed under a <ulink url="http://creativecommons.org/licenses/by/4.0/legalcode"><citetitle>Creative Commons Attribution (CC-BY) 4.0 License</citetitle></ulink>.</para></intellectualRights>
    <coverage>
      <geographicCoverage>
        <geographicDescription>Belgian part of the North Sea</geographicDescription>
        <boundingCoordinates><westBoundingCoordinate>2.2</westBoundingCoordinate><eastBoundingCoordinate>3.4</eastBoundingCoordinate>
          <northBoundingCoordinate>51.9</northBoundingCoordinate><southBoundingCoordinate>51.1</southBoundingCoordinate></boundingCoordinates>
      </geographicCoverage>
      <temporalCoverage><rangeOfDates><beginDate><calendarDate>2010-01-01</calendarDate></beginDate><endDate><calendarDate>2020-12-31</calendarDate></endDate></rangeOfDates></temporalCoverage>
      <taxonomicCoverage>
        <generalTaxonomicCoverage>Macrobenthos</generalTaxonomicCoverage>
        <taxonomicClassification><taxonRankName>class</taxonRankName><taxonRankValue>Bivalvia</taxonRankValue></taxonomicClassification>
      </taxonomicCoverage>
    </coverage>
    <maintenance><description><para>Updated yearly.</para></description><maintenanceUpdateFrequency>annually</maintenanceUpdateFrequency></maintenance>
    <contact><individualName><surName>Contact</surName></individualName><electronicMailAddress>contact@example.org</electronicMailAddress></contact>
    <methods><methodStep><description><para>Van Veen grab, 0.1 m2.</para></description></methodStep>
      <sampling><studyExtent><description><para>Ten stations.</para></description></studyExtent><samplingDescription><para>Yearly in autumn.</para></samplingDescription></sampling></methods>
    <project id="proj-1"><title>LifeWatch</title><funding><para>Research Foundation Flanders</para></funding></project>
  </dataset>
  <additionalMetadata><metadata><gbif><citation>Example A (2024) Benthos of the BPNS. VLIZ.</citation>
    <bibliography><citation>Example A (2021) A paper.</citation></bibliography></gbif></metadata></additionalMetadata>
</eml:eml>'
