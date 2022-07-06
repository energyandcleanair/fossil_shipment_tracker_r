output$methodology <- renderUI({
  HTML('<h1><span>Methodology</span>

 </h1>
 <p><span style="color: #8cc9d0;">Update 2022-06-26 </span>-
       <ul>
       <li>We have included March 2022 trade data in our pricing model. This may impact previous commodity pricing retroactively.</li>
       </ul>
      </p>
  <p><span style="color: #8cc9d0;">Update 2022-06-21 </span>-
       <ul>
       <li>We have included February 2022 trade data in our pricing model. This may impact previous commodity pricing retroactively.</li>
       </ul>
      </p>

  <p><span style="color: #8cc9d0;">Update 2022-06-16 </span>-
       Two important improvements were implemented:
       <ul>
       <li>Oil leaving from Novorossiysk CPC terminal is now attributed to Kazakhstan rather than Russia;</li>
       <li>Pipelined gas entering Greece from Turkey at Kipi/Kipoi transmission point is now assumed to come from Azerbaidjan rather than Russia.</li>
       </ul>
       Additionally, we fixed a limitation that was excluding some of the shipments that took longer than a month to reach their destination.
      </p>


  <p><span style="color: #8cc9d0;">Update 2022-05-13 </span>-
       <span>This is a significant update, with four majour changes:</span>
       <ul>
       <li>we have fixed a mistake that led us to overestimate pipelined gas imports from Russia (read more <a href="https://energyandcleanair.org/russia-fossil-tracker-corrigendum/" target="_blank">here</a>);</li>
       <li>we have implemented a new methodology to attribute pipelined gas to individual European countries (read more <a href="https://energyandcleanair.org/russia-fossil-tracker-updated-methodology/" target="_blank">here</a>);</li>
       <li>we have added shipments loading at anchorage; and</li>
       <li>we have included January 2022 trade data in our price modeling.</li>
       </ul>
      </p>

 <p><span style="color: #8cc9d0;">Update 2022-05-04 </span>-
       <span>We fixed a bug pertaining to ship IMO / MMSI correspondance. As a result, several shipments that were not accounted for before were added to the counter.</span></p>

 <p><span style="color: #8cc9d0;">Update 2022-04-25 </span>-
       <span>We improved the shipment detection methodology, allowing us to detect more shipments than previously. The counter data has therefore increased faster than usual over the past dew days, progressively integrating previously missing shipments.</span></p>


       </h2><p><span style="color: #8cc9d0;">Update 2022-03-17 </span>-
       <span>We now estimate the volume of crude oil, oil product and LNG shipments based on the aggregate cargo capacity (deadweight tonnage) of ships of different types leaving Russian ports to a European Union port, extracted from <a href="https://www.marinetraffic.com/" target="_blank">MarineTraffic.com</a>.</span></p>

       <h2>Seaborne shipments</h2>
       <p>We track ship voyages between Russian ports and ports in other countries using data from MarineTraffic.com and Datalastic, derived from ship location (AIS) data.</p>
       <p>A voyage consists of a ship taking on cargo and departing from a Russian port, arriving in a non-Russian port and discharging cargo. More complex trips such as loading cargo from both a Russian and a non-Russian port are excluded.
For crude oil tankers and LNG tankers, the type of cargo is known. We assume that oil products tankers and oil/chemical tankers carry oil products. Coal is transported by bulk carrier and general cargo ships which also carry many other types of cargo. We identified 25 “coal export terminals” within Russian ports that export coal. These are specific port locations that are associated with loading coal. When a vessel takes on cargo at one of these locations, we assume that the shipment is a coal shipment.</p><p>
The amount of fuel transported in a shipment is estimated based on the cargo capacity (deadweight tonnage) of the ships, adjusted by the average ratio of ship capacity to reported customs volume. We validated this approach by aggregating the cargo capacity of shipments in the latest month with complete trade data (December 2021), and comparing these values to reported export volumes.</p><p>
Trade volumes and pricing of fossil fuels (i.e. oil, coal, and gas) are often not available on hourly or daily basis, and neither are the terms for long-term contracts. To develop this counter, we therefore relied on some assumptions, as detailed below.</p><p>

<h2>Other physical flows</h2>
For Europe, including Turkey, two main sources are being used for the following flows:

<table><tbody>
<tr><td colspan="1" rowspan="1"><p><span></span></p></td><td colspan="1" rowspan="1"><p><span>Crude oil</span></p></td><td colspan="1" rowspan="1"><p><span>Oil products</span></p></td><td colspan="1" rowspan="1"><p><span>Fossil gas</span></p></td><td colspan="1" rowspan="1"><p><span>Coal</span></p></td></tr>

<tr><td colspan="1" rowspan="1"><p><span>Pipeline</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>ENTSOG</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td></tr>

<tr><td colspan="1" rowspan="1"><p><span>Seaborne</span></p></td><td colspan="1" rowspan="1"><p><span>AIS data</span></p></td><td colspan="1" rowspan="1"><p><span>AIS data</span></p></td><td colspan="1" rowspan="1"><p><span>AIS data</span></p></td><td colspan="1" rowspan="1"><p><span>AIS data</span></p></td></tr></tbody></table>

       <p><span>ENTSOG data is available on a daily and near real-time basis.</span></p>

     <p>Eurostat data, however, is only available on a monthly basis till the end of 2021. To derive 2022 trade flows, we scale 2021 trade flows using y-o-y ratios in November-December 2021. Oil pipeline flows to the EU and China and gas pipeline flows to China are assumed to continue at 2021 levels. China’s pipeline import flows are based on latest figures given in news reports for winter 2021–22.</p>

<h3>Attribution of pipelined gas to individual countries</h3>
<p>Starting from 16 May 2022, we have implemented a new methodology to estimate pipeline gas imports from Russia to various European countries. We collect data from ENTSOG on flows between countries, at transmission interconnections for every single day. We then assume that on any given day, a country is a perfect ‘gas mixer’, that is, that all gas gets mixed before being consumed and/or re-exported. This allows us to attribute Russian gas consumption to countries that do not have direct connection with Russia. Read more about it <a href="https://energyandcleanair.org/russia-fossil-tracker-updated-methodology/" target="_blank">here</a>.<p>

     <h2>Pricing</h2>
     <p>Fossil fuels are sold on a variety of contracts including fixed-price, indexed to average oil prices and indexed to other spot prices. This means that the revenue to the exporter is not directly proportional to the current spot price.</p>

     <p>To estimate prices of fossil fuel trades in 2022, we first derive historical monthly average prices for imports from Russia to the EU from Eurostat, and to the rest of the world from UN COMTRADE, since the trade values are indicated both in physical and monetary terms. We then fit models between these historical prices and average monthly spot prices for the current month and with lags (Brent crude oil, TTF gas, Newcastle steam coal, Asian LNG, ARA coal). Models are built for main trading partners individually, and for the rest of the world as a whole.</p>

     <p>After the start of the invasion, the reluctance of many traders to take cargoes from Russia has driven discounted pricing of Russian oil. We apply the discount between Brent and Urals crude prices to crude oil exports to Europe and the discount between Brent and ESPO to exports to Asia.</p>

<p>Oil-indexed gas contracts have become less common over time, so we include a time interaction term in the model for pipeline gas prices. These models are then applied to current spot prices to estimate contract prices.</p>

<h2>References:</h2></p><ul><li><span>Eurostat: </span>
       <span><a href="https://ec.europa.eu/eurostat/databrowser/" target="_blank">https://ec.europa.eu/eurostat/databrowser/</a></span></li>
       <li><span>ENTSOG transparency platform: </span>
       <span><a href="https://transparency.entsog.eu" target="_blank">https://transparency.entsog.eu/</a></span><span>&nbsp;</span></li>
       <li><span>UN COMTRADE: </span>
       <span><a href="https://comtrade.un.org/Data/" target="_blank">https://comtrade.un.org/Data/</a></span><span>&nbsp;</span></li>
       </ul>')
})
