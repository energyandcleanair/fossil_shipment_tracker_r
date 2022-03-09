output$methodology <- renderUI({
  HTML('<h1><span>Methodology</span>
       </h1><p><span>Trade volumes and pricing of fossil fuels (i.e. oil, coal, and gas) are often not available on hourly or daily basis, and neither are the terms for long-term contracts. To develop this counter, we therefore relied on some assumptions, as detailed below.</span></p>

       <h2><span>Physical flows</span></h2><p><span>Two main sources are being used for the following flows:</span></p><p><span></span></p><a></a><a></a><table><tbody><tr><td colspan="1" rowspan="1"><p><span></span></p></td><td colspan="1" rowspan="1"><p><span>Crude oil</span></p></td><td colspan="1" rowspan="1"><p><span>Oil products</span></p></td><td colspan="1" rowspan="1"><p><span>Fossil gas</span></p></td><td colspan="1" rowspan="1"><p><span>Coal</span></p></td></tr><tr><td colspan="1" rowspan="1"><p><span>Pipeline</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>ENTSOG</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td></tr><tr><td colspan="1" rowspan="1"><p><span>Seaborne</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td><td colspan="1" rowspan="1"><p><span>Eurostat</span></p></td></tr></tbody></table><p>

       </p>

       <p><span>ENTSOG data is available on a daily and near real-time basis.</span></p>

       <p></p><p><span>Eurostat data however is available on a monthly basis till the end of 2021. To derive 2022 trade flows, we scale 2021 trade flows using y-o-y ratios in November-December 2021. We further apply, conservatively, a 50% reduction to all seaborne trades in 2022 due to the situation in Ukraine. Oil pipeline flows are reportedly continuing at normal levels.</span></p>

       <h2><span>Pricing</span></h2><p>Fossil fuels are sold on a variety of contracts including fixed-price, indexed to average oil prices and indexed to other spot prices. This means that the revenue to the exporter is not directly proportional to the current spot price.</p>


<p>To estimate prices of fossil fuel trades in 2022, we first derive historical monthly average prices for imports from Russia to the EU from Eurostat, since the trade values are indicated both in physical and monetary terms. We then fit models between these historical prices and average monthly spot prices for the current month and with lags (Brent, TTF, ARA).</p>
       <p>Oil-indexed gas contracts have become less common over time, so we include a time interaction term in the model for pipeline gas prices. These models are then applied to current spot prices to estimate contract prices.</p>

      </p><p><h2>References:</h2></p><ul><li><span>Eurostat: </span><span><a href="https://ec.europa.eu/eurostat/databrowser/" target="_blank">https://ec.europa.eu/eurostat/databrowser/</a></span></li><li><span>ENTSOG transparency platform: </span><span><a href="https://transparency.entsog.eu" target="_blank">https://transparency.entsog.eu/</a></span><span>&nbsp;</span></li></ul>')
})
