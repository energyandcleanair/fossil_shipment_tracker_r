
### Regenerate EUROSTAT pricing model

To regenerate the EUROSTAT pricing model:
1. Go to the root directory of the `fossil_shipment_tracker_r` project in your terminal
2. Run the script `scripts/rebuild_eurostat.R`
   ```
   ./scripts/rebuild_eurostat.R
   ```
3. Add, commit, and push the latest `pricing_models_eurostat.RDS` files:
   ```
   git add **/pricing_models_eurostat.RDS
   git commit -m "Update eurostat model"
   git push
   ```
