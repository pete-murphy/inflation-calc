Note: API is limited to ten years at a time (and probably also rate-limited)

```sh
❯ curl -X POST -H 'Content-Type: application/json' \
     -d '{"seriesid":["CUUR0000SA0"],
        "startyear":"2013", "endyear":"2022"}' \
        https://api.bls.gov/publicAPI/v1/timeseries/data/ > 2013.json
```
