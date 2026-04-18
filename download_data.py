import os
import pandas as pd
import fastf1

YEAR = 2024
EVENT = "Monaco"
# Race
SESSION = "R"
OUTDIR = "data"

os.makedirs(OUTDIR, exist_ok=True)
os.makedirs(os.path.join(OUTDIR, "cache"), exist_ok=True)
os.makedirs(os.path.join(OUTDIR, "telemetry_raw"), exist_ok=True)

# enable cache
fastf1.Cache.enable_cache(os.path.join(OUTDIR, "cache"))

# creating session
session = fastf1.get_session(YEAR, EVENT, SESSION)
session.load()

# getting laps data
laps = session.laps.copy()
laps.to_csv(os.path.join(OUTDIR, "laps_raw.csv"), index=False)

# getting weather data 
weather = session.weather_data.copy()
weather.to_csv(os.path.join(OUTDIR, "weather_raw.csv"), index=False)

# getting telemetry
for idx, lap in laps.iterrows():

    driver = lap["Driver"]
    # lapNumber is originally float
    lap_no = int(lap["LapNumber"])

    tel = lap.get_car_data()

    filename = f"{driver}_lap_{lap_no:03d}.csv"
    tel.to_csv(os.path.join(os.path.join(OUTDIR, "telemetry_raw"), filename), index=False)