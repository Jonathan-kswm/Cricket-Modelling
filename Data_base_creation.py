#We need to gather data on the womens T20I cricket

#We are going to collect games from teams playing in this years T20 world cup
#then we are going to add these to a csv after cleaning the data

import yaml 
import shutil
import os

#see where everything is
with open("t20s/211028.yaml", "r") as f:
  match = yaml.safe_load(f)

print(match.keys())

print(match['meta'])

os.makedirs("wt20", exist_ok=True)

for file in os.listdir("t20s"):
    if not file.endswith(".yaml"):
        continue
    with open(f"t20s/{file}", "r") as f:
        match = yaml.safe_load(f)

    if match["info"]["gender"] == "female":
        shutil.move(f"t20s/{file}", f"wt20/{file}")