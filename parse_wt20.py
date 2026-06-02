"""
parse_wt20.py

Parses Cricsheet-format YAML files for Women's T20 Internationals located in
the ./wt20/ folder (same directory as this script) and produces a single
ball-by-ball CSV (wt20_ball_by_ball.csv) ready for statistical analysis in R.

Sections:
  1. Imports and configuration
  2. Helper functions (innings number, extras parsing, first-innings total)
  3. Per-match parser that walks the YAML and yields one row per delivery
  4. Main loop that iterates over every YAML file, logging failures to
     parse_errors.log and continuing on error
  5. Export to UTF-8 CSV and print of summary statistics
"""

# 1. Imports and configuration ------------------------------------------------
from pathlib import Path
import yaml
import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR / "wt20"
OUTPUT_CSV = SCRIPT_DIR / "wt20_ball_by_ball.csv"
ERROR_LOG = SCRIPT_DIR / "parse_errors.log"

T20_LEGAL_DELIVERIES = 120


# 2. Helpers ------------------------------------------------------------------
def innings_number(innings_key: str) -> int:
    """Map a YAML innings key ("1st innings", "2nd innings", "...super over")
    to 1, 2, or 3."""
    key = innings_key.lower()
    if "super over" in key:
        return 3
    if key.startswith("1st"):
        return 1
    if key.startswith("2nd"):
        return 2
    return 0


def extras_summary(extras_dict):
    """Return (extras_type, is_wide, is_noball, is_legal_delivery)."""
    if not extras_dict:
        return (pd.NA, 0, 0, 1)
    types = ",".join(sorted(extras_dict.keys()))
    is_wide = int("wides" in extras_dict)
    is_noball = int("noballs" in extras_dict)
    is_legal = 0 if (is_wide or is_noball) else 1
    return (types, is_wide, is_noball, is_legal)


def first_innings_total(innings_list):
    """Sum total runs of the first innings, for use as the chase target."""
    for innings in innings_list:
        for key, body in innings.items():
            if innings_number(key) == 1:
                return sum(
                    next(iter(d.values()))["runs"]["total"]
                    for d in body.get("deliveries", [])
                )
    return None


# 3. Per-match parser ---------------------------------------------------------
def parse_match(path: Path):
    """Yield one dict per delivery for a single YAML file."""
    with open(path, "r", encoding="utf-8") as f:
        match = yaml.safe_load(f)

    info = match["info"]
    match_id = path.stem

    dates = info.get("dates", [])
    date = str(dates[0]) if dates else pd.NA

    teams = info.get("teams", [])
    team_1 = teams[0] if len(teams) > 0 else pd.NA
    team_2 = teams[1] if len(teams) > 1 else pd.NA

    toss = info.get("toss", {}) or {}
    outcome = info.get("outcome", {}) or {}
    by = outcome.get("by", {}) or {}

    pom_list = info.get("player_of_match") or []
    player_of_match = pom_list[0] if pom_list else pd.NA

    match_row = {
        "match_id": match_id,
        "date": date,
        "venue": info.get("venue", pd.NA),
        "city": info.get("city", pd.NA),
        "match_type": info.get("match_type", pd.NA),
        "gender": info.get("gender", pd.NA),
        "team_1": team_1,
        "team_2": team_2,
        "toss_winner": toss.get("winner", pd.NA),
        "toss_decision": toss.get("decision", pd.NA),
        "winner": outcome.get("winner", pd.NA),
        "win_by_runs": int(by.get("runs", 0) or 0),
        "win_by_wickets": int(by.get("wickets", 0) or 0),
        "player_of_match": player_of_match,
    }

    first_total = first_innings_total(match["innings"])

    for innings in match["innings"]:
        for innings_key, body in innings.items():
            inn_no = innings_number(innings_key)
            batting_team = body.get("team", pd.NA)
            if batting_team == team_1:
                bowling_team = team_2
            elif batting_team == team_2:
                bowling_team = team_1
            else:
                bowling_team = pd.NA

            cumulative_runs = 0
            cumulative_wickets = 0
            balls_bowled = 0
            global_ball = 0

            this_target = (
                first_total + 1
                if inn_no == 2 and first_total is not None
                else None
            )

            for delivery in body.get("deliveries", []):
                ball_key, ball = next(iter(delivery.items()))
                over_str, ball_str = str(ball_key).split(".")
                over_one_indexed = int(over_str) + 1
                ball_in_over = int(ball_str)
                global_ball += 1

                runs = ball.get("runs", {}) or {}
                runs_batter = int(runs.get("batsman", 0) or 0)
                runs_extras = int(runs.get("extras", 0) or 0)
                runs_total = int(runs.get("total", runs_batter + runs_extras))

                extras_type, is_wide, is_noball, is_legal = extras_summary(
                    ball.get("extras")
                )

                wicket = ball.get("wicket")
                if wicket:
                    # `wicket` is usually a dict, but can be a list of dicts
                    # when multiple batters are dismissed on one delivery
                    # (e.g. retired-out cascade). Normalise to a list.
                    wickets = wicket if isinstance(wicket, list) else [wicket]
                    is_wicket = 1
                    wickets_this_ball = len(wickets)
                    wicket_kind = ",".join(
                        str(w.get("kind", "")) for w in wickets
                    ) or pd.NA
                    player_out = ",".join(
                        str(w.get("player_out", "")) for w in wickets
                    ) or pd.NA
                    first_fielders = wickets[0].get("fielders") or []
                    fielder = first_fielders[0] if first_fielders else pd.NA
                else:
                    is_wicket = 0
                    wickets_this_ball = 0
                    wicket_kind = pd.NA
                    player_out = pd.NA
                    fielder = pd.NA

                cumulative_runs += runs_total
                cumulative_wickets += wickets_this_ball
                if is_legal:
                    balls_bowled += 1

                balls_remaining = max(T20_LEGAL_DELIVERIES - balls_bowled, 0)
                current_run_rate = (
                    cumulative_runs / (balls_bowled / 6)
                    if balls_bowled > 0
                    else 0.0
                )

                if inn_no == 2 and this_target is not None:
                    runs_required = max(this_target - cumulative_runs, 0)
                    required_run_rate = (
                        runs_required / (balls_remaining / 6)
                        if balls_remaining > 0
                        else 0.0
                    )
                    target_val = this_target
                else:
                    runs_required = pd.NA
                    required_run_rate = pd.NA
                    target_val = pd.NA

                yield {
                    **match_row,
                    "innings": inn_no,
                    "batting_team": batting_team,
                    "bowling_team": bowling_team,
                    "over": over_one_indexed,
                    "ball": ball_in_over,
                    "global_ball": global_ball,
                    "batter": ball.get("batsman", pd.NA),
                    "non_striker": ball.get("non_striker", pd.NA),
                    "bowler": ball.get("bowler", pd.NA),
                    "runs_batter": runs_batter,
                    "runs_extras": runs_extras,
                    "runs_total": runs_total,
                    "extras_type": extras_type,
                    "is_wide": is_wide,
                    "is_noball": is_noball,
                    "is_legal_delivery": is_legal,
                    "cumulative_runs": cumulative_runs,
                    "cumulative_wickets": cumulative_wickets,
                    "balls_bowled": balls_bowled,
                    "balls_remaining": balls_remaining,
                    "current_run_rate": current_run_rate,
                    "target": target_val,
                    "runs_required": runs_required,
                    "required_run_rate": required_run_rate,
                    "is_wicket": is_wicket,
                    "wicket_kind": wicket_kind,
                    "player_out": player_out,
                    "fielder": fielder,
                }


# 4. Main loop ----------------------------------------------------------------
def main():
    rows = []
    processed = 0
    failed = 0

    yaml_files = sorted(DATA_DIR.glob("*.yaml"))

    with open(ERROR_LOG, "w", encoding="utf-8") as log:
        for path in yaml_files:
            try:
                rows.extend(parse_match(path))
                processed += 1
            except Exception as exc:
                failed += 1
                log.write(f"{path.name}: {exc!r}\n")

    df = pd.DataFrame(rows)

    # 5. Export and summary ---------------------------------------------------
    df.to_csv(OUTPUT_CSV, index=False, encoding="utf-8")

    if not df.empty:
        dmin, dmax = df["date"].min(), df["date"].max()
        teams = sorted(
            set(df["team_1"].dropna().unique())
            | set(df["team_2"].dropna().unique())
        )
    else:
        dmin = dmax = None
        teams = []

    print(f"Files processed: {processed}")
    print(f"Files failed:    {failed}  (see {ERROR_LOG.name})")
    print(f"Total rows:      {len(df)}")
    print(f"Date range:      {dmin} to {dmax}")
    print(f"Teams ({len(teams)}):")
    for t in teams:
        print(f"  - {t}")


if __name__ == "__main__":
    main()
