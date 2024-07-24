"""
Script to multiple CSVs (each containing profile information of compiling one file) into
a single summary profile information CSV file.
"""

from argparse import ArgumentParser
import csv
from pathlib import Path
from typing import Dict, List, Union

parser = ArgumentParser(description="Combine multiple profile information CSVs into one")
parser.add_argument("dump_dir", help="The folder the profile information has been dumped in")
parser.add_argument("summary_path", help="The path to store the summary CSV")

args = parser.parse_args()

DUMP_DIR = Path(args.dump_dir)
if not DUMP_DIR.exists():
    raise ValueError(f"{DUMP_DIR} does not exist")
SUMMARY_PATH = Path(args.summary_path)

SUMMARY_NON_COUNTER_FIELD_NAMES = [
    "pass name",
    "time",
    # CR mitom: Not supporting memory for now
    # "alloc",
    # "top-heap",
    # "absolute-top-heap",
]
SUMMARY_COUNTER_FIELD_NAMES = [
    "spill",
    "reload",
]
SUMMARY_FIELD_NAMES = SUMMARY_NON_COUNTER_FIELD_NAMES + SUMMARY_COUNTER_FIELD_NAMES


def get_field_names(csv_path: Path) -> List[str]:
    with open(csv_path) as csv_file:
        csv_reader = csv.DictReader(csv_file)
        field_names = csv_reader.fieldnames
    return field_names


def parse_counters(counters: str) -> Dict[str, str]:
    split_counters = counters.strip("[]").split("; ")
    return dict(map(lambda x: tuple(x.split(" = ")), split_counters))


def csv_to_summary(csv_reader: csv.DictReader) -> Dict[str, str]:
    rows = list(csv_reader)
    non_counters_summary = {
        k: v for k, v in rows[0].items() if k in SUMMARY_NON_COUNTER_FIELD_NAMES
    }
    counters_summary = next(
        (parse_counters(row["counters"]) for row in rows if row["counters"]),
        None
    )
    if counters_summary is None:
        return None
    return {**non_counters_summary, **counters_summary}


def split_into_number_and_unit(number_string: str) -> (str, Union[int, float]):
    number_chars = "".join(str(i) for i in range(10)) + "."
    suffix_start = next(
        (i for i, c in enumerate(number_string) if c not in number_chars),
        len(number_string)
    )
    number, unit = float(number_string[:suffix_start]), number_string[suffix_start:]
    if number.is_integer():
        number = int(number)
    return number, unit


def update_totals(totals: dict, summary: dict) -> None:
    for k, v in summary.items():
        if k != "pass name":
            total_number, total_unit = totals[k]
            number, unit = split_into_number_and_unit(v)
            if total_unit is None:
                total_unit = unit
            total_number += number
            totals[k] = (total_number, total_unit)


def process_csv(csv_path: Path, summary_csv_writer: csv.DictWriter, totals: dict) -> None:
    with open(csv_path) as csv_file:
        csv_reader = csv.DictReader(csv_file)
        summary = csv_to_summary(csv_reader)
        if summary is not None:
            summary_csv_writer.writerow(summary)
            update_totals(totals, summary)


with open(SUMMARY_PATH, "w", newline="") as summary_csv_file:
    totals = {k: (0, None) for k in SUMMARY_FIELD_NAMES if k != "pass name"}
    summary_csv_writer = csv.DictWriter(summary_csv_file, fieldnames=SUMMARY_FIELD_NAMES)
    summary_csv_writer.writeheader()

    for curr_csv_path in DUMP_DIR.glob("*.csv"):
        process_csv(curr_csv_path, summary_csv_writer, totals)
        curr_csv_path.unlink()
    totals = {k: "".join(map(str, v)) for k, v in totals.items()}
    totals["pass name"] = "total"
    summary_csv_writer.writerow(totals)
