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
parser.add_argument("-o", "--summary_path", help="The path to store the summary CSV")

args = parser.parse_args()

DUMP_DIR = Path(args.dump_dir)
if not DUMP_DIR.exists():
    raise ValueError(f"{DUMP_DIR} does not exist")

if args.summary_path is None:
    args.summary_path = DUMP_DIR.parent / "summary.csv"
SUMMARY_PATH = Path(args.summary_path)


def parse_counters(counters: str) -> Dict[str, str]:
    if not counters:
        return {}
    split_counters = counters.strip("[]").split("; ")
    return dict(map(lambda x: tuple(x.split(" = ")), split_counters))


def get_csv_rows(csv_path: Path) -> List[Dict]:
    with open(csv_path) as csv_file:
        csv_reader = csv.DictReader(csv_file)
        return list(csv_reader)


PRIMARY_KEY = "pass name"
summary_non_counter_fields = set()
summary_counter_fields = set()

for csv_path in DUMP_DIR.glob("*.csv"):
    rows = get_csv_rows(csv_path)
    curr_fields = set(rows[0])
    curr_fields.remove(PRIMARY_KEY)
    if "counters" in curr_fields:
        curr_fields.remove("counters")
        for row in rows:
            summary_counter_fields.update(parse_counters(row["counters"]))
    summary_non_counter_fields.update(curr_fields)

# CR mitom: Not supporting memory for now
SUMMARY_UNSUPPORTED_FIELDS = {
    "alloc",
    "top-heap",
    "absolute-top-heap",
}

# Ensure consistent ordering of summary fields
field_collections = {PRIMARY_KEY}, summary_non_counter_fields, summary_counter_fields
SUMMARY_FIELD_NAMES = sum(
    map(lambda fields: sorted(fields - SUMMARY_UNSUPPORTED_FIELDS), field_collections),
    []
)


def row_summary(row: Dict[str, str]) -> Dict[str, str]:
    full_summary = {**row, **parse_counters(row["counters"])}
    return {k: v for k, v in full_summary.items() if k in SUMMARY_FIELD_NAMES}


def csv_to_summaries(rows: List[Dict]) -> List[Dict[str, str]]:
    counter_rows = [row for row in rows if row["counters"]]
    return map(row_summary, counter_rows)


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


def process_csv(csv_path: Path, summary_csv_writer: csv.DictWriter) -> None:
    for summary in csv_to_summaries(get_csv_rows(csv_path)):
        summary_csv_writer.writerow(summary)


with open(SUMMARY_PATH, "w", newline="") as summary_csv_file:
    summary_csv_writer = csv.DictWriter(summary_csv_file, fieldnames=SUMMARY_FIELD_NAMES)
    summary_csv_writer.writeheader()

    for curr_csv_path in DUMP_DIR.glob("*.csv"):
        process_csv(curr_csv_path, summary_csv_writer)
        curr_csv_path.unlink()
