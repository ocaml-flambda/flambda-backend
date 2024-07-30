"""
Script to combine multiple CSVs (each containing profile information of compiling one
file) into a single summary profile information CSV file.
"""

from argparse import ArgumentParser
import csv
from pathlib import Path
from string import digits
from typing import Dict, Iterable, Iterator, List, Tuple, Union

parser = ArgumentParser(description="Combine multiple profile CSVs into one")
parser.add_argument("dump_dir", help="The folder the profile CSVs have been dumped in")
parser.add_argument("-o", "--summary_path", help="The path to store the summary CSV")

args = parser.parse_args()

DUMP_DIR = Path(args.dump_dir)
if not DUMP_DIR.exists():
    raise ValueError(f"{DUMP_DIR} does not exist")

if args.summary_path is None:
    args.summary_path = DUMP_DIR.parent / "summary.csv"
SUMMARY_PATH = Path(args.summary_path)

CsvRow = Dict[str, str]
CsvRows = List[CsvRow]
SummaryRow = Dict[str, str]


def parse_counters(counters: str) -> Dict[str, str]:
    if not counters:
        return {}
    split_counters = counters.strip("[]").split("; ")
    return dict(map(lambda x: tuple(x.split(" = ")), split_counters))


def get_csv_rows(csv_path: Path) -> CsvRows:
    with open(csv_path) as csv_file:
        csv_reader = csv.DictReader(csv_file)
        return list(csv_reader)


def get_input_csv_paths() -> Iterator[Tuple[Path, CsvRows]]:
    return (csv_path for csv_path in DUMP_DIR.glob("*.csv"))


def split_into_number_and_unit(number_string: str) -> (Union[int, float], str):
    numeric = digits + "."
    unit_start_position = next(
        (i for i, c in enumerate(number_string) if c not in numeric), len(number_string)
    )
    number = float(number_string[:unit_start_position])
    unit = number_string[unit_start_position:]
    if number.is_integer():
        number = int(number)
    return number, unit


PRIMARY_KEY = "pass name"
COUNTERS_FIELD = "counters"
summary_non_counter_fields = set()
summary_counter_fields = set()

# Retrieve non-counter and counter field names selected dynamically from CSV files (also
# asserts that all CSV files should have primary key and counters fields)
for rows in map(get_csv_rows, get_input_csv_paths()):
    fields = set(rows[0])
    assert PRIMARY_KEY in fields
    assert COUNTERS_FIELD in fields

    for row in rows:
        summary_counter_fields.update(parse_counters(row[COUNTERS_FIELD]))

    fields.remove(PRIMARY_KEY)
    fields.remove(COUNTERS_FIELD)
    summary_non_counter_fields.update(fields)

# CR mitom: Not supporting memory for now
summary_non_counter_fields -= {
    "alloc",
    "top-heap",
    "absolute-top-heap",
}

# Ensure consistent ordering of summary fields
field_collections = {PRIMARY_KEY}, summary_non_counter_fields, summary_counter_fields
SUMMARY_FIELD_NAMES = sum(map(lambda fields: sorted(fields), field_collections), [])

units = {}
for field in summary_non_counter_fields:
    first_row = get_csv_rows(next(get_input_csv_paths()))[0]
    _, units[field] = split_into_number_and_unit(first_row[field])


def strip_units(row: CsvRow) -> CsvRow:
    return {k: (v[: -len(units[k])] if k in units else v) for k, v in row.items()}


def row_summary(row: CsvRow) -> SummaryRow:
    full_summary = {**strip_units(row), **parse_counters(row["counters"])}
    return {k: v for k, v in full_summary.items() if k in SUMMARY_FIELD_NAMES}


def csv_to_summaries(rows: CsvRows) -> Iterable[SummaryRow]:
    counter_rows = [row for row in rows if row["counters"]]
    return map(row_summary, counter_rows)


def split_into_number_and_unit(number_string: str) -> (str, Union[int, float]):
    number_chars = "".join(str(i) for i in range(10)) + "."
    non_numeric = (i for i, c in enumerate(number_string) if c not in number_chars)
    suffix_start = next(non_numeric, len(number_string))
    number, unit = float(number_string[:suffix_start]), number_string[suffix_start:]
    if number.is_integer():
        number = int(number)
    return number, unit


def process_csv(csv_path: Path, summary_csv_writer: csv.DictWriter) -> None:
    for summary in csv_to_summaries(get_csv_rows(csv_path)):
        summary_csv_writer.writerow(summary)


with open(SUMMARY_PATH, "w", newline="") as summary_csv_file:
    summary_csv_writer = csv.DictWriter(
        summary_csv_file, fieldnames=SUMMARY_FIELD_NAMES
    )
    summary_csv_writer.writeheader()
    # Add units as second header (can use header=[0,1] in pandas.read_csv)
    summary_csv_writer.writerow(units)

    for curr_csv_path in get_input_csv_paths():
        process_csv(curr_csv_path, summary_csv_writer)
print("Outputted summary CSV file to {}.".format(SUMMARY_PATH))
