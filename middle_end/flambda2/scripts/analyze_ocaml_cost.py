"""Script to extract ocaml's approximated code sizes and real code sizes for a module.
The compiler must have been build by dune beforehand.
"""
import sys
import subprocess
import re
import json
from collections import namedtuple
import matplotlib.pyplot as plt
import argparse
from math import log

parser = argparse.ArgumentParser(description="Plot the groundtruth code size vs flambda approximated one")
parser.add_argument("module",
                    help="the module name to compile (from flambdatest/mlexamples)")
parser.add_argument("--plot", dest="plot_path",
                    help="Plot the result to a file")
parser.add_argument("--dump", dest="dump_path",
                    help="Dump code sizes to a file")

args = parser.parse_args()

cmd = ["../../_build/default/optmain.exe", "-nostdlib", "-nopervasives", "-c", "-g", args.module + ".ml"]
r = subprocess.run(cmd, stdout=subprocess.PIPE, check=True, cwd="flambdatest/mlexamples", env={"PRINT_SIZES": "t"})
ocaml_symbol_to_size = {}
regex = re.compile(r"\x1b.+?m")
for l in r.stdout.decode().split("\n"):
    l = re.sub(regex, "" , l)
    l = l.strip().split()
    if len(l) < 2:
        continue
    symbol = l[0]
    code_size = int(l[1])
    ocaml_symbol_to_size[symbol] = code_size


r = subprocess.run(["nm", "-S", "-f", "posix", args.module + ".o"], stdout=subprocess.PIPE, check=True, cwd="flambdatest/mlexamples")
groundtruth_symbol_to_size = {}
for l in r.stdout.decode().split("\n"):
    l = l.strip().split()
    # nm returns entry of the form SYMBOL_NAME TYPE OFFSET CODE_SIZE
    # For some entries CODE_SIZE might be missing, so skip those.
    if len(l) < 4:
        continue
    symbol = l[0]
    type_ = l[1]
    code_size = int(l[3], 16)
    groundtruth_symbol_to_size[symbol] = code_size


common_symbols = set(groundtruth_symbol_to_size.keys()) & set(ocaml_symbol_to_size.keys())

Symbol = namedtuple("Symbol", ["name", "groundtruth_size", "ocaml_size"])

max_ocaml_size = max(ocaml_symbol_to_size.values())
max_groundtruth_size = max(groundtruth_symbol_to_size.values())

symbols = []
for name in common_symbols:
    g = groundtruth_symbol_to_size[name]
    o = ocaml_symbol_to_size[name]
    symbols.append(Symbol(name, g, o))

# Create the plot
if args.plot_path is not None:
    symbol_plot = sorted(symbols, key=lambda x: x.groundtruth_size)
    ocaml_plot_size = [x.ocaml_size for x in symbol_plot]
    groundtruth_plot_size = [x.groundtruth_size for x in symbol_plot]
    _, ax = plt.subplots(1)
    ax.scatter(list(map(log, ocaml_plot_size)), list(map(log, groundtruth_plot_size)))
    ax.set_xlabel("Ocaml size")
    ax.set_ylabel("Binary size")
    ax.set_ylim(bottom=0)
    ax.set_xlim(left=0)
    ax.plot([0, 1], [0, 1], transform=ax.transAxes)
    plt.title(f"{args.module}.ml")
    plt.savefig(args.plot_path)

if args.dump_path is not None:
    with open(args.dump_path, "w") as f:
        json.dump([s._asdict() for s in symbols], f)

