"""Check FVS memory mode against the normal SQLite file output.

Covers fvsSetMemoryTables and the fvsTable* API in dbsqlite/dbstables.f.
Runs tests/FVSie/DBReportTest.key twice through the shared library, each in
its own process (FVS keeps global state, so a process runs only one mode):

- file: memory mode off; FVS writes DBReportTest.db as usual.
- mem: memory mode on, stopping at every cycle (stop point 6) and at the end
  of each stand; at each stop every table is read through the API and then
  cleared. Buffers start at 1 byte, so every read also exercises the
  "buffer too small" return code and retry.

The checks, one function each:

- check_tables_match: every table has the same columns, declared types and
  rows as the file (ignoring CaseID, the FVS_Cases run time and row order).
- check_one_case_per_stand: each stand gets its own case.
- check_no_db_files: memory mode writes no .db file.
- check_out_unchanged: the .out file is the same in both modes.

Usage:
    python3 tests/APImemTables/memtables_ctypes.py [path/to/FVSie.so]

The library defaults to bin/FVSie.so. Exits non-zero on any failure.
"""

from __future__ import annotations

import ctypes
import logging
import math
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from collections import Counter
from ctypes import byref, c_double, c_int, create_string_buffer
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
KEYDIR = ROOT / "tests" / "FVSie"
KEY = "DBReportTest.key"
IGNORE_COLS = {"CaseID", "RunDateTime"}

log = logging.getLogger("fvs_memtables")

Row = dict[str, float | str]


# ------------------------------------------------------------------- checks


def check_tables_match(file_db: Path, mem_db: Path) -> list[str]:
    """Check that memory mode produces the same tables as the file output.

    Every table must exist in both, with the same columns, the same declared
    types (from fvsTableColumns) and the same rows. CaseID and the FVS_Cases
    run time differ between runs and are ignored, as is row order. NULL is
    compared equal to NaN and to empty text, which is how the API returns it.

    Args:
        file_db: Database FVS wrote with memory mode off.
        mem_db: The tables read through the API in memory mode, as written by
            _write_db.

    Returns:
        One message per difference; empty if the tables match.
    """
    fcon, mcon = sqlite3.connect(file_db), sqlite3.connect(mem_db)
    fnames, mnames = _table_names(fcon), _table_names(mcon)
    errors: list[str] = []
    if fnames != mnames:
        errors.append(
            f"tables differ: only file {sorted(fnames - mnames)}, "
            f"only mem {sorted(mnames - fnames)}"
        )
    for name in sorted(fnames & mnames):
        fcols, mcols = _column_types(fcon, name), _column_types(mcon, name)
        if fcols.keys() != mcols.keys():
            errors.append(
                f"{name}: columns differ {sorted(fcols.keys() ^ mcols.keys())}"
            )
            continue
        if fcols != mcols:
            errors.append(
                f"{name}: declared types differ "
                f"{sorted(set(fcols.items()) ^ set(mcols.items()))}"
            )
            continue
        keep = [c for c in fcols if c not in IGNORE_COLS]
        frows, mrows = _rows(fcon, name, keep), _rows(mcon, name, keep)
        if frows != mrows:
            errors.append(
                f"{name}: rows differ ({sum(frows.values())} file, "
                f"{sum(mrows.values())} mem, "
                f"{sum((frows - mrows).values())} only in file)"
            )
        else:
            log.info(f"ok  {name}: {sum(frows.values())} rows")
    return errors


def check_one_case_per_stand(mem_db: Path) -> list[str]:
    """Check that memory mode gives each stand its own case.

    The in-memory database stays open for the whole run, so cases from all
    stands accumulate in it; each must have a distinct CaseID.

    Args:
        mem_db: The tables read through the API in memory mode.

    Returns:
        A message if the number of distinct CaseIDs in FVS_Cases differs from
        the number of stands in the keyword file; otherwise empty.
    """
    con = sqlite3.connect(mem_db)
    ncases = con.execute('SELECT count(DISTINCT CaseID) FROM "FVS_Cases"').fetchone()[0]
    nstands = sum(1 for line in open(KEYDIR / KEY) if line.strip().upper() == "PROCESS")
    return (
        [] if ncases == nstands else [f"FVS_Cases: {ncases} cases for {nstands} stands"]
    )


def check_no_db_files(mem_dir: Path) -> list[str]:
    """Check that memory mode writes no output database file.

    DBReportTest.key names an output database with DSNOut for every stand;
    in memory mode none of them may be created.

    Args:
        mem_dir: Work directory of the memory-mode run.

    Returns:
        A message naming any .db file other than the input database and
        mem_tables.db; otherwise empty.
    """
    stray = [
        p.name
        for p in mem_dir.glob("*.db")
        if p.name not in ("FVS_Data.db", "mem_tables.db")
    ]
    return [f"memory mode wrote database files: {stray}"] if stray else []


def check_out_unchanged(file_out: Path, mem_out: Path) -> list[str]:
    """Check that memory mode doesn't change the main output (.out) file.

    Args:
        file_out: .out file from the file-mode run.
        mem_out: .out file from the memory-mode run.

    Returns:
        A message if the files differ, ignoring dates and times; otherwise
        empty.
    """
    stamp = re.compile(r"\d{2}[-/:]\d{2}[-/:]\d{2,4}")
    fout, mout = (
        [stamp.sub("", line) for line in open(p, errors="replace")]
        for p in (file_out, mem_out)
    )
    return [] if fout == mout else [".out files differ"]


# ------------------------------------------------------- running FVS (private helpers)


def _run(lib_path: Path, workdir: Path, mode: str) -> None:
    """Run DBReportTest.key in workdir in "file" or "mem" mode.

    In "mem" mode, read and clear every table at each stop, then write the
    rows to mem_tables.db with the declared types from fvsTableColumns.
    """
    lib = ctypes.CDLL(str(lib_path))
    rc = c_int()
    if mode == "mem":
        lib.fvssetmemorytables_(byref(c_int(1)), byref(rc))
        assert rc.value == 0
    cmd = f"--keywordfile={KEY}".encode()
    lib.fvssetcmdline_(cmd, byref(c_int(len(cmd))), byref(rc))
    assert rc.value == 0, f"fvsSetCmdLine rc={rc.value}"
    if mode == "mem":  # after fvsSetCmdLine, which resets the stop points
        lib.fvssetstoppointcodes_(byref(c_int(6)), byref(c_int(-1)))

    tables: dict[str, dict] = {}
    while True:
        lib.fvs_(byref(rc))
        if rc.value != 0:
            break
        if mode == "mem":
            _drain(lib, tables)
    if mode == "mem":
        _drain(lib, tables)
        _write_db(tables, workdir / "mem_tables.db")


def _drain(lib: ctypes.CDLL, tables: dict[str, dict]) -> None:
    """Append every table's column types and rows to tables, then clear it in FVS."""
    for name in _table_list(lib):
        types, rows = _read_table(lib, name)
        entry = tables.setdefault(name, {"types": {}, "rows": []})
        entry["types"].update(types)  # FVS_Compute can gain columns between stands
        entry["rows"] += rows
        rc = c_int()
        lib.fvscleartable_(name.encode(), byref(c_int(len(name))), byref(rc))
        assert rc.value == 0, f"fvsClearTable({name}) rc={rc.value}"


def _table_list(lib: ctypes.CDLL) -> list[str]:
    """Table names from fvsTableList, growing the buffer until it fits."""
    size = 1
    while True:
        buf, nch, nt, rc = create_string_buffer(size), c_int(size), c_int(), c_int()
        lib.fvstablelist_(buf, byref(nch), byref(nt), byref(rc))
        if rc.value != 2:
            assert rc.value == 0, f"fvsTableList rc={rc.value}"
            return _split0(buf, nch.value)
        size = nch.value


def _read_table(lib: ctypes.CDLL, name: str) -> tuple[dict[str, str], list[Row]]:
    """Declared type of each column (numeric columns first) and every row of a table."""
    bname, nch = name.encode(), c_int(len(name))
    nrow, nnum, ntxt, rc = c_int(), c_int(), c_int(), c_int()
    lib.fvstabledims_(
        bname, byref(nch), byref(nrow), byref(nnum), byref(ntxt), byref(rc)
    )
    assert rc.value == 0, f"fvsTableDims({name}) rc={rc.value}"

    sizes = [1, 1, 1, 1]
    while True:
        bufs = [create_string_buffer(s) for s in sizes]
        ns = [c_int(s) for s in sizes]
        lib.fvstablecolumns_(
            bname,
            byref(nch),
            bufs[0],
            byref(ns[0]),
            bufs[1],
            byref(ns[1]),
            bufs[2],
            byref(ns[2]),
            bufs[3],
            byref(ns[3]),
            byref(rc),
        )
        if rc.value != 2:
            break
        sizes = [max(s, n.value) for s, n in zip(sizes, ns)]
    assert rc.value == 0, f"fvsTableColumns({name}) rc={rc.value}"
    numnames, txtnames, numtypes, txttypes = (
        _split0(b, n.value) for b, n in zip(bufs, ns)
    )
    assert len(numnames) == nnum.value and len(txtnames) == ntxt.value
    types = dict(zip(numnames + txtnames, numtypes + txttypes, strict=True))

    rows: list[Row] = []
    n = nrow.value
    if n:
        values = (c_double * max(1, nnum.value * n))()
        nrows = c_int(n)
        lib.fvstablenum_(
            bname,
            byref(nch),
            byref(c_int(1)),
            byref(nrows),
            byref(nnum),
            values,
            byref(rc),
        )
        assert rc.value == 0 and nrows.value == n, f"fvsTableNum({name}) rc={rc.value}"

        size = 1
        while True:
            text, ntextch, nrows = create_string_buffer(size), c_int(size), c_int(n)
            lib.fvstabletxt_(
                bname,
                byref(nch),
                byref(c_int(1)),
                byref(nrows),
                byref(ntxt),
                text,
                byref(ntextch),
                byref(rc),
            )
            if rc.value != 2:
                break
            size = ntextch.value
        assert rc.value == 0 and nrows.value == n, f"fvsTableTxt({name}) rc={rc.value}"
        txt = _split0(text, ntextch.value)

        k, t = nnum.value, ntxt.value
        for i in range(n):
            row = dict(zip(numnames, values[i * k : (i + 1) * k]))
            row.update(zip(txtnames, txt[i * t : (i + 1) * t]))
            rows.append(row)
    return types, rows


def _split0(buf: ctypes.Array[ctypes.c_char], n: int) -> list[str]:
    """Entries of a char(0)-separated buffer of which n characters are used."""
    return [s.decode() for s in buf.raw[:n].split(b"\0")[:-1]]


def _write_db(tables: dict[str, dict], path: Path) -> None:
    """Write tables read through the API to a SQLite file for the parent to compare."""
    con = sqlite3.connect(path)
    for name, t in tables.items():
        cols = list(t["types"])
        coldefs = ",".join(f"{c!r} {t['types'][c]}" for c in cols)
        con.execute(f'CREATE TABLE "{name}" ({coldefs})')
        con.executemany(
            f'INSERT INTO "{name}" VALUES ({",".join("?" * len(cols))})',
            [[r.get(c) for c in cols] for r in t["rows"]],
        )
    con.commit()
    con.close()


# ------------------------------------------------------ comparison (private helpers)


def _table_names(con: sqlite3.Connection) -> set[str]:
    """Names of the tables in a SQLite database."""
    return {
        r[0] for r in con.execute("SELECT name FROM sqlite_master WHERE type='table'")
    }


def _column_types(con: sqlite3.Connection, name: str) -> dict[str, str]:
    """Declared type of each column of a table, upper case, in table order."""
    return {r[1]: r[2].upper() for r in con.execute(f'PRAGMA table_info("{name}")')}


def _rows(
    con: sqlite3.Connection, name: str, cols: list[str]
) -> Counter[tuple[object, ...]]:
    """Multiset of a table's rows over cols, with values normalized by _norm."""
    rows = con.execute(f'SELECT {",".join(f"{c!r}" for c in cols)} FROM "{name}"')
    return Counter(tuple(_norm(v) for v in r) for r in rows)


def _norm(v: object) -> object:
    """None for NULL, NaN and "" (the API returns NULL as NaN or ""), numbers as float."""
    if v is None or (isinstance(v, float) and math.isnan(v)):
        return None
    if isinstance(v, (int, float)):
        return float(v)
    return v or None


# --------------------------------------------------------------------- main


def main() -> int:
    """Run both modes, each in a child process, and run every check.

    With three arguments (library, work directory, mode), runs one mode instead;
    this is how the script calls itself.

    Returns:
        0 if every check passes, 1 otherwise.
    """
    logging.basicConfig(level=logging.INFO, format="{levelname} {message}", style="{")
    if len(sys.argv) == 4:  # child process
        _run(Path(sys.argv[1]).resolve(), Path(sys.argv[2]), sys.argv[3])
        return 0
    lib_path = Path(
        sys.argv[1] if len(sys.argv) > 1 else ROOT / "bin" / "FVSie.so"
    ).resolve()
    tmp = Path(tempfile.mkdtemp(prefix="fvsmem_"))
    for mode in ("file", "mem"):
        d = tmp / mode
        d.mkdir()
        for f in (KEY, "FVS_Data.db"):
            shutil.copy(KEYDIR / f, d)
        subprocess.run(
            [sys.executable, __file__, str(lib_path), str(d), mode], cwd=d, check=True
        )

    file_dir, mem_dir = tmp / "file", tmp / "mem"
    errors = [
        *check_tables_match(file_dir / "DBReportTest.db", mem_dir / "mem_tables.db"),
        *check_one_case_per_stand(mem_dir / "mem_tables.db"),
        *check_no_db_files(mem_dir),
        *check_out_unchanged(
            file_dir / "DBReportTest.out", mem_dir / "DBReportTest.out"
        ),
    ]
    for e in errors:
        log.error(f"FAIL {e}")
    if errors:
        log.error(f"{len(errors)} failure(s); outputs in {tmp}")
        return 1
    log.info("PASS")
    shutil.rmtree(tmp)
    return 0


if __name__ == "__main__":
    sys.exit(main())
