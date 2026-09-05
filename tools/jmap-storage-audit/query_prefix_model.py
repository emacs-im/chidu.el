#!/usr/bin/env python3
"""Model the set-only prefix repair proposed for Email/query pagination.

The checker does not prove server response validity.  It deliberately explores
both immutable-order updates and a broad class of mutable-order responses, and
asserts that every repair accepted by Chidu's boundary-count test has the
correct prefix membership.
"""
from __future__ import annotations

from itertools import combinations, permutations

OLD_IDS = ("a", "b", "c", "d")
NEW_IDS = ("x", "y")


def subsets(xs):
    for n in range(len(xs) + 1):
        yield from combinations(xs, n)


def inserted_lists(survivors, inserted):
    """All lists preserving survivor relative order."""
    if not inserted:
        yield tuple(survivors)
        return
    all_items = tuple(survivors) + tuple(inserted)
    for p in permutations(all_items):
        if tuple(x for x in p if x in survivors) == tuple(survivors):
            yield p


def repair(old, n, new, removed, added):
    tail = old[n - 1]
    if tail not in new:
        return None
    boundary = new.index(tail) + 1
    prefix = set(old[:n])
    prefix.difference_update(removed)
    prefix.update(id_ for id_, index in added if index < boundary)
    # These are the fail-closed checks in the design.
    if len(prefix) != boundary or tail not in prefix:
        return None
    return prefix, boundary


def immutable_cases():
    for old in permutations(OLD_IDS):
        for n in range(1, len(old) + 1):
            for deleted in subsets(old):
                survivors = tuple(x for x in old if x not in deleted)
                for inserted in subsets(NEW_IDS):
                    for new in inserted_lists(survivors, inserted):
                        removed = set(deleted)
                        added = [(x, new.index(x)) for x in inserted]
                        yield old, n, new, removed, added


def broad_mutable_cases():
    """Conservative superset: changed current ids are removed+added.

    We mark every surviving old id as potentially changed.  This is stronger
    than most real responses and exercises arbitrary reorder safely.
    """
    for old in permutations(OLD_IDS):
        for n in range(1, len(old) + 1):
            for deleted in subsets(old):
                survivors = tuple(x for x in old if x not in deleted)
                for inserted in subsets(NEW_IDS):
                    for new in permutations(survivors + tuple(inserted)):
                        changed = set(survivors)
                        removed = set(deleted) | changed
                        added = [(x, new.index(x)) for x in set(inserted) | changed]
                        yield old, n, new, removed, added


def check(cases, label):
    total = accepted = 0
    for old, n, new, removed, added in cases:
        total += 1
        got = repair(old, n, new, removed, added)
        if got is None:
            continue
        accepted += 1
        prefix, boundary = got
        expected = set(new[:boundary])
        assert prefix == expected, (label, old, n, new, removed, added, prefix, expected)
    print(f"{label}_cases={total}")
    print(f"{label}_accepted={accepted}")


def main():
    check(immutable_cases(), "immutable")
    check(broad_mutable_cases(), "mutable")
    print("result=PASS")
    print("note=This validates membership only; it does not implement RFC errata 6603-6605 or prove liveness.")


if __name__ == "__main__":
    main()
