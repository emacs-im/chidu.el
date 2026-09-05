#!/usr/bin/env python3
"""Exhaustive lifecycle model for RFC 8620 intermediate /changes chunks.

Servers may return recent changes before older changes using composite state
strings.  RFC 8620 constrains per-record lifecycle order, not global chronology.
This model enumerates valid per-id report sequences, all interleavings, and all
adjacent response groupings.  It checks that Chidu's normalization
(destroyed wins; created/updated fetch current value; notFound removes) always
converges to final membership.
"""
from __future__ import annotations

from itertools import product
from typing import Dict, Iterator, List, Sequence, Tuple

IDS = ("a", "b")
Action = frozenset[str]  # C, U, D within one response for one id
Unit = Tuple[str, int, Action]


def variants(initial: bool, final: bool) -> Tuple[Tuple[Action, ...], ...]:
    if not initial and not final:
        return (
            (),  # created+destroyed omitted entirely
            (frozenset("CD"),),
            (frozenset("C"), frozenset("D")),
            (frozenset("CU"), frozenset("D")),
        )
    if not initial and final:
        return (
            (frozenset("C"),),
            (frozenset("CU"),),
            (frozenset("C"), frozenset("U")),
        )
    if initial and not final:
        return (
            (frozenset("D"),),
            (frozenset("UD"),),
            (frozenset("U"), frozenset("D")),
        )
    return ((), (frozenset("U"),))


def interleavings(seqs: Dict[str, Sequence[Action]]) -> Iterator[Tuple[Unit, ...]]:
    positions = {id_: 0 for id_ in IDS}
    out: List[Unit] = []

    def rec() -> Iterator[Tuple[Unit, ...]]:
        if all(positions[i] == len(seqs[i]) for i in IDS):
            yield tuple(out)
            return
        for id_ in IDS:
            pos = positions[id_]
            if pos >= len(seqs[id_]):
                continue
            positions[id_] += 1
            unit = (id_, pos, seqs[id_][pos])
            out.append(unit)
            yield from rec()
            out.pop()
            positions[id_] -= 1

    yield from rec()


def groupings(units: Sequence[Unit]) -> Iterator[Tuple[Tuple[Unit, ...], ...]]:
    if not units:
        yield ()
        return
    # Bit 1 after position i means start a new response.
    for cuts in range(1 << (len(units) - 1)):
        groups: List[List[Unit]] = [[units[0]]]
        for i, unit in enumerate(units[1:]):
            if cuts & (1 << i):
                groups.append([unit])
            else:
                groups[-1].append(unit)
        yield tuple(tuple(g) for g in groups)


def apply(initial: Dict[str, bool], final: Dict[str, bool], responses) -> Dict[str, bool]:
    cache = dict(initial)
    for response in responses:
        actions: Dict[str, set[str]] = {}
        for id_, _index, action in response:
            actions.setdefault(id_, set()).update(action)
        for id_, action in actions.items():
            if "D" in action:
                cache[id_] = False
            elif "C" in action or "U" in action:
                # Email/get observes the current server state; if the object has
                # already disappeared it is returned in notFound and removed.
                cache[id_] = final[id_]
    return cache


def main() -> None:
    lifecycle_choices = 0
    linearizations = 0
    response_sequences = 0
    for init_bits in product((False, True), repeat=len(IDS)):
        initial = dict(zip(IDS, init_bits))
        for final_bits in product((False, True), repeat=len(IDS)):
            final = dict(zip(IDS, final_bits))
            per_id = [variants(initial[i], final[i]) for i in IDS]
            for chosen in product(*per_id):
                lifecycle_choices += 1
                seqs = dict(zip(IDS, chosen))
                for units in interleavings(seqs):
                    linearizations += 1
                    for responses in groupings(units):
                        response_sequences += 1
                        got = apply(initial, final, responses)
                        assert got == final, (initial, final, seqs, responses, got)

    print(f"lifecycle_choices={lifecycle_choices}")
    print(f"linearizations={linearizations}")
    print(f"response_sequences={response_sequences}")
    print("result=PASS")


if __name__ == "__main__":
    main()
