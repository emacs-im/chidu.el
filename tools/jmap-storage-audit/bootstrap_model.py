#!/usr/bin/env python3
"""Exhaustive small-state model for Chidu's JMAP Email bootstrap.

This is not a model of a particular server implementation.  It checks the
membership/value convergence argument against the response latitude allowed by
RFC 8620 /changes, including overlapping arrays and omitted create+destroy.
"""
from __future__ import annotations

from dataclasses import dataclass
from itertools import product
from typing import Dict, FrozenSet, Iterable, Iterator, List, Mapping, Sequence, Tuple

IDS = ("a", "b")


@dataclass(frozen=True)
class Obj:
    value: int


@dataclass(frozen=True)
class Event:
    id: str
    op: str  # create | update | destroy


State = Mapping[str, Obj]


@dataclass(frozen=True)
class Delta:
    created: FrozenSet[str] = frozenset()
    updated: FrozenSet[str] = frozenset()
    destroyed: FrozenSet[str] = frozenset()


def apply_event(state: State, event: Event) -> Dict[str, Obj] | None:
    out = dict(state)
    if event.op == "create":
        if event.id in out:
            return None
        # Never reuse an id after destroy in generated histories; caller checks.
        out[event.id] = Obj(0)
    elif event.op == "update":
        if event.id not in out:
            return None
        out[event.id] = Obj(out[event.id].value + 1)
    elif event.op == "destroy":
        if event.id not in out:
            return None
        del out[event.id]
    else:
        raise AssertionError(event)
    return out


def histories(max_events: int = 5) -> Iterator[Tuple[List[Event], List[Dict[str, Obj]]]]:
    """Generate valid histories without id reuse after destruction."""
    initial_states = [
        {},
        {"a": Obj(0)},
        {"b": Obj(0)},
        {"a": Obj(0), "b": Obj(0)},
    ]
    choices = [Event(i, op) for i in IDS for op in ("create", "update", "destroy")]

    def rec(events: List[Event], states: List[Dict[str, Obj]], dead: FrozenSet[str]) -> Iterator:
        yield events, states
        if len(events) == max_events:
            return
        for event in choices:
            if event.op == "create" and event.id in dead:
                continue
            nxt = apply_event(states[-1], event)
            if nxt is None:
                continue
            yield from rec(
                events + [event],
                states + [nxt],
                dead | ({event.id} if event.op == "destroy" else set()),
            )

    for initial in initial_states:
        yield from rec([], [initial], frozenset())


def allowed_classifications(
    states: Sequence[State], events: Sequence[Event], start: int, end: int, id_: str
) -> List[Tuple[bool, bool, bool]]:
    """Return allowed (created, updated, destroyed) memberships for one id.

    We deliberately include the overlap latitude from RFC 8620 section 5.2.
    """
    before = id_ in states[start]
    after = id_ in states[end]
    ops = [e.op for e in events[start:end] if e.id == id_]
    created = "create" in ops
    updated = "update" in ops
    destroyed = "destroy" in ops

    if not ops:
        return [(False, False, False)]
    if created and destroyed:
        # RFC: SHOULD omit entirely; MAY destroyed-only or both.
        return [
            (False, False, False),
            (False, False, True),
            (True, False, True),
            # Updates may also be reported; accepting them must not alter precedence.
            (True, updated, True),
        ]
    if created:
        # created+updated SHOULD be created-only but MAY also be updated.
        return [(True, False, False), (True, updated, False)]
    if destroyed:
        # updated+destroyed SHOULD be destroyed-only but MAY also be updated.
        return [(False, False, True), (False, updated, True)]
    if updated and before and after:
        return [(False, True, False)]
    raise AssertionError((states, events, start, end, id_, ops))


def deltas(states: Sequence[State], events: Sequence[Event], start: int, end: int) -> Iterator[Delta]:
    per_id = [allowed_classifications(states, events, start, end, i) for i in IDS]
    for choices in product(*per_id):
        created = frozenset(i for i, c in zip(IDS, choices) if c[0])
        updated = frozenset(i for i, c in zip(IDS, choices) if c[1])
        destroyed = frozenset(i for i, c in zip(IDS, choices) if c[2])
        yield Delta(created, updated, destroyed)


def apply_membership_delta(cache: Dict[str, Obj | None], delta: Delta) -> None:
    """Destroyed wins, then created/updated are candidates for get."""
    for id_ in delta.destroyed:
        cache.pop(id_, None)
    for id_ in delta.created | delta.updated:
        if id_ not in delta.destroyed:
            cache.setdefault(id_, None)


def hydrate(cache: Dict[str, Obj | None], ids: Iterable[str], observed: State) -> None:
    """Apply Email/get(list/notFound) at OBSERVED state."""
    for id_ in set(ids):
        if id_ in observed:
            cache[id_] = observed[id_]
        else:
            cache.pop(id_, None)


def run_case(
    events: Sequence[Event],
    states: Sequence[State],
    q: int,
    c1: int,
    g1: int,
    c2: int,
    g2: int,
) -> Tuple[int, int]:
    """Check bootstrap convergence with the real closure condition.

    q: stable query snapshot after e0 (e0 is state index 0)
    c1: first membership-catch-up endpoint
    g1: state observed by the coverage-aware hydration
    c2: next /changes endpoint
    g2: state observed by the changed-id Email/get calls (may be ahead)

    A round may close immediately when every get observes c2.  If a get is
    ahead, replaying changes from c2 to a later state and fetching changed ids
    at that same later state must close the cache there; no extra empty round
    is required.
    """
    final = len(events)
    direct_closures = 0
    replay_closures = 0
    for d1 in deltas(states, events, 0, c1):
        cache: Dict[str, Obj | None] = {id_: None for id_ in states[q]}
        apply_membership_delta(cache, d1)
        # Every member receives current list/notFound coverage; this removes
        # query ghosts omitted by created+destroyed coalescing.
        hydrate(cache, list(cache), states[g1])

        for d2 in deltas(states, events, c1, c2):
            cache2 = dict(cache)
            apply_membership_delta(cache2, d2)
            hydrate(cache2, d2.created | d2.updated, states[g2])

            if g2 == c2:
                assert cache2 == states[c2], (
                    "direct-closure",
                    events,
                    states,
                    (q, c1, g1, c2, g2),
                    d1,
                    d2,
                    cache2,
                    states[c2],
                )
                direct_closures += 1

            # Model the mandatory continuation when Email/get observed a state
            # later than changes.newState.  The next changes response may itself
            # be nonempty; it closes as soon as its gets all observe newState.
            for df in deltas(states, events, c2, final):
                cache3 = dict(cache2)
                apply_membership_delta(cache3, df)
                hydrate(cache3, df.created | df.updated, states[final])
                assert cache3 == states[final], (
                    "replay-closure",
                    events,
                    states,
                    (q, c1, g1, c2, g2, final),
                    d1,
                    d2,
                    df,
                    cache3,
                    states[final],
                )
                replay_closures += 1
    return direct_closures, replay_closures


def main() -> None:
    histories_checked = 0
    scenarios_checked = 0
    direct_closures_checked = 0
    replay_closures_checked = 0
    for events, states in histories(5):
        n = len(events)
        histories_checked += 1
        # e0 is states[0]. Query happens after it.  Preserve temporal order.
        for q in range(0, n + 1):
            for c1 in range(q, n + 1):
                for g1 in range(c1, n + 1):
                    for c2 in range(c1, n + 1):
                        for g2 in range(c2, n + 1):
                            # If c2 precedes full hydration observation, the real
                            # implementation would serialize phases; exclude that.
                            if c2 < g1:
                                continue
                            scenarios_checked += 1
                            direct, replay = run_case(events, states, q, c1, g1, c2, g2)
                            direct_closures_checked += direct
                            replay_closures_checked += replay

    print(f"histories={histories_checked}")
    print(f"timelines={scenarios_checked}")
    print(f"direct_closures={direct_closures_checked}")
    print(f"replay_closures={replay_closures_checked}")
    print("result=PASS")


if __name__ == "__main__":
    main()
