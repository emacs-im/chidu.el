#!/usr/bin/env python3
"""Model two Email/get calls observing independent server states.

Chidu's incremental effect may fetch created ids with the required metadata
profile and updated-only ids with a mutable-only profile.  Those method calls
can observe different JMAP Email states.  This model checks both direct
state-matched closure and replay from changes.newState after either get is
observed ahead.
"""
from __future__ import annotations

from typing import Dict, Iterable

from bootstrap_model import (
    Delta,
    Obj,
    apply_membership_delta,
    deltas,
    histories,
    hydrate,
)


def state_choices(issued: bool, start: int, final: int) -> Iterable[int | None]:
    if not issued:
        return (None,)
    return range(start, final + 1)


def apply_split_gets(
    cache: Dict[str, Obj | None],
    delta: Delta,
    states: list[Dict[str, Obj]],
    created_state: int | None,
    updated_state: int | None,
) -> None:
    created = delta.created - delta.destroyed
    updated_only = delta.updated - delta.created - delta.destroyed
    if created:
        assert created_state is not None
        hydrate(cache, created, states[created_state])
    if updated_only:
        assert updated_state is not None
        hydrate(cache, updated_only, states[updated_state])


def main() -> None:
    histories_checked = 0
    deltas_checked = 0
    direct_closures = 0
    replay_closures = 0

    for events, states in histories(5):
        histories_checked += 1
        final = len(events)
        for start in range(final + 1):
            for end in range(start, final + 1):
                for delta in deltas(states, events, start, end):
                    deltas_checked += 1
                    created_issued = bool(delta.created - delta.destroyed)
                    updated_issued = bool(delta.updated - delta.created - delta.destroyed)
                    for gc in state_choices(created_issued, end, final):
                        for gu in state_choices(updated_issued, end, final):
                            cache: Dict[str, Obj | None] = dict(states[start])
                            apply_membership_delta(cache, delta)
                            apply_split_gets(cache, delta, states, gc, gu)

                            created_matches = gc in (None, end)
                            updated_matches = gu in (None, end)
                            if created_matches and updated_matches:
                                assert cache == states[end], (
                                    "direct",
                                    events,
                                    start,
                                    end,
                                    delta,
                                    gc,
                                    gu,
                                    cache,
                                    states[end],
                                )
                                direct_closures += 1

                            # Continue from changes.newState to a later state.
                            for replay in deltas(states, events, end, final):
                                closed = dict(cache)
                                apply_membership_delta(closed, replay)
                                replay_created = replay.created - replay.destroyed
                                replay_updated = replay.updated - replay.created - replay.destroyed
                                hydrate(closed, replay_created, states[final])
                                hydrate(closed, replay_updated, states[final])
                                assert closed == states[final], (
                                    "replay",
                                    events,
                                    start,
                                    end,
                                    delta,
                                    gc,
                                    gu,
                                    replay,
                                    closed,
                                    states[final],
                                )
                                replay_closures += 1

    print(f"histories={histories_checked}")
    print(f"deltas={deltas_checked}")
    print(f"direct_closures={direct_closures}")
    print(f"replay_closures={replay_closures}")
    print("result=PASS")


if __name__ == "__main__":
    main()
