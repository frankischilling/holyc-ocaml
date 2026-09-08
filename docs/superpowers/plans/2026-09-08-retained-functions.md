# Retained task functions

This implements the retained-function portion of the stateful `#exe` design in
`../specs/2026-09-08-stateful-exe-design.md`. The full `#exe` task remains open.

## Constraints

Reuse the checked direct-call binder, conversions, and complete graph seals.
Keep current-module declaration ownership exact. Prior function metadata must
retain the selected declaration and classification snapshot. Executable links
are opaque task capabilities; spelling or symbol IDs cannot grant authority.
Each old body retains its original callees, globals, statics, and mutable literal
image. Commands never reparse or reexecute earlier source. Failed preflight
cannot publish a body. Runtime faults preserve reached task effects.

## Tasks

1. Extend outer function metadata, body and top-level direct-call resolution,
   and target classification with explicit selected outer binding evidence.
2. Publish metadata and opaque function links in task catalog snapshots. Seal
   retained links into runtime calls through the exact selected task view.
3. Retain prepared function owners in VM task state, switching owner context
   across calls and returns without rebuilding literals or reseeding storage.
4. Verify old calls from new entries and functions, original global/static and
   literal state, nested owner switches, pending selection, and rejected calls.
   Run full regression checks and independent code review before a checkpoint.

The four new RED groups in `test/test_integer_task.ml` establish the initial
missing behavior. Root serializes all builds and tests; implementers edit only
their assigned files. Existing branch and push authorization persist.

## Implementation evidence

The initial four retained-call groups changed from RED to GREEN with 29 task
groups passing. Extended coverage exposed a current-command-only initializer
guard; exact admitted source lookup now lets the same guards inspect retained
bodies under their original storage and frame context. Pointer returns remain
outside the existing integer function domain; retained pointer-local coverage
checks the supported object-reference behavior.

The final focused run passes 43 task groups and 11 function-classification groups,
including three metadata ownership controls. Full regression evidence is recorded
with the resulting checkpoint. All seven full #635 acceptance items remain open.
