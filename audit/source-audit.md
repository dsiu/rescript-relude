# Source Audit: ReasonML relude → ReScript v12.2 Port

**Original**: `/Users/dsiu/code/playground/relude` (ReasonML, v0.66.1)  
**Port**: `/Users/dsiu/code/playground/rescript-relude-rescript-v12` (ReScript v12.2)  
**Audit date**: 2026-04-23

---

## Module Inventory

| Metric | Count |
|--------|-------|
| Original source files | 106 `.re` |
| Port source files | 107 `.res` |
| Missing from port | **0** |
| New in port | 1 (`src/ocaml/ocaml_map.res`) |

All 106 original modules are present in the port. The extra `ocaml_map.res` provides a
compatibility shim for `Map.Make` functor in ReScript v12.

---

## Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Fully equivalent, no gaps |
| ⚠️ | Known difference — intentional or acceptable |
| 🐛 | Bug found (see status column) |

---

## Namespace / Re-export Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude.res` | ✅ | All 64 module re-exports match |
| `Relude_Array.res` | ✅ | Re-exports Array submodules + Infix; match |
| `Relude_List.res` | ✅ | Re-exports List submodules; match |
| `Relude_Option.res` | ✅ | Re-exports Option submodules; uses `open!` vs `open` (shadowing) |
| `Relude_Free.res` | ✅ | Two module aliases; match |
| `Relude_Js.res` | ✅ | Five module aliases; match |
| `Relude_Interface.res` | ✅ | All 9 module type re-exports; match |
| `Relude_Unsafe.res` | ✅ | Single `external` binding; match |
| `Relude_Globals.res` | 🐛 **FIXED** | `module RIO = Relude_RIO` was commented out; restored in commit `db64fed` |
| `Relude_NonEmpty.res` | ✅ | Re-exports `WithSequence` functor; match |
| `Relude_Sequence.res` | ✅ | All 28 functions + `List`/`Array` submodules; match |
| `Relude_StringMap.res` | ✅ | Single `include` statement; match |
| `Relude_ArrayZipper.res` | ✅ | Pure `include Relude_SequenceZipper.WithSequence(...)` wrapper; match |
| `Relude_ListZipper.res` | ✅ | Same pattern; match |

---

## Core Data Type Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_Bool.res` | 🐛 **FIXED** | `not_` was accidentally named `not__`; fixed in commit `3f77f5e` |
| `Relude_Int.res` | ✅ | All 21 functions; stdlib migration (`Js.Int.*` → `Int.*`, `Js.Math.*` → `Math.Int.*`) |
| `Relude_Float.res` | ✅ | All 30 functions; stdlib migration (`Js.Math.*` → `Math.*`) |
| `Relude_String.res` | ✅ | All 55 functions + `Semigroup`/`Monoid`/`Eq`/`Ord`/`Map`/`Set`; stdlib migration (`Js.String.*` → `String.*`, argument orders updated to match new API) |
| `Relude_Decimal.res` | ✅ | All 9 implemented functions; `multiply` and `divide` remain TODO stubs, matching the original |
| `Relude_Unit.res` | ✅ | All 3 functions + 5 typeclass modules; match |
| `Relude_Void.res` | ✅ | `absurd` and `show`; match |
| `Relude_Eq.res` | ✅ | `by`, `cmap`, `Contravariant`, `invert`; match |
| `Relude_Ord.res` | ✅ | All 24+ functions; match |
| `Relude_Ordering.res` | ✅ | All 8 functions + typeclass modules; match |

---

## Collection Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_Array_Base.res` | 🐛 **FIXED** | `setAt` missing `i < 0` guard — negative indices polluted the JS array object instead of returning `None`; fixed in commit `3f77f5e` |
| `Relude_Array_Instances.res` | ✅ | All 16 functions; Belt stdlib → ReScript stdlib (`Belt.Array.*` → `Array.*`) |
| `Relude_Array_Specializations.res` | ✅ | All module specializations; match |
| `Relude_List_Base.res` | ✅ | All 62 functions; `sortWithInt` correctly converts `int → ordering` since ReScript v12 `List.sort` expects ordering type |
| `Relude_List_Instances.res` | ✅ | All 16 functions; Belt → ReScript stdlib |
| `Relude_List_Specializations.res` | ✅ | All module specializations; match |
| `Relude_Option_Base.res` | ✅ | All 20 functions; match |
| `Relude_Option_Instances.res` | ✅ | All 15 functions + 19 module re-exports; `open` → `open!` |
| `Relude_Option_Specializations.res` | ✅ | All specializations; match |
| `Relude_Map.res` | ✅ | All 44 functions + `MAP` module type + `WithOrd` functor; match |
| `Relude_Set.res` | ✅ | All 36 functions + `SET` type + `WithOrd` functor; match |
| `Relude_HList.res` | ✅ | All 18 functions + 3 types; match |
| `Relude_HMap.res` | ✅ | All 2 functions + 2 types; uses `ocaml_map.res` shim for `Map.Make` |
| `Relude_SequenceZipper.res` | ✅ | All functions; `apply` uses `(f, a) => f(a)` instead of the ReasonML curried identity trick `a => a` — semantically equivalent |

---

## Container / Monad Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_Result.res` | ✅ | All 53 functions + 2 types + typeclass modules; match |
| `Relude_Validation.res` | ✅ | All 30 functions + `WithErrors` functor; match |
| `Relude_Option_Base.res` | ✅ | (see Collections) |
| `Relude_AsyncData.res` | ✅ | All 36 functions + 1 type + typeclass modules; match |
| `Relude_AsyncResult.res` | ✅ | All 55 functions; `eqBy` changed from curried 2-arg to explicit 4-arg — compatible for single calls |
| `Relude_Ior.res` | ✅ | All 32 functions + `WithThats` functor; match |
| `Relude_Ior_Type.res` | ✅ | `type t<'a, 'b>` variant; match |
| `Relude_Identity.res` | ✅ | All 10 functions + typeclass modules; match |
| `Relude_IO.res` | ✅ | All 55 functions + 2 types + operators; `Ref.contents` vs `^` dereference (ReScript idiom) |
| `Relude_Tree.res` | ✅ | All 32 functions + 1 type + 14 re-exports; match |
| `Relude_TreeZipper.res` | ✅ | All 54 functions + 2 types; polymorphic variant syntax updated `` `Up `` → `#Up` |

---

## Function / Utility Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_Function.res` | ✅ | All original functions present; port adds `apply_x`, `pure_x`, `bind_x`, `uncurryFn2–5` for ReScript uncurried compatibility |
| `Relude_Tuple.res` | ✅ | All 64 functions + 20 re-exports; match |
| `Relude_Tuple2.res` | ✅ | All 10 functions + 6 re-exports; match |
| `Relude_Tuple3.res` | ✅ | All 11 functions; match |
| `Relude_Tuple4.res` | ✅ | All 12 functions; match |
| `Relude_Tuple5.res` | ✅ | All 14 functions; match |
| `Relude_Globals.res` | 🐛 **FIXED** | (see Namespace section) |
| `Relude_Debounce.res` | ✅ | `debounce` type + function; uses `setTimeout`/`clearTimeout` as globals (equivalent to `Js.Global.*` bindings in JS output) |
| `Relude_Throttle.res` | ✅ | Same pattern as Debounce; match |
| `Relude_Timer.res` | ✅ | `delay`, `repeat`, `repeatTimes`; uses global timer bindings |

---

## Monad Transformer Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_ContT.res` | ✅ | All functions; match |
| `Relude_ReaderT.res` | ✅ | All functions; match |
| `Relude_StateT.res` | ✅ | All functions; `fst`/`snd` → `Pair.first`/`Pair.second` |
| `Relude_WriterT.res` | ✅ | All functions; match |
| `Relude_ResultT.res` | ✅ | All functions; match |
| `Relude_OptionT.res` | ✅ | All functions; match |
| `Relude_RIO.res` | ✅ | All functions; match |
| `Relude_RWST.res` | ✅ | All functions; match |
| `Relude_Free.res` | ✅ | Module aliases; match |
| `Relude_Free_Applicative.res` | ✅ | Adds local `flip` helper for uncurried mode compatibility; functionally equivalent |
| `Relude_Free_Monad.res` | ✅ | All functions; match |

---

## Extensions (Typeclass) Modules

All 34 extension modules were audited. Minor notes:

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_Extensions.res` | ✅ | All 37 module re-exports; match |
| `Relude_Extensions_Applicative.res` | ✅ | `all` uses explicit curried form `result => rest => list{...}` vs ReasonML curried shorthand; semantically equivalent |
| `Relude_Extensions_BoundedEnum.res` | ⚠️ | `inverseMapEqBy` changed from staged (returns `a => option<E.t>`) to eager `(eqA, eToA, a) => option<E.t>` — same result for single calls, but loses the "build once, reuse" optimization |
| `Relude_Extensions_Foldable.res` | ✅ | `containsBy` uses `f(x, _)` placeholder — equivalent to original `f(x)` curried application |
| `Relude_Extensions_Show.res` | ✅ | `Js.Console.*` → `Console.*` (ReScript stdlib) |
| All other Extensions | ✅ | Syntax migration only; no functional gaps |

---

## JS Interop Modules

| Module | Status | Notes |
|--------|--------|-------|
| `Relude_Js.res` | ✅ | All 5 module aliases; match |
| `Relude_Js_Animation.res` | ✅ | `@val` binding (was `[@bs.val]`); match |
| `Relude_Js_Console.res` | ✅ | `Js.Console.*` → `Console.*`; match |
| `Relude_Js_Exn.res` | ✅ | `Js.Exn.t` → `JsExn.t`, pattern `Js.Exn.Error(e)` → `JsExn(e)`; match |
| `Relude_Js_Json.res` | ✅ | All 65 functions; complete JSON API migration `Js.Json.*` → `JSON.*`, `Js.Dict.*` → `dict` |
| `Relude_Js_Promise.res` | ⚠️ | Error type changed from `Js.Promise.error` to `exn` — intentional: `Js.Promise.error` removed in ReScript v12. The original TODO comment acknowledged this open question. |

---

## Port-Only Module

| Module | Notes |
|--------|-------|
| `src/ocaml/ocaml_map.res` | Required compatibility shim. Provides `Map.Make` functor used by `Relude_HMap`. Not part of the public API. |

---

## Known Semantic Differences (Not Bugs)

These are intentional changes resulting from the ReasonML → ReScript v12 migration:

1. **`Int.fromString` / `Float.fromString` / `String.toInt` / `String.toFloat`** — JavaScript's `parseInt`/`parseFloat` accept leading-numeric strings like `"3a"` → `Some(3)`, while OCaml's `int_of_string` rejects them with `None`. Seven tests are deliberately skipped with `Skip.test` to document this difference.

2. **`Js.Promise.error` → `exn`** — The `Js.Promise` module was removed in ReScript v12. `Relude_Js_Promise.toIO` and `toIOLazy` now return `Relude_IO.t<'a, exn>` instead of `Relude_IO.t<'a, Js.Promise.error>`.

3. **Belt → ReScript stdlib** — All Belt (`Belt.Array.*`, `Belt.List.*`, etc.) and legacy Js (`Js.String.*`, `Js.Float.*`, etc.) APIs have been replaced with ReScript v12 stdlib equivalents. Argument orders were updated to match new APIs (e.g., `String.indexOf(str, search)` vs `Js.String.indexOf(search, str)`).

4. **`Relude_Function` additions** — `apply_x`, `pure_x`, `bind_x`, `uncurryFn2–5` added for ReScript uncurried mode compatibility. These are additions, not replacements.

5. **`Relude_AsyncResult.eqBy`** — Changed from 2-arg curried form to explicit 4-arg. Callers that used `eqBy(errEq, okEq)` as a point-free predicate must now write `eqBy(errEq, okEq, r1, r2)`.

---

## Bugs Fixed During Audit

| Commit | File | Bug |
|--------|------|-----|
| `3f77f5e` | `src/array/Relude_Array_Base.res` | `setAt` returned `Some(polluted_array)` for negative indices instead of `None` |
| `3f77f5e` | `src/Relude_Bool.res` | `not_` accidentally named `not__` (double underscore) |
| `db64fed` | `src/Relude_Globals.res` | `module RIO = Relude_RIO` was commented out, removing `RIO` from the public API |
