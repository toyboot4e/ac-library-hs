# Revision history for acl-hs

## 1.2.1.0 -- Feb 2025

- Added dynamic segment tree family.
- Added `Extra.Seq.Map`.
- Fixed `Extra.Pool.size`.
- `Handle` is moved from `Extra.Seq` to `Extra.Pool`.

## 1.2.0.0 -- Feb 2025

- Added `AtCoder.Extra.Seq`.
- Tweaked `INLINE` settings for less compile time.
- Breaking changes:
  - `Matrix.diag` now does not take length parameter.
  - `Extra.Math.primitiveRoot` is renamed to `primitiveRoot32`.
  - `Internal.Convolution` functions now use `ST` instead of `PrimMonad`.
  - `SegAct` implementation for `Extra.Monoid.RangeAdd` over `Max` and `Min` were fixed.

## 1.1.1.0 -- Jan 2025

- Added `AtCoder.Extra.Tree.Lct`.
- Added `blockCut`, `blockCutComponents` in `AtCoder.Extra.Graph`.
- Added `popBack_` in `AtCoder.Internal.Buffer`.
- Added `square`, `rank`, `inv`, `invRaw`, `detMod`, `detMint` in `AtCoder.Extra.Matrix`.

## 1.1.0.0 -- Jan 2025

- Removed `RangeSetId` and `RangeAddId` from `AtCoder.Extra.Monoid`.
- Implemented `SegAct` for `RangeSet`, `RangeAdd` and `Max`, `Min`.
- Added `segActWithLength` to `SegAct`.
- Added `build1` to `AtCoder.Internal.Csr`.
- Added a bunch of extra modules.

## 1.0.0.0 -- Dec 2024

- First version.
- Added ACL-compatible modules.
- Added Extra module of `Math` (binary exponentiation) and `Monoid` (`SegAct` instances).

