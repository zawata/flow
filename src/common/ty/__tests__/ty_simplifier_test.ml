(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

open Ty
open Ty_utils


module UnionSimplification = struct

  let tests = [
    (*
     * {f: number} | {f: number}
     * ~>
     * {f: number}
     *)
    "simplify_union_obj" >:: begin fun ctxt ->
      let t_in = Ty.Union (
        (Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
            obj_props =
            [(Ty.NamedProp ("f",
                (Ty.Field ((Ty.Num None),
                   { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
                ))
              ]
            }),
        (Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
            obj_props =
            [(Ty.NamedProp ("f",
                (Ty.Field ((Ty.Num None),
                   { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
                ))
              ]
            }),
        []) in
      let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
      let t_exp = Ty.Obj {
        Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
        obj_props = [
          Ty.NamedProp ("f", (Ty.Field ((Ty.Num None),
            { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
          )
        ]
      } in
      assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
    end;

    (*
     * {+f: number} | {-f: number}
     * ~>
     * {+f: number} | {-f: number}
     *)
    "simplify_union_obj" >:: begin fun ctxt ->
      let t_in = Ty.Union (
        (Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
            obj_props =
            [(Ty.NamedProp ("f",
                (Ty.Field ((Ty.Num None),
                   { Ty.fld_polarity = Ty.Positive; fld_optional = false }))
                ))
              ]
            }),
        (Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
            obj_props =
            [(Ty.NamedProp ("f",
                (Ty.Field ((Ty.Num None),
                   { Ty.fld_polarity = Ty.Negative; fld_optional = false }))
                ))
              ]
            }),
        []) in
      let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
      let t_exp = t_in in
      assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
    end;
  ]
end

module BotAndTopSimplification = struct

  let tests = [
    (* When merge_kinds is true, all kinds of `empty` are equivalent, even when
     * nested under a type constructor.
     *
     * {f: empty} | {f: empty'}
     * ~> (merge_kinds:true)
     * {f: empty'}
     *)
    "simplify_union_obj_empty_insensitive" >:: begin fun ctxt ->
      let t_in = Ty.Union (
        (Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
            obj_props =
            [(Ty.NamedProp ("f",
                (Ty.Field ((Ty.Bot Ty.EmptyType),
                   { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
                ))
              ]
            }),
        (Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
            obj_props =
            [(Ty.NamedProp ("f",
                (Ty.Field ((Ty.Bot Ty.EmptyMatchingPropT),
                   { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
                ))
              ]
            }),
        []) in
      let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
      let t_exp = Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Bot Ty.EmptyType),
                 { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
              ))]
        } in
      assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
    end;

    (* This tests the conversion `mixed & T -> T` and that `empty' | T` remains
     * as is when:
     * - `empty'` is not the empty type due to
     *   + an annotation, or
     *   + a tvar with no lower and no upper bounds
     * - merge_kinds is false
     *
     * mixed & (empty' | (mixed & (empty'' | number)))
     * ~> (merge_kinds:false)
     * empty' | empty'' | number
     *)
    "merge_bot_and_any_kinds_sensitive" >:: begin fun ctxt ->
      let t_in = Ty.Inter (Ty.Top,
        Ty.Union (Ty.Bot (Ty.EmptyTypeDestructorTriggerT ALoc.none),
          Ty.Inter (Ty.Top,
             Ty.Union (Ty.Bot (Ty.NoLowerWithUpper
               (Ty.SomeUnknownUpper "blah")
              ), Ty.Num None, []), []),
          []),
        []) in
      let t_out = Ty_utils.simplify_type ~merge_kinds:false ~sort:false t_in in
      let t_exp = Ty.Union (Ty.Bot (Ty.EmptyTypeDestructorTriggerT ALoc.none),
        Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeUnknownUpper "blah")),
        [Ty.Num None]
      ) in
      assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
    end;

    (* This tests the conversion `mixed & T -> T` and `empty' | T -> T` when
     * merge_kinds is true.
     *
     * mixed & (empty' | (mixed & (empty'' | number)))
     * ~>
     * number
     *)
    "merge_bot_and_any_kinds_insensitive" >:: begin fun ctxt ->
      let t_in = Ty.Inter (Ty.Top,
        Ty.Union (Ty.Bot (Ty.EmptyTypeDestructorTriggerT ALoc.none),
          Ty.Inter (Ty.Top,
             Ty.Union (Ty.Bot (Ty.NoLowerWithUpper
               (Ty.SomeUnknownUpper "blah")
              ), Ty.Num None, []), []),
          []),
        []) in
      let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
      let t_exp = Ty.Num None in
      assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
    end;
  ]
end

module Sorting = struct
  let simplify_base = simplify_type ~merge_kinds:false ~sort:false
  let simplify_sort = simplify_type ~merge_kinds:false ~sort:true

  let t0 = Union (Any Explicit, Num None, [NumLit "42"])
  let t1 = Union (NumLit "1", NumLit "2", [NumLit "42"])
  let t2 = Union (NumLit "2", t0, [t1])
  let t3 = Union (t0, t1, [t2])
  let t4 = Union (t3, t2, [t1; t0])
  let t5 = Union (t0, t1, [t2; t3; t4])
  let t6 = Union (t3, t2, [t4; t0; t1; t5])
  let t6_sorted = Union (Any Explicit, NumLit "1", [NumLit "2"; NumLit "42"; Num None])

  let tests = [
    "idempotence" >:: begin fun ctxt ->
      assert_equal ~ctxt ~printer:Ty_printer.string_of_t
        (simplify_base t0) (simplify_base (simplify_base t0));
      assert_equal ~ctxt ~printer:Ty_printer.string_of_t
        (simplify_base t6) (simplify_base (simplify_base (simplify_base t6)));
      assert_equal ~ctxt ~printer:Ty_printer.string_of_t
        (simplify_sort t4) (simplify_sort (simplify_sort t4));
      assert_equal ~ctxt ~printer:Ty_printer.string_of_t
        (simplify_sort t6) (simplify_sort (simplify_sort (simplify_sort t6)));
    end;

    "sorting" >:: begin fun ctxt ->
      assert_equal ~ctxt ~printer:Ty_printer.string_of_t t6_sorted  (simplify_sort t6)
    end;

    "union/intersection" >:: begin fun ctxt ->
      let t_in = Inter (Union (Void, Inter (Void, Any Explicit, [NumLit "1"]),
                                [Inter (NumLit "1", Any Explicit, [Void])]),
                        Union (Inter (Any Explicit, Void, [NumLit "1"]), Void, []), []) in
      let t_out = simplify_sort t_in in
      let t_exp = Union (Void, Inter (Any Explicit, Void,[NumLit "1"]), []) in
      assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
    end;

  ]
end

let tests = "ty_simplifier" >::: (
  UnionSimplification.tests @
  BotAndTopSimplification.tests @
  Sorting.tests
)
